{
XpresElementsPIC
================
Definitions and implementation for the AST (Abstract Syntax Tree) structure.
This unit is based in the unit XpresElements from the framework Xpres, and is adapted
to the 6502 CPU architecture and to the Pascal dialect used here.

                                                       By Tito Hinostroza.
}
unit AstTree;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, LazLogger, AstElemP65, LexPas, CompGlobals;
type  //Abstract Syntax Tree
  {Represent the state of a search with FindFirst-FindNext}
  TAstFindState = object
    Name  : string;
    Node  : TAstElement;
    Idx   : integer;
    inUnit: boolean;
  end;

  { TAstTree }
  {Árbol de sintaxis abstracta. Aquí es donde se guardará la referencia a todas los
  elementos sintácticos creados (variables, constantes, ..).
  Se usa también como para resolver nombres de elementos.}
  TAstTree = class
  public  //Tree definition/events
    main      : TAstProg;  //Root node
    curNode   : TAstElement;  //Reference to current node
    curCodCont: TAstCodeCont;  {Reference to current code container, used to solve
                                 identifiers. It could be:
                                   - Body opened.
                                   - Constant declaration opened.
                                   - Variable declaration opened.
                                   - Type declaration opened.
                                  Note this don't consider TAstBlock elements.
                                   }
    OnFindElement: procedure(elem: TAstElement) of object;  //Evento de búsqueda
  public  //Containers
    AllCons  : TAstConsDecs;
    AllVars  : TAstVarDecs;
    AllUnits : TAstUnits;
    AllFuncs : TAstFunDecs;
    AllTypes : TAstTypeDecs;
    procedure Clear;
    procedure RefreshAllUnits;
  public  //Filling the tree
    procedure AddElement(elem: TAstElement; position: integer = - 1);
    procedure AddElementAndOpen(elem: TAstElement; position: integer = - 1);
    procedure AddElementToParent(elem: TAstElement; AtBegin: boolean);
    procedure OpenElement(elem: TAstElement);
    procedure CloseElement;
    procedure DeleteTypeNode(typNode: TAstTypeDec);
    procedure ChangeParentTo(newparent, elem: TAstElement; position: integer = - 1);
    procedure InsertParentTo(newparent, elem: TAstElement);
    function AddBodyAndOpen(srcPos: TSrcPos): TAstBody;
    function AddConsDecAndOpen(srcPos: TSrcPos; cname: string;
      ctype: TAstTypeDec): TAstConsDec;
    function AddVarDecAndOpen(srcPos: TSrcPos; vname: string;
      vtype: TAstTypeDec): TAstVarDec;
    function AddTypeDecAndOpen(srcPos: TSrcPos; tname: string; tsize: word;
      catType: TCatType; group: TTypeGroup; position: integer = - 1
  ): TAstTypeDec;
    function AddElementBlockAndOpen(srcPos: TSrcPos; position: integer = - 1
      ): TAstBlock;
    function AddElementSentAndOpen(srcPos: TSrcPos; sntType: TSentenceType): TAstSentence;
  public  //Element resolution (FindFirst() - FindNext())
    curFind: TAstFindState; //State variables for searching
    function FindFirst(const name: string): TAstElement;
    function FindNext: TAstElement;
    function FindNextFuncName: TAstFunDec;
    function FindFirstType(const name: string): TAstTypeDec;
    function FindNextType: TAstTypeDec;
    function FindVar(varName: string): TAstVarDec;
    function FindType(typName: string): TAstTypeDec;
  public  //Searching/Identify
    function LastNode: TAstElement;
    function BodyNode: TAstBody;
    function CurNodeName: string;
    function ExistsArrayType(itemType: TAstTypeDec; nEle: integer;
                             out typFound: TAstTypeDec): boolean;
    function ExistsPointerType(ptrType: TAstTypeDec;
                             out typFound: TAstTypeDec): boolean;
    function GetElementBodyAt(posXY: TPoint): TAstBody;
    function GetElementAt(posXY: TPoint): TAstElement;
    function GetElementCalledAt(const srcPos: TSrcPos): TAstElement;
    function GetELementDeclaredAt(const srcPos: TSrcPos): TAstElement;
    function FunctionExistInCur(funName: string; const pars: TAstParamArray
      ): boolean;
  public  //Debug
    procedure print();  //Show the AST
  public  //Constructor and destructror
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  function SameParamsType(f: TAstFunBase; const funpars: TAstParamArray): boolean;
  function SameParamsName(f: TAstFunBase; const funpars: TAstParamArray): boolean;
var
  // Tipo nulo. Usado para elementos sin tipo.
  typNull : TAstTypeDec;

implementation
function SameParamsType(f: TAstFunBase; const funpars: TAstParamArray): boolean;
{Compara los parámetros de la función con una lista de parámetros. Si tienen el mismo
número de parámetros y el mismo tipo, devuelve TRUE.}
var
  i, npars: Integer;
begin
  npars := length(f.pars);
  if (npars=0) and (funpars=nil) then exit(true);  //No tienen parámetros
  if High(f.pars) <> High(funpars) then
    exit(false);   //Distinct parameters number
  //They have the same numbers of parameters, verify:
  for i := 0 to High(f.pars) do begin
    //A Null type matches everything (wildcard). Used in INLINE functions.
    if f.pars[i].typ = typNull then continue;
    //Compare tipe
    if f.pars[i].typ <> funpars[i].typ then begin
      exit(false);
    end;
  end;
  //Si llegó hasta aquí; hay coincidencia, sale con TRUE
  exit(true);
end;
function SameParamsName(f: TAstFunBase; const funpars: TAstParamArray): boolean;
{Compara los parámetros de la función con una lista de parámetros. Si tienen el mismo
nombre, devuelve TRUE. No se hace verificación de tipo o cantidad de parámetros. Esa
verifiación debe haberse hecho previamente con SameParamsType().}
var
  i: Integer;
begin
  for i := 0 to High(f.pars) do begin
    if UpCase(f.pars[i].name) <> UpCase(funpars[i].name) then begin
      exit(false);
    end;
  end;
  //Si llegó hasta aquí; hay coincidencia, sale con TRUE
  exit(true);
end;

{ TAstTree }
procedure TAstTree.Clear;
begin
  main.elements.Clear;  //esto debe hacer un borrado recursivo
  main.Clear;

  curNode := main;      //retorna al nodo principal
  curCodCont := nil;    //Important
  //ELimina lista internas
  AllCons.Clear;
  AllVars.Clear;
  AllUnits.Clear;
  AllFuncs.Clear;
  AllTypes.Clear;
end;
procedure TAstTree.RefreshAllUnits;
var
  ele : TAstElement;
begin
  AllUnits.Clear;   //por si estaba llena
  for ele in main.elements do begin
    if ele.idClass = eleUnit then begin
       AllUnits.Add( TAstUnit(ele) );
    end;
  end;
end;
//Filling the tree
procedure TAstTree.AddElement(elem: TAstElement; position: integer = -1);
{Add a new element to the current node. Commonly elements are added at the end of the
list unless "position" is specified.
This is the unique entry point to add elements to the Syntax Tree.}
begin
  //Add the node
  if position<>-1 then curNode.AddElement(elem, position)
  else curNode.AddElement(elem);
//  if OnAddElement<>nil then OnAddElement(elem);
  //Update Lists
  case elem.idClass of
  eleConsDec : AllCons.Add(TAstConsDec(elem));
  eleVarDec  : begin
//debugln('<adding>' + elem.Parent.name + ',' + elem.name);
    AllVars.Add(TAstVarDec(elem));
    end;
  eleFuncDec : AllFuncs.Add(TAstFunDec(elem)); //Declarations are now stored in AllFuncs.
  eleTypeDec : AllTypes.Add(TAstTypeDec(elem));
  //No se incluye el código de RefreshAllUnits() porque solo trabaja en el "main".
  end;
end;
procedure TAstTree.AddElementAndOpen(elem: TAstElement; position: integer = -1);
{Add an element and change the current node to this new node. Open an element means
that all new nodes added, will be children of this node (The current node).
To open an element is useful when it will contain other nodes, like a function body.}
begin
  {Las funciones o procedimientos no se validan inicialmente, sino hasta que
  tengan todos sus parámetros agregados, porque pueden ser sobrecargados.}
  AddElement(elem, position);
  //Genera otro espacio de nombres
  curNode := elem;  //Set new Current node.
end;
procedure TAstTree.AddElementToParent(elem: TAstElement; AtBegin: boolean);
{Add element to the parent of the current element.}
var
  tmp: TAstElement;
begin
  tmp := curNode;  //Save currente node
  curNode := curNode.Parent;  //Set to parent
  if AtBegin then AddElement(elem, 0) else AddElement(elem);
  curNode := tmp;  //Restore position
end;
procedure TAstTree.OpenElement(elem: TAstElement);
{Accede al espacio de nombres del elemento indicado.}
begin
  curNode := elem;  //empieza a trabajar en esta lista
end;
procedure TAstTree.CloseElement;
{Close the current node and returns to the parent node.}
var
  isCodeContainer: Boolean;
begin
  isCodeContainer := (curNode=curCodCont);
  if curNode.Parent<>nil then begin
    curNode := curNode.Parent;
  end;
  if isCodeContainer then begin //We are closing a Code container
    curCodCont := curNode.codCont;  //Restore last value
  end;
end;
procedure TAstTree.DeleteTypeNode(typNode: TAstTypeDec);
{Delete a node of type "TAstTypeDec"}
var
  parent: TAstElement;
begin
  parent := typNode.Parent;
  //parent.elements.Remove(typNode);
  parent.elements.Extract(typNode);  //Doesn't free "ele". No need to update lists (AllCons, AllVars, ...) because it will be reinserted.
  AllTypes.Extract(typNode);
  typNode.Destroy;
  { WARNING: This procedure is incomplete. It doesn't delete the possible "callers"
   existing in "typNode".}
end;
procedure TAstTree.ChangeParentTo(newparent, elem: TAstElement; position: integer = -1);
{Change the current parent of "elem". The element "elem" is reinserted in the
new parent "newparent" at the position "position".}
var
  parent: TAstElement;
begin
  parent := elem.Parent;
  parent.elements.Extract(elem);  //Doesn't free "ele". No need to update lists (AllCons, AllVars, ...) because it will be reinserted.
  newparent.AddElement(elem, position);  //Reinsert here
end;
procedure TAstTree.InsertParentTo(newparent, elem: TAstElement);
{Set "newparent" as parent to the element "elem" making it descend one level.
Doesn't change current node.}
var
  i: Integer;
  parent, tmp: TAstElement;
begin
  parent := elem.Parent;
  i := parent.elements.IndexOf(elem);  //Position of "elem"
  parent.elements.Extract(elem);  //Doesn't free "elem". No need to update lists (AllCons, AllVars, ...) because it will be reinserted.

  //Insert in "newparent" in the same position of "elem".
//  parent.AddElement(newparent, i);
  tmp := curNode;  //Save currente node
  curNode := parent;  //Set to parent
  AddElement(newparent, i);  //Use AddElement() to mantain the unique point for Adding nodes.
  curNode := tmp;  //Restore position

  //Reinsert "elem" as child of newparent
  newparent.AddElement(elem, 0);
end;
function TAstTree.AddBodyAndOpen(srcPos: TSrcPos): TAstBody;
{Similar to AddElementAndOpen() but create and open a Body node. Returns the Body created.
This function must be used always when creating a Body, because it mantains updated the
variable "curBody" that is used to resolve names.}
begin
  Result := TAstBody.Create;
  Result.name := TIT_BODY_ELE;
  Result.srcDec := srcPos;
  curNode.codCont := curCodCont;  //Save before change
  //For functions, updates "bodyImplem" on declaration
  if curNode.idClass = eleFuncDec then begin
    TAstFunDec(curNode).bodyImplem := Result;
  end else if curNode.idClass = eleFuncImp then begin
    TAstFunImp(curNode).declar.bodyImplem := Result;   //Point to implementation
  end;
  AddElementAndOpen(Result);
  curCodCont := Result;  //Update current Code container
end;
function TAstTree.AddConsDecAndOpen(srcPos: TSrcPos; cname: string;
                         ctype: TAstTypeDec): TAstConsDec;
begin
  Result := TAstConsDec.Create;
  Result.name   := cname;
  Result.typ    := ctype;   //Set reference to type.
  Result.srcDec := srcPos;
  curNode.codCont := curCodCont;  //Save before change
  AddElementAndOpen(Result);
  curCodCont := Result;  //Update current Code container
end;
function TAstTree.AddVarDecAndOpen(srcPos: TSrcPos; vname: string;
                         vtype: TAstTypeDec): TAstVarDec;
begin
  Result := TAstVarDec.Create;
  Result.name   :=vname;
  Result.typ    := vtype;   //fija  referencia a tipo
  Result.srcDec := srcPos;
  curNode.codCont := curCodCont;  //Save before change
  AddElementAndOpen(Result);
  curCodCont := Result;  //Update current Code container
end;
function TAstTree.AddTypeDecAndOpen(srcPos: TSrcPos; tname: string;
  tsize: word; catType: TCatType; group: TTypeGroup;
  position: integer = -1): TAstTypeDec;
begin
  Result := TAstTypeDec.Create;
  Result.name    := tname;
  Result.srcDec  := srcPos;
  Result.size    := tsize;
  Result.catType := catType;
  Result.group   := group;
  curNode.codCont := curCodCont;  //Save before change
  AddElementAndOpen(Result, position);  //Open type
  curCodCont := Result;  //Update current Code container
end;
function TAstTree.AddElementBlockAndOpen(srcPos: TSrcPos; position: integer = -1): TAstBlock;
begin
  Result := TAstBlock.Create;
  Result.name := 'block';
  Result.srcDec := srcPos;
  AddElementAndOpen(Result, position);
end;
function TAstTree.AddElementSentAndOpen(srcPos: TSrcPos; sntType: TSentenceType): TAstSentence;
begin
  Result := TAstSentence.Create;
  //Result.name := 'sent';
  Result.srcDec := srcPos;
  Result.sntType := sntType;
  AddElementAndOpen(Result);
end;
//Element resolution
function TAstTree.FindFirst(const name: string): TAstElement;
{Routine to resolve an identifier inside the SyntaxTree, following the scope rules for
identifiers of the Pascal syntax (first the current space and then the parents spaces).
If found returns the reference to the element otherwise returns NIL.
If "name" is empty string, all the elements, of the Syntax Tree, will be scanned.}
begin
  //Busca recursivamente, a partir del espacio actual
  curFind.Name := UpCase(name);  //This value won't change in all the search
  curFind.inUnit := false;       //Set flag
  if curCodCont=nil then exit(nil);  //This shouldn't happen
  if curCodCont.idClass = eleBody then begin
    {Para los cuerpos de procedimientos o de programa, se debe explorar hacia atrás a
    partir de la posición del nodo actual.}
    curFind.Idx := curCodCont.Index;   //Set index for searching. Here is the body index.
    curFind.Node := curCodCont.Parent; //Set the parent node as the node to search.
    Result := FindNext;             //Start search
  end else begin
    {La otras forma de resolución, debe ser:
    1. Declaración de constantes, cuando se definen como expresión con otras constantes
    2. Declaración de variables, cuando se definen como ABSOLUTE <variable>
    3. Declaración de tipos, cuando se refiere a otros tipos o cuando se define como objeto.
    }
    curFind.Node := curNode;  //Actualiza nodo actual de búsqueda
    {Formalmente debería apuntar a la posición del elemento actual, pero se deja
    apuntando a la posición final, sin peligro, porque, la resolución de nombres para
    constantes y variables, se hace solo en la primera pasada (con el árbol de sintaxis
    llenándose.)}
    curFind.Idx := curNode.elements.Count;
    //Busca
    Result := FindNext;
  end;
end;
function TAstTree.FindNext: TAstElement;
{Realiza una búsqueda recursiva en el nodo "curFindNode", a partir de la posición,
"curFindIdx", hacia "atrás", el elemento con nombre "curFindName". También implementa
la búsqueda en unidades.
Esta rutina es quien define la resolución de nombres (alcance) en el lenguaje.}
var
  elem: TAstElement;
begin
  //debugln(' Explorando nivel: [%s] desde pos: %d de %s', [curFind.name, curFind.Idx - 1, curFind.Node.name]);
  repeat
    curFind.Idx := curFind.Idx - 1;  //Siempre salta a la posición anterior
    if curFind.Idx<0 then begin
      //No encontró, en ese nivel. Hay que ir más atrás. Pero esto se resuelve aquí.
      if curFind.Node.Parent = nil then begin
        //No hay nodo padre. Este es el nodo Main
        Result := nil;
        exit;  //aquí termina la búsqueda
      end;
      //Busca en el espacio padre
      curFind.Idx := curFind.Node.Index;   //Posición actual
      curFind.Node := curFind.Node.Parent; //Apunta al padre
      if curFind.inUnit then curFind.inUnit := false;   //Sale de una unidad
      Result := FindNext();  //Recursividad IMPORTANTE: Usar paréntesis.
//      Result := nil;
      exit;
    end;
    //Verifica ahora este elemento
    elem := curFind.Node.elements[curFind.Idx];
    if curFind.inUnit and (elem.location = locImplement) then begin
      //No debería ser accesible
      continue;
    end;
    //Genera evento para indicar que está buscando.
    if OnFindElement<>nil then OnFindElement(elem);
    //Compara
    if (curFind.Name = '') or (elem.uname = curFind.Name) then begin
      //Encontró en "findSt.curFindIdx"
      Result := elem;
      //La siguiente búsqueda empezará en "findSt.curFindIdx-1".
      exit;
    end else begin
      //No tiene el mismo nombre, a lo mejor es una unidad
      if (elem.idClass = eleUnit) and not curFind.inUnit then begin   //Si es el priemr nodo de unidad
        //¡Diablos es una unidad! Ahora tenemos que implementar la búsqueda.
        curFind.inUnit := true;   //Marca, para que solo busque en un nivel
        curFind.Idx := elem.elements.Count;  //para que busque desde el último
        curFind.Node := elem;  //apunta a la unidad
        Result := FindNext();  //Recursividad IMPORTANTE: Usar paréntesis.
        if Result <> nil then begin  //¿Ya encontró?
          exit;  //Sí. No hay más que hacer aquí
        end;
        //No encontró. Hay que seguir buscando
      end;
    end;
  until false;
end;
function TAstTree.FindNextFuncName: TAstFunDec;
{Scans recursively toward root, in the syntax tree, until find a function element with
the same name provided in a previous call to FindFirst.
Must be called after calling FindFirst() with the name of the function.
If not found, returns NIL.}
var
  ele: TAstElement;
begin
  repeat
    ele := FindNext;
  until (ele=nil) or (ele.idClass in [eleFuncImp, eleFuncDec]);
  //Puede que haya encontrado la función o no
  if ele = nil then exit(nil);  //No encontró
  if ele.idClass = eleFuncDec then begin
    Result := TAstFunDec(ele);   //devuelve como función
  end else begin //Implementation
    Result := TAstFunImp(ele).declar;   //devuelve como función
  end;
end;
function TAstTree.FindFirstType(const name: string): TAstTypeDec;
{Starts the search for a element type in the syntax Tree.}
var
  ele: TAstElement;
begin
  ele := FindFirst(name);
  while (ele<>nil) and (ele.idClass <> eleTypeDec) do begin
    ele := FindNext;
  end;
  if ele = nil then exit(nil) else exit( TAstTypeDec(ele) );
end;
function TAstTree.FindNextType: TAstTypeDec;
{Scan recursively toward root, in the syntax tree, until find a type element.
Must be called after calling FindFirst(). If not found, returns NIL.}
var
  ele: TAstElement;
begin
  repeat
    ele := FindNext;
  until (ele=nil) or (ele.idClass = eleTypeDec);
  //Puede que haya encontrado la función o no
  if ele = nil then exit(nil);  //No encontró
  Result := TAstTypeDec(ele);   //devuelve como función
end;
function TAstTree.FindVar(varName: string): TAstVarDec;
{Busca una variable con el nombre indicado en el espacio de nombres actual}
var
  ele : TAstElement;
  uName: String;
begin
  uName := upcase(varName);
  for ele in curNode.elements do begin
    if (ele.idClass = eleVarDec) and (ele.uname = uName) then begin
      Result := TAstVarDec(ele);
      exit;
    end;
  end;
  exit(nil);
end;
function TAstTree.FindType(typName: string): TAstTypeDec;
{Find a type, by name, in the current element of the Synyax Tree.}
var
  ele: TAstElement;
begin
  ele := FindFirst(typName);
//  while (ele<>nil) and (ele.idClass <> eleType) do begin
//    ele := FindNext;
//  end;
  if ele = nil then exit(nil);
  if ele.idClass = eleTypeDec then exit( TAstTypeDec(ele) ) else exit(nil);
end;
//Searching/Identification
function TAstTree.LastNode: TAstElement;
{Devuelve una referencia al último nodo de "main"}
begin
  Result := main.LastNode;
end;
function TAstTree.BodyNode: TAstBody;
{Devuelve la referencia al cuerpo principal del programa.}
begin
  Result := main.BodyNode;
end;
function TAstTree.CurNodeName: string;
{Devuelve el nombre del nodo actual}
begin
  Result := curNode.name;
end;
function TAstTree.ExistsArrayType(itemType: TAstTypeDec; nEle: integer;
  out typFound: TAstTypeDec): boolean;
{Finds an array type declaration, accesible from the current position in the syntax tree.
If found, returns TRUE and the type reference in "typFound".}
begin
  typFound := FindFirstType('');  //Any name
  while (typFound <> nil) and not typFound.IsArrayOf(itemType, nEle) do begin
    typFound := FindNextType;
  end;
  //Verify result
  Result := typFound <> nil;
end;
function TAstTree.ExistsPointerType(ptrType: TAstTypeDec; out
  typFound: TAstTypeDec): boolean;
{Finds a pointer type declaration, accesible from the current position in the syntax tree.
If found, returns TRUE and the type reference in "typFound".}
begin
  typFound := FindFirstType('');  //Any name
  while (typFound <> nil) and not typFound.IsPointerTo(ptrType) do begin
    typFound := FindNextType;
  end;
  //Verify result
  Result := typFound <> nil;
end;
function TAstTree.GetElementBodyAt(posXY: TPoint): TAstBody;
{Busca en el árbol de sintaxis, dentro del nodo principal, y sus nodos hijos, en qué
cuerpo (nodo Body) se encuentra la coordenada del cursor "posXY".
Si no encuentra, devuelve NIL.}
var
  res: TAstBody;

  procedure ExploreForBody(nod: TAstElement);
  var
    ele : TAstElement;
  begin
    //Explora a todos sus elementos
    for ele in nod.elements do begin
      if ele.idClass = eleBody then begin
        //Encontró un Body, verifica
        if ele.posXYin(posXY) then begin
          res := TAstBody(ele);   //guarda referencia
          exit;
        end;
      end else begin
        //No es un body, puede ser un elemento con nodos hijos
        ExploreForBody(ele);  //recursivo
      end;
    end;
  end;
begin
  //Realiza una búsqueda recursiva.
  res := nil;   //Por defecto
  ExploreForBody(main);
  Result := res;
end;
function TAstTree.GetElementAt(posXY: TPoint): TAstElement;
{Busca en el árbol de sintaxis, en qué nodo Body se encuentra la coordenada del
cursor "posXY". Si no encuentra, devuelve NIL.}
var
  res: TAstBody;

  procedure ExploreFor(nod: TAstElement);
  var
    ele : TAstElement;
  begin
    //Explora a todos sus elementos
    for ele in nod.elements do begin
//debugln('nod='+ele.Path);
      //Tiene nodos interiores.
      ExploreFor(ele);  //Explora primero en los nodos hijos
      if res<>nil then exit;  //encontró
      //No encontró en los hijos, busca en el mismo nodo
      if ele.posXYin(posXY) then begin
        res := TAstBody(ele);   //guarda referencia
        if res<>nil then exit;  //encontró
      end;
    end;
  end;
begin
  //Realiza una búsqueda recursiva.
  res := nil;   //Por defecto
  ExploreFor(main);
  Result := res;
end;
function TAstTree.GetElementCalledAt(const srcPos: TSrcPos): TAstElement;
{Explora los elementos, para ver si alguno es llamado desde la posición indicada.
Si no lo encuentra, devueleve NIL.}
var
  res: TAstElement;

  procedure ExploreForCall(nod: TAstElement);
  var
    ele : TAstElement;
  begin
    //Explora a todos sus elementos
    for ele in nod.elements do begin
      if ele.IsCAlledAt(srcPos) then begin
          res := ele;   //guarda referencia
          exit;
      end else begin
        //No es un body, puede ser un eleemnto con nodos hijos
        ExploreForCall(ele);  //recursivo
      end;
    end;
  end;
begin
  //Realiza una búsqueda recursiva.
  res := nil;   //Por defecto
  ExploreForCall(main);
  Result := res;
end;
function TAstTree.GetELementDeclaredAt(const srcPos: TSrcPos): TAstElement;
{Explora los elementos, para ver si alguno es declarado en la posición indicada.}
var
  res: TAstElement;

  procedure ExploreForDec(nod: TAstElement);
  var
    ele : TAstElement;
  begin
    //Explora a todos sus elementos
    for ele in nod.elements do begin
      if ele.IsDeclaredAt(srcPos) then begin
          res := ele;   //guarda referencia
          exit;
      end else begin
        //No es un body, puede ser un eleemnto con nodos hijos
        ExploreForDec(ele);  //recursivo
      end;
    end;
  end;
begin
  //Realiza una búsqueda recursiva.
  res := nil;   //Por defecto
  ExploreForDec(main);
  Result := res;
end;
function TAstTree.FunctionExistInCur(funName: string;
  const pars: TAstParamArray): boolean;
{Indica si la función definida por el nombre y parámetros, existe en el nodo actual.
La búsqueda se hace bajo la consideración de que dos funciones son iguales si tiene el
mismo nombre y los mismos tipos de parámetros.}
var
  ele: TAstElement;
  uname: String;
  funbas: TAstFunBase;
begin
  uname := Upcase(funName);
  for ele in curNode.elements do begin
    if ele.uname = uname then begin
      //hay coincidencia de nombre
      if ele.idClass in [eleFuncImp, eleFuncDec] then begin
        funbas := TAstFunBase(ele);
        //para las funciones, se debe comparar los parámetros
        if SameParamsType(funbas, pars) then begin
          exit(true);
        end;
      end else begin
        //Ssi tiene el mismo nombre que cualquier otro elemento, es conflicto
        exit(true);
      end;
    end;
  end;
  exit(false);
end;
//Debug
procedure TAstTree.print();
{Función de ayuda a la depuración;}
  procedure printNode(nod: TAstElement; level: integer);
  var
    ele: TAstElement;
    expr: TAstExpress;
  begin
    for ele in nod.elements do begin
      if ele.idClass = eleExpress then begin
        expr := TAstExpress(ele);
        debugln(Space(level*2)+'ele='+expr.Name + '('+expr.StoAsStr+')');
      end else begin
        debugln(Space(level*2)+'ele='+ele.Name {+ '('+ele.Sto+')'});
      end;
      printNode(ele, level+1);
    end;
  end;
var
  ele : TAstElement;
begin
  debugln('AST('+IntToStr(main.elements.Count)+') = ');
  for ele in main.elements do begin
    debugln('  ele='+ele.Name);
    if ele.name = 'Body' then printNode(ele, 2);
  end;
  debugln('');
end;
//Constructor y destructor
constructor TAstTree.Create;
begin
  main:= TAstProg.Create;  //No debería
  main.name := 'Main';
  AllCons  := TAstConsDecs.Create(false);  //Create list
  AllVars  := TAstVarDecs.Create(false);   //Create list
  AllFuncs := TAstFunDecs.Create(false);   //Create list
  AllUnits := TAstUnits.Create(false);     //Create list
  AllTypes := TAstTypeDecs.Create(false);  //Create list
  curNode := main;                         //Empieza con el nodo principal como espacio de nombres actual
end;
destructor TAstTree.Destroy;
begin
  main.Destroy;
  AllTypes.Destroy;
  AllUnits.Destroy;
  AllFuncs.Free;
  AllVars.Free;    //por si estaba creada
  AllCons.Free;
  inherited Destroy;
end;
initialization
  //crea el operador NULL
  typNull := TAstTypeDec.Create;
  typNull.name := 'null';

finalization
  typNull.Destroy;
end.
//708
