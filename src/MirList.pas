unit MirList;
{Unidad con la definiicón del objeto MIR. A diferencia del árbol de sintaxis esta
representación intermedia es lineal.
                                          Por Tito Hinostroza 21/10/2023.}
{$mode ObjFPC}{$H+}
{$Define DEBUGMODE}   //Enable MIR visualization
interface
uses
  Classes, SysUtils, fgl, AstElemP65, LexPas, LazLogger;
type  //MIR base class
  //MIR Element type
  TMirType = (
    //Declarations
     mtyDeclars     //Folder for Variable and Constant declaration
    ,mtyVarDec      //Variable declaration
    ,mtyConDec      //Constant declaration
    ,mtyFunDec      //Function declaration
    //Instructions
    ,mtyCode        //Folder for Instructions
    ,mtyAssign      //Assignment from function
    ,mtyFunCall     //Procedure or Function call
    ,mtyLabel       //Label
    ,mtyGoto        //Goto
    ,mtyIfJump      //Conditional jump
    );

  { TMirElement }
  {Base class for all MIR elements.}
  TMirElement = Class;
  TMirElements = specialize TFPGObjectList<TMirElement>;
  TMirElement = Class
    mirType: TMirType;
    text   : String;  //Text representing the instruction.
  end;
type  //MIR Operand for expressions
  //Classes used
  TMirConDec = class;
  TMirVarDec = class;
  TMirFunDec = class;

  {General container for constant values. A constant value can be
  an atomic type (tctAtomic) or a structured type.
  This is only the container. To obtain the final
  constant value, we need one additional parameter (Not stored here by design):
    - Data type: Defines in which field or fields to read the values. It's important
                 the attribute "catType".
  }
  TMirConsValue = object
  public  //Status
    consType : TConsType;   //Constant type for the atomic type.
    function evaluated(typ: TAstTypeDec): Boolean;
    procedure evaluate(typ: TAstTypeDec);
  public  //Values the for atomic type.
    ValInt  : Int64;    //For values t_integer y t_uinteger
    ValFloat: extended; //For values t_float
    ValBool : boolean;  //For values t_boolean
    ValStr  : string;   //For values t_string
  public  //Aditional information
    consRef  : TMirConDec;   //Ref. to TAstConsDec when consType=ctConsRef *** ¿Se necesita además de "conDec"?
    addrVar  : TMirVarDec;   //Ref. to TAstVarDec  when consType=ctVarAddr
    addrFun  : TMirFunDec;   //Ref. to TEleFun when consType=ctFunAddr
  public //Support for Arrays and Objects
    items   : array of TMirConsValue;  //Ítems list
    nItems  : integer;  //Number of items
    curSize : integer;  //*** ¿Se usa?
    fname   : String;   //Field name. Used to identify a field when this constant is an object.
    procedure InitItems;
    procedure AddConsItem(const c: TMirConsValue);
    procedure CloseItems;
  public  //Access to ValInt
    function LByte: byte; inline;  //Returns low byte of integer value.
    function HByte: byte; inline;  //Returns high byte of integer value.
    function EByte: byte; inline;
    function UByte: byte; inline;
    function valuesAsString: string;
  end;

  { TMirOperand }
  TMirOperand = object
    Text    : string;        //Label for the operand.
    opType  : TopType;       //Operand type (otVariab, otConst, otFunct) like AST elements.
    Sto     : TStorage;      //Storage of the value (memory, register, value)
    Typ     : TAstTypeDec;   //Data type for the operand.
    conDec  : TMirConDec;    //Ref. to constant declaration.
    astOperand: TAstExpress; //Ref. to AST element. Should be used only for error location.
    function StoAsStr: string;  //Storage as string
  public //Fields used when "opType" is otFunc.
    funDec  : TMirFunDec;    //Reference to function declaration, when it's accesible.
    elements: array of TMirOperand; //Parameter list.  ***Mejor Cambiar al nombre "pars"
    function FunCallText: string;
    procedure SetParAsVar(i: Integer; vardec0: TMirVarDec);
  public  //Fields used when "opType" is otConst
    value   : TMirConsValue;  //Constant value
    //Functions to read values.
    function val: dword;
    function valL: word;
    function valH: word;
    function valU: word;
    function valE: word;
    function valWlo: word;
    function valWhi: word;
    procedure SetCon_Literal(valBool: Boolean);
    procedure SetCon_Literal(valInt: Int64);
    procedure SetCon_ConstRef(cons0: TMirConDec);
    procedure SetCon_VarAddr(var0: TMirVarDec);
    procedure SetCon_FunAddr(fun0: TMirFunDec);
    procedure ToLiteral();         //Convert constant to "ctLiteral"
    function evaluated: boolean;   //Activated when constant is evaluated.
  public  //Fields used when "opType" is otVariab
    {We use until two fields to get the effective address. They are:
      - The constant offset. Stored in "value" field.
      - The index variable. Referenced by "idxvar" field.
    The use of some specific fields depends on "Sto":
      - stRamFix -> Uses only the constant offset.
      - stRamVar -> Uses only the index variable.
      - stRamVarOf -> Uses the constant offset and the index variable.
    }
    idxvar: TMirVarDec;  //It should be a declared variable.
    function allocated: boolean;
    function vardec: TMirVarDec;
    procedure SetVar_RamFix(vardec0: TMirVarDec);
    procedure SetVar_RamFix(addr: word);
    procedure SetVar_RamVarOf(vardec0: TMirVarDec; idxVar0: TMirVarDec);
  public  //Campos creados solo para compatibilidad para soportar la implementación de las SIF. Deberían cambiarse a futuro.
    function offs: integer;  //Dirección de una variable ???
    function add: word;  //Dirección de una variable ???
    function addL: word;  //Dirección de una variable ???
    function addH: word;
    function name: String;
    function srcDec: TSrcPos;  //Tal cez esta propiedad deba quedar quí
    procedure Exchange(i1, i2: integer);
  end;

type  //Structural elements


  { TMirContainer }

  TMirContainer = class(TMirElement)
  public  //Initialization
    items    : TMirElements;   //Instruction list.
  private  //Insertion
    insPoint: Integer;      //Insert point for instructions
    procedure AddInstruction(itm: TMirElement; position: integer = 1);
    procedure SetInsertMode(iPoint: integer); inline;
    procedure ClearInsertMode; inline;
    function InsertModeActive: Boolean; inline;
  public //Inicialización
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  { TMirDeclars *** NO USADO POR AHORA}
  {Container for Variable and Constant declarations.}
  TMirDeclars = class(TMirElement)
  public  //Initialization
    items    : TMirElements;   //Instruction list.
    constructor Create;
    destructor Destroy; override;
  end;
  { TMirCode *** NO CREADO POR AHORA. BASTA CON TMirContainer}
  {Container for Variable and Constant declarations.}

  { TMirProgFrame }
  {Base class for all porgram-like containers (functions and main program).}
  TMirProgFrame = Class(TMirElement)
    //items    : TMirElements;   //Instruction list.
    declars  : TMirContainer;   //Declaration list.
    instrucs : TMirContainer;   //Instruction list.
  public   //Initialization
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

type  //MIR declarations

  { TMirVarDec }
  TMirVarDec = Class(TMirElement)
    typ      : TAstTypeDec; //Variable type.
    vardec   : TAstVarDec;  //AST Declared variable, when it's associated to AST. If not it's NIL.
    inival   : TMirOperand;  //Initial value expression. Opctional.
    IsParameter: Boolean;   //Flag for variables that are parameters.
    required : boolean;     {Indicates the variable is required to be allocated. Work
                            for variables used as registers. *** ¿Es necesario?}
  public   //Manejo de parámetros adicionales
    adicPar  : TAdicVarDec;  //Parámetros adicionales en la declaración de la variable.
  public  //Campos para guardar las direcciones físicas asignadas en RAM.
    allocated: boolean;    //Activated when variable is allocated (RAM or register).
    storage  : TStorage;   //Depend on adicPar.hasAdic.
    addr     : word;       //Base address.
    function addrL: word; inline;  //Devuelve la dirección absoluta de la variable (LOW)
    function addrH: word; inline;  //Devuelve la dirección absoluta de la variable (HIGH)
    function addrE: word; inline;  //Devuelve la dirección absoluta de la variable (EXTRA)
    function addrU: word; inline;  //Devuelve la dirección absoluta de la variable (ULTRA)
    function AddrString: string;   //Devuelve la dirección física como cadena
    procedure ResetAddress; //Limpia las direcciones físicas
    function stoStr: string;
  public
    constructor Create; virtual;
  end;

  { TMirConDec }
  TMirConDec = Class(TMirElement)
  public
    typ      : TAstTypeDec; //Constant type.
    condec   : TAstConsDec;  //AST Declared variable.
    value    : TConsValue;   //Constant value.  *** NO debería usarse. Debe salir de "inival".
    inival   : TMirOperand;  //Initial value expression. Must always exist.
    evaluated: boolean;
  public
    procedure UpdateText;    //Updates "Text" attribute.
    constructor Create; virtual;
  end;

  TMirParam = object
//    name    : string;      //Parameter name
//    typ     : TAstTypeDec; //Reference to type
    vardec  : TMirVarDec;  //Reference to variable used for this parameter
//    srcPos  : TSrcPos;     //Parameter location.
//    adicVar : TAdicVarDec; //Aditional option for "vardec".
  end;
  TMirParamArray = array of TMirParam;

  { TMirFunDec }
  TMirFunDec = class(TMirProgFrame)
    pars     : TMirParamArray; //Reference to paramenters.
    astFunDec: TAstFunDec;     //AST function.
    //binOper  : char;         //Binary operator when it's associated to an operator.
    IsTerminal2: boolean;      //Flag. Indicates is function is terminal.
    procedure ReadParamsFromAST(astFunDec0: TAstFunDec);
  public  //Operator
    operTyp    : TOperatorType; //Operand type
    oper       : string;   //Operator associated to the function when it works as a method.
  public  //Phisyscal
    adrr   : integer;  //Physical address where function is compiled.
    adrr2  : integer;  //Aditional physical address, for other entry point of the function.
    srcSize: integer;  {Tamaño del código compilado. En la primera pasada, es referencial,
                        porque el tamaño puede variar al reubicarse.}
    coded : boolean;   //Indicates the function was compiled in memory.
  public  //Initialization
    constructor Create;
  end;

type  //Innstructions elements

  { TMirFunCall }
  TMirFunCall = Class(TMirElement)
    func  : TMirOperand;     //Function called.
    procedure UpdateText;    //Updates "Text" attribute.
    constructor Create;
  end;

  { TMirAssign }
  TMirAssign = Class(TMirElement)
  public
    isSimple: Boolean;       {When is TRUE, instruction is: var_a := <simple operamd>;
                              When is FALSE, instruction is: var_a := <function_call>;
                               *** Es necesario?}
    dest    : TMirOperand;   //Target variable.
    opSrc   : TMirOperand;   //Source operand, when "isSimple = TRUE".
    procedure SetDestFromVarDec(vardec: TMirVarDec);
  public
    procedure UpdateText;    //Updates "Text" attribute.
    constructor Create;
  end;

  { TMirLabel }
  TMirLabel = class(TMirElement)
    ilabel: integer;    //Idntifier for label used in Goto instructions. We use number
                      //instead of string for speed.
  public
    procedure UpdateText;    //Updates "Text" attribute.
    constructor Create;
  end;

  { TMirGoto }
  TMirGoto = Class(TMirElement)
  public
    ilabel  : integer;          //Label identifier. We used a number instead of a string for speed.
  public
    procedure UpdateText;    //Updates "Text" attribute.
    constructor Create;
  end;

  { TMirIfGoto }
  TMirIfGoto = Class(TMirElement)
  public
    condit  : TMirOperand;   //Condition operand.
    negated : Boolean;       //Flag to indicate the condition is inverted.
    ilabel  : integer;          //Label identifier. We used a number instead of a string for speed.
  public
    procedure UpdateText;    //Updates "Text" attribute.
    constructor Create;
  end;

type  //Main Container
  { TMirList }
  TMirList = class
  public
//    items   : TMirElements;
    root    : TMirProgFrame; //Root node.
    ngotos  : Integer;      //Number of gotos
  public  //Adding instructions
    function AddFunCall(mcont: TMirContainer; Op1: TAstExpress): TMirFunCall;
    function AddGoto(mcont: TMirContainer; ilabel: integer = - 1): TMirGoto;
    function AddGoto(mcont: TMirContainer; mlabel: TMirLabel): TMirGoto;
    function AddLabel(mcont: TMirContainer): TMirLabel;
    procedure EndGoto(mcont: TMirContainer; gotoInstr: TMirGoto);
    procedure EndGoto(mcont: TMirContainer; ilabel: integer);
    function AddIfGoto(mcont: TMirContainer; condition: TAstExpress;
      negated: boolean): TMirIfGoto;
    procedure EndIfGoto(mcont: TMirContainer; ifInstruc: TMirIfGoto);
  public  //Initialization
    procedure Clear;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  procedure AddFunParamAssign(mcont: TMirContainer;
           var func: TMirOperand; Op1: TAstExpress);
  function AddAssign(mcont: TMirContainer; vardec: TMirVarDec;
           Op2: TAstExpress): TMirAssign;
  //Adding declarations
  function AddMirConDec(mcont: TMirContainer; conDec0: TAstConsDec): TMirConDec;
  function AddMirVarDec(mcont: TMirContainer; varDec0: TAstVarDec): TMirVarDec;
  function AddMirVarDec(mcont: TMirContainer; varName: string;
           eleTyp: TAstTypeDec): TMirVarDec;
  function AddMirFunDecSNF(mcont: TMirContainer; funcName0: TAstFunDec): TMirFunDec;
  function AddMirFunDecUNF(mcont: TMirContainer; funcName0: TAstFunDec): TMirFunDec;

type  //Events for AST elements
  //This type will be used to cast the field TAstFunDec.codSysInline.
  //This needs to be defined here because uses the TMirOperand type declared here.
  TCodSysInline = procedure(var fun: TMirOperand) of object;


implementation

{ TMirContainer }
//Insertion
procedure TMirContainer.AddInstruction(itm: TMirElement;
         position: integer = 1);
{Add an item to the "instrucs" section of the container "mcont".}
begin
  if insPoint<>-1 then begin
    items.Insert(insPoint, itm);
    inc(insPoint);
  end else begin
    items.Add(itm);
  end;
end;
procedure TMirContainer.SetInsertMode(iPoint: integer);
{Set the MIR List in mode "Insert".}
begin
  insPoint := iPoint;
end;
procedure TMirContainer.ClearInsertMode;
{Leaves the mode "Insert".}
begin
  insPoint := -1;
end;
function TMirContainer.InsertModeActive: Boolean;
{Indicates if the mode "Insert" is activated.}
begin
  exit(insPoint <> -1);
end;
procedure TMirContainer.Clear;
begin
  items.Clear;
  insPoint := -1;     //Disable
end;
//Initialization
constructor TMirContainer.Create;
begin
  inherited;
  mirType := mtyCode;
  items:= TMirElements.Create(true);
end;
destructor TMirContainer.Destroy;
begin
  items.Destroy;
  inherited Destroy;
end;
{ TMirDeclars }
constructor TMirDeclars.Create;
begin
  inherited;
  mirType := mtyDeclars;
  items:= TMirElements.Create(true);
end;
destructor TMirDeclars.Destroy;
begin
  items.Destroy;
  inherited Destroy;
end;
{ TMirProgFrame }
procedure TMirProgFrame.Clear;
begin
  declars.Clear;
  instrucs.Clear;
end;
constructor TMirProgFrame.Create;
begin
  declars := TMirContainer.Create;
  instrucs := TMirContainer.Create;
end;
destructor TMirProgFrame.Destroy;
begin
  declars.Destroy;
  instrucs.Destroy;
  inherited Destroy;
end;
{ TMirConsValue }
procedure TMirConsValue.InitItems;
begin
  nItems := 0;
  curSize := CONS_ITEM_BLOCK;   //Block size
  setlength(items, curSize);  //initial size
end;
procedure TMirConsValue.AddConsItem(const c: TMirConsValue);
begin
  items[nItems] := c;
  inc(nItems);
  if nItems >= curSize then begin
    curSize += CONS_ITEM_BLOCK;   //Increase size by block
    setlength(items, curSize);  //make space
  end;
end;
function TMirConsValue.evaluated(typ: TAstTypeDec): Boolean;
var
  itemExp: TMirConsValue;
begin
  if typ.catType = tctAtomic then begin
    //Simple type
    case consType of
    ctLiteral: exit(true);
    ctConsRef: exit(consRef.evaluated);
    ctVarAddr: exit(addrVar.allocated);
    ctFunAddr: exit(addrFun.coded);
    end;
  end else if typ.catType = tctArray then begin;
    //Constant array. Let's evaluate by items
    for itemExp in items do begin
      if not itemExp.evaluated(typ.itmType) then exit(false);
    end;
    exit(true);
  end else begin
    //Not implemented other types
    exit(false);
  end;
end;
procedure TMirConsValue.evaluate(typ: TAstTypeDec);
var
  itemExp: TMirConsValue;
begin
  if Typ.catType = tctAtomic then begin
    //Simple type
    case consType of
    ctLiteral: exit;   //No need to convert.
    ctConsRef: ValInt := consRef.value.ValInt;
    ctVarAddr: ValInt := addrVar.addr;
    ctFunAddr: ValInt := addrFun.adrr;
    end;
  end else if Typ.catType = tctArray then begin
    //Constant array. Let's evaluate by items
    for itemExp in items do begin
      itemExp.Evaluate(typ.itmType);
    end;
  end else begin
    //Object or pointer.
    //Pointers are not allowed because constant pointers generate variables: ($123)^
    //Object are not implemented.
    debugln('Not implemented');
  end;
end;
procedure TMirConsValue.CloseItems;
begin
  setlength(items, nItems);
end;
function TMirConsValue.LByte: byte;
begin
  Result := LO(word(valInt));
end;
function TMirConsValue.HByte: byte;
begin
  Result := HI(word(valInt));
end;
function TMirConsValue.EByte: byte;
begin
  Result := (valInt >> 16) and $FF;
end;
function TMirConsValue.UByte: byte;
begin
  Result := (valInt >> 24) and $FF;
end;
function TMirConsValue.valuesAsString: string;
{Returns a string containing the abstract of values stored.}
var
  tmp: Char;
begin
  If ValBool then tmp := 'T' else tmp := 'F';
  Result := 'int=' + IntToStr(ValInt) + ',bool=' + tmp;
end;
{ TMirVarDec }
function TMirVarDec.addrL: word;
{Dirección absoluta de la variable de menor pero, cuando es de tipo WORD.}
begin
  Result := addr;
end;
function TMirVarDec.addrH: word;
{Dirección absoluta de la variable de mayor pero, cuando es de tipo WORD.}
begin
  Result := addr + 1;
end;
function TMirVarDec.addrE: word;
begin
  Result := addr + 2;
end;
function TMirVarDec.addrU: word;
begin
  Result := addr + 3;
end;
function TMirVarDec.AddrString: string;
{Devuelve una cadena, que representa a la dirección física.}
begin
  if vardec.typ.IsByteSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else if vardec.typ.IsWordSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else if vardec.typ.IsDWordSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else begin
    Result := '';   //Error
  end;
end;
procedure TMirVarDec.ResetAddress;
begin
  addr := 0;
end;
function TMirVarDec.stoStr: string;
begin
  WriteStr(Result, storage);
end;
constructor TMirVarDec.Create;
begin
  mirType := mtyVarDec;
end;
{ TMirConDec }
procedure TMirConDec.UpdateText;
begin
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  if inival.opType = otFunct then
    Text := Text + ' := ' + inival.FunCallText
  else
    Text := Text + ' := ' + inival.Text;
  {$ENDIF}
end;
constructor TMirConDec.Create;
begin
  mirType := mtyConDec;
end;
{ TMirFunDec }
procedure TMirFunDec.ReadParamsFromAST(astFunDec0: TAstFunDec);
{Read parameters from an AST function declaration.}
var
  i: Integer;
begin
  //Add parameteres
  SetLength(pars, length(astFunDec0.pars));
  for i:=0 to High(astFunDec0.pars) do begin
    pars[i].vardec := TMirVarDec(astFunDec0.pars[i].vardec.mirVarDec);

  end;
end;
constructor TMirFunDec.Create;
begin
  inherited;
  mirType := mtyFunDec;
end;
{ TMirOperand }
function TMirOperand.StoAsStr: string;
begin
  WriteStr(Result, Sto);
end;
function TMirOperand.FunCallText: string;
//Returns the function call in text.
//Only works when "opType" is otFunc.
var
  i: Integer;
begin
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  Result := Text + '(';
  if (funDec=Nil) or  //Is System
     (funDec.astFunDec.callType in [ctUsrInline, ctSysInline]) then begin
      //Only in this case, shows parameters
      for i:=0 to High(elements) do begin
        //Agrega nombre de parámetro
          if i=0 then Result += elements[i].Text
          else        Result += ',' + elements[i].Text;
      end;
  end;
  Result += ')';
  {$ENDIF}
end;
procedure TMirOperand.SetParAsVar(i: Integer; vardec0: TMirVarDec);
{Set a parameter like a variable}
var
  par: ^TMirOperand;
begin
  par := @elements[i];
  //Convert "par1" to the temporal variable
  par^.Text   := vardec0.text;
  par^.SetVar_RamFix(vardec0);
  par^.astOperand := nil;
end;
//Fields used when "Sto" is stConst
function TMirOperand.val: dword; inline;
begin
  Result := value.ValInt;
end;
function TMirOperand.valL: word; inline;
begin
  Result := LO(word(value.ValInt));
end;
function TMirOperand.valH: word; inline;
begin
  Result := HI(word(value.ValInt));
end;
function TMirOperand.valU: word; inline;
begin
  Result := (value.valInt >> 24) and $FF;
end;
function TMirOperand.valE: word; inline;
begin
  Result := (value.valInt >> 16) and $FF;
end;
function TMirOperand.valWlo: word; inline;
begin
  Result := word(value.ValInt);
end;
function TMirOperand.valWhi: word; inline;
begin
  Result := (value.valInt >> 16) and $FFFF;
end;
procedure TMirOperand.SetCon_Literal(valBool: Boolean);
{Set the value of a Constant boolean expression.}
begin
  opType := otConst;
  value.consType := ctLiteral;   //Only set the atomic constant type
  value.valBool := valBool;    //Tal vez no sea necesario si usamos solo "value.ValInt"
  //Como en algunos casos se usa el valor numérico, lo fijamos también.
  if valBool then begin
    value.ValInt := 255;
  end else begin
    value.ValInt := 0;
  end;
end;
procedure TMirOperand.SetCon_Literal(valInt: Int64);
begin
  opType := otConst;
  value.consType := ctLiteral;   //Only set the atomic constant type
  value.ValInt := valInt;
end;
procedure TMirOperand.SetCon_ConstRef(cons0: TMirConDec);
begin
  opType := otConst;
  value.consType := ctConsRef;   //Only set the atomic constant type
  value.consRef := cons0;  //Keep reference
  ToLiteral;
end;
procedure TMirOperand.SetCon_VarAddr(var0: TMirVarDec);
begin
  opType := otConst;
  value.consType := ctVarAddr;   //Only set the atomic constant type
  value.addrVar := var0;  //Keep reference
  ToLiteral;
end;
procedure TMirOperand.SetCon_FunAddr(fun0: TMirFunDec);
begin
  opType := otConst;
  value.consType := ctFunAddr;
  value.addrFun := fun0;  //Keep reference
  ToLiteral;
end;
procedure TMirOperand.ToLiteral();
{Evaluate constant values to literal values in "value". Must be called after testing
with evaluated().}
begin
  value.evaluate(typ);
end;
function TMirOperand.evaluated: boolean;
{Indicates if the constant value is evaluated. It means if its literal value can be read
from "value" field.}
begin
  exit(value.evaluated(typ));
end;
function TMirOperand.allocated: boolean;
{Indicates if the variable is allocated ein memory.}
begin
  if Sto = stRamFix then begin
    //Allocations depends on constant value.
    exit(value.evaluated(typ));
  end else begin
    //In all other cases (indexed, addressed by constant )
    exit(true);
  end;
end;
function TMirOperand.vardec: TMirVarDec;
{Give the reference to a variable declaration when it exists. Otherwise returns NIL.}
begin
  if Sto= stRamFix then begin
    if value.consType = ctVarAddr then exit(value.addrVar)
    else exit(nil);
  end else begin
    exit(nil);
  end;
end;
procedure TMirOperand.SetVar_RamFix(vardec0: TMirVarDec);
begin
  opType    := otVariab;
  Sto       := stRamFix;
  //Add the Constant offset in "value".
  value.consType := ctVarAddr;
  value.addrVar  := vardec0;  //Keep reference
end;
procedure TMirOperand.SetVar_RamFix(addr: word);
begin
  opType    := otVariab;
  Sto       := stRamFix;
  //Add the Constant offset in "value".
  value.consType := ctLiteral;
  value.ValInt := addr;
end;
procedure TMirOperand.SetVar_RamVarOf(vardec0: TMirVarDec;
  idxVar0: TMirVarDec);
begin
  opType    := otVariab;
  Sto       := stRamVarOf;
  //Add the Constant offset in "value".
  value.consType := ctVarAddr;
  value.addrVar  := vardec0;  //Keep reference
  //Add the index
  idxVar := idxVar0;
end;

function TMirOperand.offs: integer;
begin
  exit(value.ValInt);
end;
function TMirOperand.add: word;
begin
  exit(value.ValInt);
end;
function TMirOperand.addL: word;
begin
  exit(value.ValInt);
end;
function TMirOperand.addH: word;
begin
  exit(value.ValInt+1);
end;
function TMirOperand.name: String;
begin
  Exit(text);
end;
function TMirOperand.srcDec: TSrcPos;
var
  vd: TMirVarDec;
begin
  exit(astOperand.srcDec);
end;

procedure TMirOperand.Exchange(i1, i2: integer);
var
  tmp: TMirOperand;
begin
  tmp := elements[i1];
  elements[i1] := elements[i2];
  elements[i2] := tmp;
end;

{ TMirFunCall }
procedure TMirFunCall.UpdateText;
{Actualiza el "Text" de la instrucción a partir del operando función.}
begin
  Text := func.FunCallText;
end;
constructor TMirFunCall.Create;
begin
  mirType := mtyFunCall;
end;
{ TMirAssign }
procedure TMirAssign.SetDestFromVarDec(vardec: TMirVarDec);
{Set "dest" attribute from a Mir Variable declaration. It's equivalent to create a new
MIR Operand (from a MIR variable declaration) and set "dest" to that new operand.
That's why this operand doesn't have an AST reference.}
begin
  dest.Text := vardec.text;
  dest.opType := otVariab;
  dest.SetVar_RamFix(vardec);
  dest.astOperand := nil;
end;
procedure TMirAssign.UpdateText;
{Set the "Text" attribute from the content of the instruction.}
begin
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  if isSimple then begin  //Simple assignment  *** ¿Mo basta con leer opSrc.opType?
    Text := dest.Text + ' := ' + opSrc.Text;
  end else begin   //Assignment from function
    Text := dest.Text + ' := ' + opSrc.FunCallText;
  end;
  {$ENDIF}
end;
constructor TMirAssign.Create;
begin
  mirType := mtyAssign;
end;
{ TMirLabel }
procedure TMirLabel.UpdateText;
begin
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  Text := 'L' + IntToStr(ilabel) + ':';
  {$ENDIF}
end;
constructor TMirLabel.Create;
begin
  mirType := mtyLabel;
end;
{ TMirGoto }
procedure TMirGoto.UpdateText;
begin
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  Text := 'GOTO L' + IntToStr(ilabel);
  {$ENDIF}
end;
constructor TMirGoto.Create;
begin
  mirType := mtyGoto;
end;
{ TMirIfGoto }
procedure TMirIfGoto.UpdateText;
begin
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  if negated then Text := 'IFNOT ' else Text := 'IF ';
  if condit.opType = otFunct then begin  //Simple assignment
    Text += condit.FunCallText + ' GOTO L' + IntToStr(ilabel);
  end else begin   //Assignment from function
    Text += condit.Text + ' GOTO L' + IntToStr(ilabel);
  end;
  {$ENDIF}
end;
constructor TMirIfGoto.Create;
begin
  mirType := mtyIfJump;
end;
procedure GetMIROperandFromASTExpress(out MirOper: TMirOperand;
                                      const AstOper: TAstExpress);
{Read data from a TAstExpress and set a TMirOperand}
var
  AstVarDec: TAstVarDec;
begin
  MirOper.opType := AstOper.opType;  //Must be set
  MirOper.Text := AstOper.name;
  MirOper.Typ  := AstOper.Typ;
  if MirOper.Typ.catType = tctAtomic then begin  //Atomic type
    if AstOper.opType = otConst then begin
      if AstOper.consType in [ctVarAddr, ctFunAddr] then begin
        MirOper.Text := '@' + AstOper.name;  //UPdate name including indicator.
      end;
      MirOper.value.consType := AstOper.consType;  //Constant type
      //We need to update references to declaractions too. It's supposed they should exist.
      if MirOper.value.consType = ctConsRef then
         MirOper.value.consRef := TMirConDec(AstOper.consRef.mirConDec);
      if MirOper.value.consType = ctVarAddr then
         MirOper.value.addrVar := TMirVarDec(AstOper.addrVar.mirVarDec);
      if MirOper.value.consType = ctFunAddr then
        MirOper.value.addrFun := TMirFunDec(AstOper.addrFun.mirFunDec);
//    end else if AstOper.opType = otVariab then begin
//      AstVarDec := AstOper.vardec;
//      if AstVarDec=nil then begin
//        MirOper.varDec := Nil;  //No defined by a variable declaration
//      end else begin
//        MirOper.varDec := TMirVarDec(AstVarDec.mirVarDec);  //Must be set
//      end;
//      MirOper.funDec := nil;
    end else if AstOper.opType = otFunct then begin
      MirOper.funDec := TMirFunDec(AstOper.fundec.mirFunDec);
    end;
    MirOper.astOperand := AstOper;
//  end else if Typ.catType = tctArray then begin
//
  end else begin
      //Object or pointer.
    debugln('Not implemented');
  end;
end;
//Adding declarations
function AddMirConDec(mcont: TMirContainer; conDec0: TAstConsDec): TMirConDec;
var
  astInival: TAstExpress;
begin
  Result := TMirConDec.Create;
  Result.text       := conDec0.name;
  Result.typ        := conDec0.typ;
  Result.condec     := conDec0;
  //Set initial value
  astInival := TAstExpress(conDec0.elements[0]);
  GetMIROperandFromASTExpress(Result.inival, astInival);
  //Set parameters
  if astInival.opType = otFunct then begin
    //Only functions should have parameters
    AddFunParamAssign(mcont, Result.inival, astInival);
  end;
  Result.UpdateText;
  //Add to declarations container
  mcont.items.Add(Result);
end;
function AddMirVarDec(mcont: TMirContainer; varDec0: TAstVarDec): TMirVarDec;
{Add a Variable  declaration}
begin
  Result := TMirVarDec.Create;
  Result.text       := varDec0.name;
  Result.typ        := varDec0.typ;
  Result.vardec     := varDec0;
  Result.IsParameter:= varDec0.IsParameter;
  Result.required   := varDec0.required;
  Result.adicPar    := varDec0.adicPar;
  //Set initial value
  //astInival := TAstExpress(conDec0.elements[0]);
  //GetMIROperandFromASTExpress(Result.inival, astInival);

  //Add to declarations container
  mcont.items.Add(Result);
end;
function AddMirVarDec(mcont: TMirContainer; varName: string; eleTyp: TAstTypeDec
  ): TMirVarDec;
{Add a variable declaration to the container "mcont". The declaration is created
after the last declaration.}
var
  n: Integer;
begin
  //Create unique name
  n := mcont.items.Count;
  if varName='' then varName := '#' + IntToStr(n);

  Result := TMirVarDec.Create;
  Result.text := varName;
  Result.typ  := eleTyp;
  //Add to declarations container
  mcont.items.Add(Result);
end;
function AddMirFunDecSNF(mcont: TMirContainer; funcName0: TAstFunDec): TMirFunDec;
begin
  Result := TMirFunDec.Create;
  Result.text := funcName0.name;
  Result.astFunDec := funcName0;
  Result.IsTerminal2 := funcName0.IsTerminal2;
  Result.operTyp := funcName0.operTyp;
  Result.oper := funcName0.oper;
  mcont.items.Add(Result);
end;
function AddMirFunDecUNF(mcont: TMirContainer; funcName0: TAstFunDec): TMirFunDec;
{Add a User Normal Function to the MIR list.}
begin
  Result := TMirFunDec.Create;
  Result.text := funcName0.name;
  Result.astFunDec := funcName0;
  Result.IsTerminal2 := funcName0.IsTerminal2;
  Result.oper := funcName0.oper;
  mcont.items.Add(Result);
end;
{ TMirList }
//Adding instructions
procedure AddFunParamAssign(mcont: TMirContainer;
                                     var func: TMirOperand; Op1: TAstExpress);
var
  astPar: TAstExpress;
  vardec, tmpdec: TMirVarDec;
  i: Integer;
begin
  if Op1.opType <> otFunct then exit;  //Protection
  //Set parameters number.
  SetLength(func.elements, Op1.elements.count);
  //Proccess according to the function type
  case Op1.fundec.callType of
  ctUsrNormal, ctSysNormal: begin      //Normal function
    //        {IT's the form:
    //             x := func(x,y);
    //                  \_______/
    //                     Op2
    //        }
    {Move all parameters (children nodes) to a separate assignment}
    for i:=0 to Op1.elements.Count-1 do begin
      astPar := TAstExpress(Op1.elements[i]);
      GetMIROperandFromASTExpress(func.elements[i], astPar);
      vardec := func.funDec.pars[i].vardec;
      AddAssign(mcont, vardec, astPar); //???? Y no hay que buscar el método _set?
    end;
  end;
  ctUsrInline, ctSysInline: begin       //INLINE function
    {IT's the form:
         x := A + B
              \___/
               Op2
    or:
         x := A++        }
    {We expect parameters A, B should be simple operands (Constant or variables)
    otherwise we will move them to a separate assignment}
    //Check if some parameter needs to be converted in an assignment
    for i:=0 to Op1.elements.Count-1 do begin
      astPar := TAstExpress(Op1.elements[i]);
      GetMIROperandFromASTExpress(func.elements[i], astPar);
      if astPar.opType = otFunct then begin
        //Create a new temporal variable declaration.
        tmpdec := AddMirVarDec(mcont, '', astPar.Typ);
        //Insert a new assigment
        AddAssign(mcont, tmpdec, astPar);
        //Update the parameter
        func.SetParAsVar(i, tmpdec);  //Convert parameter to the temporal variable.
      end;
    end;
  end;
  end;
end;
function TMirList.AddFunCall(mcont: TMirContainer; Op1: TAstExpress
  ): TMirFunCall;
{Convierte una expresión, de llamada a una función/procedimiento, a su su representación
en el MIR.}
begin
  Result:= TMirFunCall.Create;
  //Set function operand
  GetMIROperandFromASTExpress(Result.func, Op1);
  AddFunParamAssign(mcont, Result.func, Op1);
  Result.UpdateText;              //Update label.
  //Add to list
  mcont.AddInstruction(Result);
end;
function AddAssign(mcont: TMirContainer; vardec: TMirVarDec;
  Op2: TAstExpress): TMirAssign;
{General function to add an assignment instruction to the MIR container "mcont".
 The assignment in created in TAC format. This is a recursive function.
Parameters:
  "vardec" -> Variable declaration.
  "Op2"    -> Operando from the value for assignment is taken. Can be a simple Operand
              or a function call. It is an AST expression element.}
var
  astPar: TAstExpress;
  i: Integer;
  tmpdec: TMirVarDec;
begin
  Result:= TMirAssign.Create;
  if Op2.opType <> otFunct then Result.isSimple := true else Result.isSimple := false;
  Result.SetDestFromVarDec(vardec);  //Set variable destination.
  //Set function operand
  GetMIROperandFromASTExpress(Result.opSrc, Op2);
  //Set parameters
  if Op2.opType = otFunct then begin
    //Only functions should have parameters
    AddFunParamAssign(mcont, Result.opSrc, Op2);
  end;
  Result.UpdateText;              //Update label.
  //Add to list
  mcont.AddInstruction(Result);
end;
function TMirList.AddGoto(mcont: TMirContainer; ilabel: integer = -1): TMirGoto;
begin
  Result:= TMirGoto.Create;
  if ilabel=-1 then begin   //Generates the index label.
    inc(ngotos);  //Count
    Result.ilabel := ngotos; //Update label
  end else begin    //Uses the parameter index.
    Result.ilabel := ilabel; //Update label
  end;
  Result.UpdateText;              //Update label.
  //Add to list
  mcont.AddInstruction(Result);
end;
function TMirList.AddGoto(mcont: TMirContainer; mlabel: TMirLabel): TMirGoto;
begin
  Result:= TMirGoto.Create;
  Result.ilabel := mlabel.ilabel; //Update label
  Result.UpdateText;              //Update label.
  //Add to list
  mcont.AddInstruction(Result);
end;
function TMirList.AddLabel(mcont: TMirContainer): TMirLabel;
var
  lblInstruct: TMirLabel;
begin
  Result := TMirLabel.Create;
  inc(ngotos);  //Count
  Result.ilabel := ngotos;
  Result.UpdateText;
  mcont.AddInstruction(Result);
end;
procedure TMirList.EndGoto(mcont: TMirContainer; gotoInstr: TMirGoto);
var
  lblInstruct: TMirLabel;
begin
  lblInstruct := TMirLabel.Create;
  lblInstruct.ilabel := gotoInstr.ilabel;
  lblInstruct.UpdateText;
  mcont.AddInstruction(lblInstruct);
end;
procedure TMirList.EndGoto(mcont: TMirContainer; ilabel: integer);
var
  lblInstruct: TMirLabel;
begin
  lblInstruct := TMirLabel.Create;
  lblInstruct.ilabel := ilabel;
  lblInstruct.UpdateText;
  mcont.AddInstruction(lblInstruct);
end;
function TMirList.AddIfGoto(mcont: TMirContainer;
  condition: TAstExpress; negated: boolean): TMirIfGoto;
{General function to add an IF instruction to the MIR container "mcont".
Parameters:
  "condition" -> The condition expression.
}
begin
  Result:= TMirIfGoto.Create;
  Result.negated := negated;
  inc(ngotos);  //Count
  Result.ilabel := ngotos; //Update label
  //Set function operand
  GetMIROperandFromASTExpress(Result.condit, condition);
  //Set parameters
  if condition.opType = otFunct then begin
    //Only functions should have parameters
    AddFunParamAssign(mcont, Result.condit, condition);
  end;
  Result.UpdateText;              //Update label.
  //Add to list
  mcont.AddInstruction(Result);
end;
procedure TMirList.EndIfGoto(mcont: TMirContainer; ifInstruc: TMirIfGoto);
{Finish the IF ... GOTO instruction.}
var
  lblInstruct: TMirLabel;
begin
  lblInstruct := TMirLabel.Create;
  lblInstruct.ilabel := ifInstruc.ilabel;
  lblInstruct.UpdateText;
  mcont.AddInstruction(lblInstruct);
end;
//Initialization
procedure TMirList.Clear;
var
  declarations: TMirDeclars;
  code: TMirContainer;
begin
  root.Clear;
  ngotos := 0;
end;
constructor TMirList.Create;
begin
  root := TMirProgFrame.Create;
end;
destructor TMirList.Destroy;
begin
  root.Destroy;
  inherited Destroy;
end;
end.
