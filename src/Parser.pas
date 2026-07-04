{Parser
Clase para la creación de un analizador sintáctico en Pascal.
Todas las rutinas definidas aquí son independientes de la de CPU.
}
//{$Define LogExpres}
unit Parser;
interface
uses
  Classes, SysUtils, Types, LazLogger, alexiaLex, ASTunit;
type  //Declaraciones generales
//Primary location for elements
{Current location for scan. This tells the compiler where it's scanning. It useful because
some declarations have to be interpreted in different ways according to the location.}
TElemLocation = (
              locMain,       //En el programa principal.
              locInterface,  //En INTERFACE de una unidad.
              locImplement   //En IMPLEMENTATION de una unidad.
);

type  //TParser
{Clase que implementa al analizador sintáctico (Parser).}
TParser = class
public    //Componentes principales del compilador
  lex  : TAleLexer;        //Analizador léxico
  msg  : TMessageManager;  //Referencia al gestor de mensajes
  ast  : TProgram;         //Árbol de sintaxis abstracto
public    //Messages
  procedure ClearError;
  function HayError: boolean; inline;          //Flag for errors
  //Rutinas de generación de mensajes
  procedure GenInfo(txt: string; const srcPos: TSrcPos);
  procedure GenInfo(txt: string);
  //Rutinas de generación de advertencias
  procedure GenWarn(txt: string; const srcPos: TSrcPos);
  procedure GenWarn(txt: string);
  //Rutinas de generación de error. Envolturas para llamar al gestor de mensajes.
  procedure GenError(txt: string; const srcPos: TSrcPos);
  procedure GenError(txt: String; const Args: array of const; const srcPos: TSrcPos);
  procedure GenError(txt: string);
  procedure GenError(txt: String; const Args: array of const);
private   //Objetos auxiliares
  NamesList: TStringList;
protected //Calls to Directive Module (ParserDirec.pas)
  callProcDIRline  : procedure(const AsmLin: string; out ctxChanged: boolean) of object;
protected // Métodos auxiliares para el parser
  function tokIdent: TTokenIdent; inline;
  function CaptureSemicolon: boolean;
  procedure SkipWhites;
  procedure SkipWhitesNoDirect;
  procedure Next;
  function ConsumeTok(tokId: TTokenIdent; const msgErr: string): boolean;
private   // Expresiones
  function ParseNumberLiteral: TNumberLiteral;
  function ParseIdentifier: TExpression;
  function ParseStringLiteral: TStringLiteral;
  function ParseArrayLiteral: TArrayLiteral;
  function ParseFactor: TExpression;
  function ParseTerm: TExpression;
  function ParseSimpleExpression: TExpression;
  function ParseExpression: TExpression;
private   // Métodos auxiliares para las declaraciones
  procedure ParseParameters(Params: TVarDeclList);
  function ParseSubrangeType: TSubrangeTypeDef;
  function ParseEnumType: TEnumTypeDef;
  function ParseArrayTypeDef: TArrayTypeDef;
  function ParseRecordTypeDef: TRecordTypeDef;
  function ParsePointerType: TPointerTypeDef;
  function ParseTypeDefinition: TTypeDef;
private   // Declaraciones
  procedure ParseVarDeclaration(declars: TDeclarations);
  procedure ParseConstDeclaration(declars: TDeclarations);
  procedure ParseProcedureDeclaration(declars: TDeclarations);
  procedure ParseFunctionDeclaration(declars: TDeclarations);
  procedure ParseTypeDeclaration(declars: TDeclarations);
private   // Instrucciones
  procedure ParseAssigOrProcedureCall(var Block: TBlock);
  procedure ParseIfStatement(var Block: TBlock);
  procedure ParseWhileLoop(var Block: TBlock);
  procedure ParseForLoop(var Block: TBlock);
  procedure ParseRepeatUntil(var Block: TBlock);
  function ParseCaseBranch: TCaseBranch;
  procedure ParseCaseStatement(var Block: TBlock);
  procedure ParseWithStatement(var Block: TBlock);
  procedure ParseExitStatement(var Block: TBlock);
public    // Sentencia, bloque y programa
  procedure ParseStatement(Body: TBlock);
  procedure ParseDeclarations(Declars: TDeclarations);
  procedure ParseBody(Body: TBlock);
  procedure ParseProgram;
public    // Inicialización
  procedure Clear;  // Reinicia el compilador para un nuevo programa
  constructor Create(msg0: TMessageManager);
  destructor Destroy; override;
end;

implementation

{TParser}
{$region "Messages"}
procedure TParser.ClearError;
{Limpia la bandera de errores. Tomar en cuenta que solo se debe usar para iniciar el
procesamiento de errores. Limpiar errores en medio de la compilación, podría hacer que
se pierda el rastro de errores anteriores, y que inclusive, la compilación termine sin
error, aún cuando haya generado errores intermedios.
Como norma, se podría decir que solo se debe usar, después de haber procesado un posible
error anterior.}
begin
  msg.nErrors := 0;
  msg.nInfos := 0;
  msg.nWarns := 0;
end;
function TParser.HayError: boolean;
begin
  exit(msg.nErrors>0);
end;
procedure TParser.GenInfo(txt: string; const srcPos: TSrcPos);
{Genera un mensaje de información, en la posición indicada.}
begin
  msg.info(lex.GetMsgInfo(txt, srcPos));
end;
procedure TParser.GenInfo(txt: string);
{Genera un mensaje de Información, en la posición actual del contexto. }
begin
  msg.info(lex.GetMsgInfo(txt));
end;
procedure TParser.GenWarn(txt: string; const srcPos: TSrcPos);
{Genera un mensaje de advertencia en la posición indicada.}
{ #todo : Considerar usar directamente un parámetro de tipo TMsgInfo}
begin
  msg.warn(lex.GetMsgInfo(txt, srcPos));
end;
procedure TParser.GenWarn(txt: string);
{Genera un mensaje de Advertencia, en la posición actual del contexto. }
begin
  msg.warn(lex.GetMsgInfo(txt));
end;
procedure TParser.GenError(txt: string; const srcPos: TSrcPos);
{Genera un mensaje de error en la posición indicada.}
begin
  msg.error(lex.GetMsgInfoE(txt, srcPos));
end;
procedure TParser.GenError(txt: String; const Args: array of const; const srcPos: TSrcPos);
{Versión con parámetros de GenError.}
begin
  msg.error(lex.GetMsgInfoE(Format(txt, Args), srcPos));
end;
procedure TParser.GenError(txt: string);
{Genera un mensaje de error en la posición actual a la posición del contexto actual.}
begin
  msg.error(lex.GetMsgInfoE(txt));
end;
procedure TParser.GenError(txt: String; const Args: array of const);
{Genera un mensaje de error en la posición actual del contexto.}
begin
  msg.error(lex.GetMsgInfoE(Format(txt, Args)));
end;
{$EndRegion}
{$region "Métodos auxiliares para el parser"}
function TParser.tokIdent: TTokenIdent;
begin
  exit(lex.curCtx.tokIdent);
end;
function TParser.CaptureSemicolon: boolean;
//Verifica si sigue el delimitador de expresión ";". Si no encuentra devuelve false.
begin
  lex.SkipWhites;
  if tokIdent = tiSEMIC then begin //encontró
    Next;   //pasa al siguiente
    exit(true);
  end else begin   //es un error
    GenError('Se esperaba delimitador ";".');
    exit(false);  //sale con error
  end;
end;
procedure TParser.SkipWhites;
{Consume comentarios y directivas del código fuente.
Notar que este procedimiento puede detectar varios errores en el mismo bloque, y que
pasa al siguiente token, aún cuando detecta errores. }
var
  ctxChanged: Boolean;  //Manejamos variables locales para permitir recursividad
begin
  lex.SkipWhites;
  while (lex.tokType = tkDirective) do begin
    //Es una directiva
    callProcDIRline(lex.token, ctxChanged);  //procesa línea
    if HayError then begin
      lex.Next;   //Pasa, porque es un error ya ubicado, y mejor buscamos otros
      lex.SkipWhites;
      continue;
    end;
    if ctxChanged then begin
      {Hubo cambio de contexto. Procesamos nuevamente, porque ahora estamos ya en
      otro contexto y se supone que esta llamada a ProcComments(), se hace precisamente
      para saltar blancos, comentarios, directivas.}
      SkipWhites;   {En el nuevo contexto puede haber nuevos comentarios.}
      exit;
    end;
    //Pasa a siguiente
    lex.Next;
    lex.SkipWhites;  //limpia blancos
  end;
end;
procedure TParser.SkipWhitesNoDirect;
{Similar a SkipWhites(), pero no ejecuta directivas.}
begin
  lex.SkipWhites;
  while (lex.tokType = tkDirective) do begin
    //Pasa a siguiente
    Next;
  end;
end;
procedure TParser.Next;
{Versión de SkipWhites() que primero consume un token con lex.Next. Se evita llamar a
SkipWhites(), y se duplica parte del código,  para evitar la sobrecarga de una llamada
adiconal.}
var
  ctxChanged: Boolean;  //Manejamos variables locales para permitir recursividad
begin
  lex.Next;
  lex.SkipWhites;
  while (lex.tokType = tkDirective) do begin
    //Es una directiva
    callProcDIRline(lex.token, ctxChanged);  //procesa línea
    if HayError then begin
      lex.Next;   //Pasa, porque es un error ya ubicado, y mejor buscamos otros
      lex.SkipWhites;
      continue;
    end;
    if ctxChanged then begin
      {Hubo cambio de contexto. Procesamos nuevamente, porque ahora estamos ya en
      otro contexto y se supone que esta llamada a ProcComments(), se hace precisamente
      para saltar blancos, comentarios, directivas.}
      SkipWhites;   {En el nuevo contexto puede haber nuevos comentarios.}
      exit;
    end;
    //Pasa a siguiente
    lex.Next;
    lex.SkipWhites;  //limpia blancos
  end;
end;
function TParser.ConsumeTok(tokId: TTokenIdent; const msgErr: string): boolean;
{Consume el token identificado por "tokIdent", y pasa al siguiente token saltando
blancos, comentarios o directivas.
Si no encuentra al token "tokIdent", genera el mensaje de error "msgErr", en la posición
en donde se espera encontrar el token y devuelve el valor FALS.}
begin
  if lex.curCtx.tokIdent = tokId then begin
    //Se ecnontró el token buscado
    Next;
    exit(True);
  end else begin
    GenError(msgErr);
    exit(False);
  end;
end;
{$endregion}
{$region "Expresiones"}
function TParser.ParseNumberLiteral: TNumberLiteral;
var
  Value: Integer;
  ValueF: Double;
begin
  if tokIdent = tiLitNumbI then begin
    //Es entero
    Value := StrToInt(lex.token);
    Result := TNumberLiteral.Create(Value, lex.GetSrcPos);
  end else begin
    //Es flotante
    ValueF := StrToFloat(lex.token);
    Result := TNumberLiteral.Create(ValueF, lex.GetSrcPos);
  end;
  Next;
end;
function TParser.ParseIdentifier: TExpression;
{Extrae un identificador, en la posición actual del lexer. El identificador puede ser:
 - Un identificador simple, como "var1" o "func1".
 - Una llamada a función, como "func1()".
 - Un arreglo, como "var_array1[<expresión>]".
 - Un método como "var_base1.func".
Devuelve la referencia a un objeto TExpression. Si se produce un error, devuelve NIL.}
  function ParseModifiers(BaseExpr: TExpression): TExpression;
  {Analiza la parte que sigue después del nombre de un identificador, en busca de
  caracteres epseciales (".", "[" o "^") que modifican el significado del identificador.
  El parámetro "BaseExpr" representa a la expresión base que puede ser la referencia a una
  expresión sencilla como "var1" o puede ser una expresión elaborada como "aaa.bbb[i].ccc"
  Si se produce algún error, devuelve NIL y destruye al objeto "BaseExpr".}
  var
    FieldName: string;
    idxExpr: TExpression;
    FieldAccess: TFieldAccess;
    ArrayAccess: TArrayIndex;
    PointerDeref: TPointerDeref;
  begin
    if tokIdent = tiDOT then begin               //"." -> Campo
      Next;  // Consumir '.'
      if lex.tokType <> tkIdentifier then begin
        GenError('Se esperaba un nombre de campo');
        BaseExpr.Destroy;
        Exit(nil);
      end;
      FieldName := lex.token;
      Next;  // Consumir el nombre del campo
      //Aquí faltaría procesara los parámetros por si el método los incluye.
      // if tokIdent = tiPAREN_OP ... como se hace más abajo
      //
      // Crear el acceso a campo actual
      FieldAccess := TFieldAccess.Create(BaseExpr, FieldName, lex.GetSrcPos);
      //Busca más modificadores del operando
      Result := ParseModifiers(FieldAccess);
    end else if tokIdent = tiBRACK_OP then begin //"[" -> Arreglo
      Next;  // Consumir '['
      //Crea nodo de arreglo a partir de la expresión base
      ArrayAccess := TArrayIndex.Create(BaseExpr, lex.GetSrcPos);
      // Parsear índices
      while not HayError do begin
        idxExpr := ParseExpression;
        if HayError then Break;
        ArrayAccess.AddIndex(idxExpr);
        if tokIdent = tiCOMMA then Next else Break;
      end;
      if HayError then begin
        ArrayAccess.Destroy;  //Destruye también "BaseExpr".
        Exit(nil);
      end;
      if not ConsumeTok(tiBRACK_CL, 'Se esperaba "]"') then begin
        ArrayAccess.Destroy;  //Destruye también "BaseExpr".
        Exit(nil);
      end;
      //Busca más modificadores del operando
      Result := ParseModifiers(ArrayAccess);
    end else if tokIdent = tiPOINTER then begin  //"^" -> Puntero
      Next;  // Consumir '^'
      //Crea nodo de arreglo a partir de la expresión base
      PointerDeref := TPointerDeref.Create(BaseExpr, lex.GetSrcPos);
      //Busca más modificadores del operando
      Result := ParseModifiers(PointerDeref);
    end else begin
      // No hay más niveles, retornar el acceso actual
      Result := BaseExpr;
    end;
  end;
var
  SrcPos: TSrcPos;
  token: String;
  functCall: TFunctionCall;
  BaseExpr: TExpression;
begin
  token := lex.token;       //Guarda nombre del identificador.
  SrcPos := lex.GetSrcPos;  //Guarda posición del identificador.
  Next;  //Pasamos al siguiente token para validar otros casos
  if tokIdent = tiPAREN_OP then begin  // "("
    //Sigue "(", debe ser una llamada a función o procedimiento.
    functCall := TFunctionCall.Create(token, SrcPos);
    // Parsear argumentos
    Next;  //Pasamos el "("
    if tokIdent = tiPAREN_CL then begin  // ")", No hay parámetros
      Next;
    end else begin  //Hay al menos un parámetro
      while true do begin
        //Debe seguir una expresión
        functCall.AddArgument(ParseExpression);
        if HayError then break;
        //Debe seguir "," o ")"
        if tokIdent = tiCOMMA then Next else Break;
      end;
      if HayError then begin
        functCall.Destroy;  //Elimina el objeto no usado
        Exit(nil);
      end;
      if not ConsumeTok(tiPAREN_CL, 'Se esperaba ")"') then begin
        functCall.Destroy;  //Elimina el objeto no usado
        Exit(nil);
      end;
    end;
    BaseExpr := functCall;
  end else begin
    //No sigue "(", entonces debe ser una variable simple, aunque podría ser la llamada
    //a un procedimiento/función. Asumiremos, por ahora, que es una variable.
    BaseExpr := TVariableRef.Create(token, SrcPos);
  end;
  //Busca si hay modificadores del operando ".", "[" o "^".
  Result := ParseModifiers(BaseExpr);
end;
function TParser.ParseStringLiteral: TStringLiteral;
var
  SrcPos: TSrcPos;
begin
  if lex.tokType<> tkString then begin
    GenError('Se esperaba una cadena');
    Exit(nil);
  end;

  SrcPos := lex.GetSrcPos;
  Result := TStringLiteral.Create(lex.token, SrcPos);
  Next;
end;
function TParser.ParseArrayLiteral: TArrayLiteral;
var
  SrcPos: TSrcPos;
  ArrayLit: TArrayLiteral;
  Value: TExpression;
begin
  SrcPos := lex.GetSrcPos;
  Next;  // Consumir '['
  ArrayLit := TArrayLiteral.Create(SrcPos);
  // Verificar si está vacío: []
  if tokIdent = tiBRACK_CL then begin
    Next;  // Consumir ']'
    Result := ArrayLit;
    Exit;
  end;
  // Parsear valores
  while not HayError do begin
    // Verificar si es un array anidado: [1, 2], [3, 4]
    if tokIdent = tiBRACK_OP then begin
      // Es un array anidado (multidimensional)
      Value := ParseArrayLiteral;
      if HayError then begin
        ArrayLit.Free;
        Result := nil;
        Exit;
      end;
      ArrayLit.AddValue(Value);
    end else begin
      // Es un valor simple
      Value := ParseExpression;
      if HayError then begin
        ArrayLit.Free;
        Result := nil;
        Exit;
      end;
      ArrayLit.AddValue(Value);
    end;
    // Verificar si hay más elementos
    if tokIdent = tiCOMMA then
      Next  // Consumir ',' y continuar
    else
      Break;  // No hay más elementos
  end;
  // Verificar cierre ']'
  if tokIdent <> tiBRACK_CL then begin
    GenError('Se esperaba "]" para cerrar el literal.');
    ArrayLit.Free;
    Result := nil;
    Exit;
  end;
  Next;  // Consumir ']'
  Result := ArrayLit;
end;
function TParser.ParseFactor: TExpression;
{Analiza un operando, o factor, que puede ser de diversos tipos.
Si no reconoce al operando, devuelve NIL }
var
  SrcPos: TSrcPos;
  UnaryOp: string;
  Expr: TExpression;
begin
  SrcPos := lex.GetSrcPos;
  //Detecta el operador unario, si existe.
  if tokIdent in [tiPLUS, tiMINUS, tiNOT, tiADDRESS] then begin
    UnaryOp := lex.token;
    Next;   //Consume al operador
    Expr := ParseFactor();  // Recursivo para manejar múltiples signos
    if not HayError then
      Result := TUnaryOp.Create(UnaryOp, Expr, SrcPos)
    else
      Result := nil;
    Exit;
  end;
  //Caso de operando sin signo.
  if lex.tokType = tkLitNumber then begin
    Result := ParseNumberLiteral
  end else if lex.tokType = tkIdentifier then begin
    Result := ParseIdentifier;
  end else if lex.tokType = tkString then begin
    Result := ParseStringLiteral
  end else if tokIdent = tiTRUE then begin
    Next;
    Result := TBooleanLiteral.Create(True, SrcPos);
  end else if tokIdent = tiFALSE then begin
    Next;
    Result := TBooleanLiteral.Create(False, SrcPos);
  end else if tokIdent = tiPAREN_OP then begin  //'('
    Next;
    Result := ParseExpression;
    if not HayError then begin
      if tokIdent <> tiPAREN_CL then begin
        GenError('Se esperaba ")"');
      end;
      Next;
    end;
  end else if tokIdent = tiBRACK_OP then begin
    Result := ParseArrayLiteral;
  end else begin
    GenError('Operando no reconocido', SrcPos);
    Result := nil;
  end;
end;
function TParser.ParseTerm: TExpression;
var
  Left, Right: TExpression;
  Op: string;
  SrcPos: TSrcPos;
begin
  Left := ParseFactor;
  if HayError then Exit(Left);

  // Operadores *, /, div, mod, and
  while tokIdent in [tiMULT, tiDIV, tiIDIV, tiMOD, tiAND] do begin
    if tokIdent in [tiMULT, tiDIV] then begin
      Op := lex.token;
    end else begin
      Op := LowerCase(lex.token);
    end;
    SrcPos := lex.GetSrcPos;
    Next;

    Right := ParseFactor;   //Toma un operando simple, arreglo, puntero, función, ...
    if not HayError then begin
      Left := TBinaryOp.Create(Op, Left, Right, SrcPos);
    end;
  end;

  Result := Left;
end;
function TParser.ParseSimpleExpression: TExpression;
var
  Left, Right: TExpression;
  Op: string;
  SrcPos: TSrcPos;
begin
  // Operador unario opcional
  //if tokIdent in [tiPLUS, tiMINUS] then begin
  //  UnaryOp := lex.token;
  //  SrcPos := lex.GetSrcPos;
  //  lex.Next;
  //  Left := ParseTerm;
  //  if not HayError then
  //    Result := TUnaryOp.Create(UnaryOp, Left, SrcPos)
  //  else
  //    Result := Left;
  //end else begin
    Left := ParseTerm;  //Toma expresiones que sean productos de factores
  //end;

  if HayError then begin
    Exit(Left);
  end;

  // Operadores +, -, or
  while tokIdent in [tiPLUS, tiMINUS, tiOR] do begin
    Op := lex.token;
    SrcPos := lex.GetSrcPos;
    Next;
    Right := ParseTerm;  //Toma expresiones que sean productos de factores
    if not HayError then begin
      Left := TBinaryOp.Create(Op, Left, Right, SrcPos);
    end;
  end;

  Result := Left;
end;
function TParser.ParseExpression: TExpression;
{Analiza una expresión y devuelve un objeto "TExpression" (un árbol sintáctico) que
representa a la expresión analizada.}
var
  Left, Right: TExpression;
  Op: string;
  SrcPos: TSrcPos;
begin
  Left := ParseSimpleExpression;  //Toma expresiones que sean suma de términos.
  if HayError then Exit(Left);
  // Operadores relacionales
  if tokIdent in [tiEQUAL, tiLESS, tiGREAT, tiNOT_EQ, tiLESS_E, tiGREAT_E] then begin
    Op := lex.token;
    SrcPos := lex.GetSrcPos;
    Next;
    Right := ParseSimpleExpression;  //Toma expresiones que sean suma de términos.
    if not HayError then
      Result := TBinaryOp.Create(Op, Left, Right, SrcPos)
    else
      Result := Left;
  end else begin
    Result := Left;
  end;
end;
{$endregion}
{$region "Métodos auxiliares para las declaraciones"}
procedure TParser.ParseParameters(Params: TVarDeclList);
var
  Param: TVarDecl;
  SrcPos: TSrcPos;
  DataTypeName: string;
  i: Integer;
  IsVarParam: Boolean;
begin
  while not HayError do begin
    // Verificar si es parámetro var
    IsVarParam := False;
    if tokIdent = tiVAR then begin
      IsVarParam := True;
      Next;
    end;
    // Leer lista de identificadores
    NamesList.Clear;
    while not HayError do begin
      if lex.tokType <> tkIdentifier then begin
        GenError('Se esperaba un identificador para el parámetro');
        Break;
      end;
      NamesList.Add(lex.token);
      Next;
      if tokIdent = tiCOMMA then begin
        Next;
        if lex.tokType<>tkIdentifier then
          GenError('Se esperaba un identificador después de ","');
      end else
        Break;
    end;
    if HayError then Exit;
    // Verificar el tipo
    if tokIdent <> tiCOLON then begin
      GenError('Se esperaba ":" después de los parámetros');
      Exit;
    end;
    Next;
    if lex.tokType <> tkIdentifier then begin
      GenError('Se esperaba un tipo de dato');
      Exit;
    end;
    DataTypeName := lex.token;
    Next;
    SrcPos := lex.GetSrcPos;
    // Crear parámetros
    for i := 0 to NamesList.Count - 1 do begin
      Param := TVarDecl.Create(NamesList[i], DataTypeName, SrcPos);
      Param.IsParameter := True;
      Param.IsByReference := IsVarParam;
      Params.Add(Param);
    end;
    // Verificar si hay más parámetros
    if tokIdent = tiSEMIC then begin
      Next;
      Continue;
    end else begin
      Break;
    end;
  end;
end;
function TParser.ParseSubrangeType: TSubrangeTypeDef;
var
  LowExpr, HighExpr: TExpression;
begin
  LowExpr := ParseExpression;
  if HayError then begin
    Result := nil;
    Exit;
  end;
  if tokIdent <> tiDOTDOT then begin
    GenError('Se esperaba ".." en el subrango');
    LowExpr.Free;
    Result := nil;
    Exit;
  end;
  Next;
  HighExpr := ParseExpression;
  if HayError then begin
    LowExpr.Free;
    Result := nil;
    Exit;
  end;
  Result := TSubrangeTypeDef.Create(LowExpr, HighExpr, lex.GetSrcPos);
end;
function TParser.ParseEnumType: TEnumTypeDef;
var
  EnumType: TEnumTypeDef;
begin
  Next;     //Consume "("
  EnumType := TEnumTypeDef.Create(lex.GetSrcPos);
  while not HayError do begin
    if lex.tokType <> tkIdentifier then begin
      GenError('Se esperaba un identificador en el enumerado');
      EnumType.Free;
      Result := nil;
      Exit;
    end;
    EnumType.AddValue(lex.token);
    Next;
    if tokIdent = tiCOMMA then
      Next
    else
      Break;
  end;
  if tokIdent <> tiPAREN_CL then begin
    GenError('Se esperaba ")" para cerrar el enumerado');
    EnumType.Free;
    Result := nil;
    Exit;
  end;
  Next;
  Result := EnumType;
end;
function TParser.ParseArrayTypeDef: TArrayTypeDef;
var
  ArrayType: TArrayTypeDef;
  LowExpr, HighExpr: TExpression;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  ArrayType := TArrayTypeDef.Create(SrcPos);
  Next;     //Consume ARRAY
  if not ConsumeTok(tiBRACK_OP, 'Se esperaba "[" después de "array"') then begin
    ArrayType.Free;
    Exit(nil);
  end;
  while not HayError do begin
    LowExpr := ParseExpression;
    if HayError then Break;
    if tokIdent <> tiDOTDOT then begin
      GenError('Se esperaba ".." en el rango del arreglo');
      LowExpr.Free;
      ArrayType.Free;
      Exit(nil);
    end;
    Next;
    HighExpr := ParseExpression;
    if HayError then begin
      LowExpr.Free;
      ArrayType.Free;
      Exit(nil);
    end;
    ArrayType.AddRange(TArrayRange.Create(LowExpr, HighExpr, SrcPos));
    if tokIdent = tiCOMMA then
      Next
    else
      Break;
  end;
  if not ConsumeTok(tiBRACK_CL, 'Se esperaba "]"') then begin
    ArrayType.Free;
    Exit(nil);
  end;
  if not ConsumeTok(tiOF, 'Se esperaba "of"') then begin
    ArrayType.Free;
    Exit(nil);
  end;
  // Parsear el tipo de los elementos (puede ser cualquier tipo)
  // Aquí llamamos a ParseTypeDefinition recursivamente
  // Pero cuidado: podría causar recursión infinita con tipos mutuamente referenciados
  // Para simplificar, leemos el nombre del tipo o una definición inline
  if lex.tokType = tkIdentifier then begin
    ArrayType.ElementTypeName := lex.token;
    Next;
  end else begin
    // Definición inline (ej: array[1..10] of record ... end)
    ArrayType.ElementTypeDef := ParseTypeDefinition;
    if HayError then begin
      ArrayType.Free;
      Exit(nil);
    end;
  end;
  Result := ArrayType;
end;
function TParser.ParseRecordTypeDef: TRecordTypeDef;
var
  RecordType: TRecordTypeDef;
  Field: TFieldDef;
  SrcPos: TSrcPos;
  i: Integer;
begin
  SrcPos := lex.GetSrcPos;
  RecordType := TRecordTypeDef.Create(SrcPos);
  Next;  //Toma el token "RECORD"
  while not (HayError or (tokIdent = tiEND)) do begin
    NamesList.Clear;
    while not HayError do begin
      if lex.tokType <> tkIdentifier then begin
        GenError('Se esperaba un identificador para el campo');
        Break;
      end;
      NamesList.Add(lex.token);
      Next;
      if tokIdent = tiCOMMA then
        Next
      else
        Break;
    end;
    if HayError then Break;
    if tokIdent <> tiCOLON then begin
      GenError('Se esperaba ":" después del nombre del campo');
      Break;
    end;
    Next;  //Dejamos al lexer apuntando al tipo: string, array[], ...
    SrcPos := lex.GetSrcPos;
    // Crear los campos
    for i := 0 to NamesList.Count - 1 do begin
      Field := TFieldDef.Create(NamesList[i], SrcPos);
      // Parsear el tipo del campo
      if lex.tokType = tkIdentifier then begin
        Field.TypeName := lex.token;
        //Si estamos en el último ítem de la lista, tomamos el nombre del tipo, y así
        //estamos listo para leer el siguiente campo del RECORD.
        if i = NamesList.Count - 1 then Next;
      end else begin
        // Definición inline (ej: record ... end dentro de un campo)
        Field.TypeDef := ParseTypeDefinition;  //*** Esto fallará si son varios campos: "a,b,c: ARRAY[1..3] OF char" porque no se puede parsear la misma definición de tipo varias veces.
        if HayError then begin
          Field.Free;
          RecordType.Free;
          Exit(nil);
        end;
      end;
      RecordType.AddField(Field);
    end;
    if tokIdent = tiSEMIC then
      Next
    else
      Break;
  end;
  if HayError then begin
    RecordType.Free;
    Exit(nil);
  end;
  if not ConsumeTok(tiEND, 'Se esperaba "end"') then begin
    RecordType.Free;
    Exit(nil);
  end;
  Result := RecordType;
end;
function TParser.ParsePointerType: TPointerTypeDef;
var
  TargetTypeName: string;
begin
  Next;   //Consume "^"
  if lex.tokType <> tkIdentifier then begin
    GenError('Se esperaba el tipo al que apunta el puntero');
    Result := nil;
    Exit;
  end;
  TargetTypeName := lex.token;
  Next;
  Result := TPointerTypeDef.Create(TargetTypeName, lex.GetSrcPos);
end;
function TParser.ParseTypeDefinition: TTypeDef;
var
  SrcPos: TSrcPos;
  TypeName: string;
begin
  SrcPos := lex.GetSrcPos;
  // 1. Alias: = integer, byte, TPersona, etc.
  if lex.tokType = tkIdentifier then begin
    TypeName := lex.token;
    Next;
    Result := TAliasTypeDef.Create(TypeName, SrcPos);
    Exit;
  end;
  // 2. Subrango:  = 1..10, 'a'..'z'
  if lex.tokType in [tkLitNumber, tkString] then begin
    Result := ParseSubrangeType;
    Exit;
  end;
  // 3. Enumerado: = (Rojo, Verde, Azul)
  if tokIdent = tiPAREN_OP then begin
    Result := ParseEnumType;
    Exit;
  end;
  // 4. Arreglo: = array[1..10] of integer
  if tokIdent = tiARRAY then begin
    Result := ParseArrayTypeDef;
    Exit;
  end;
  // 5. Registro: = record ... end
  if tokIdent = tiRECORD then begin
    Result := ParseRecordTypeDef;
    Exit;
  end;
  // 6. Puntero: = ^integer
  if tokIdent = tiPOINTER then begin
    Result := ParsePointerType;
    Exit;
  end;
  GenError('Definición de tipo no reconocida', SrcPos);
  Result := nil;
end;
{$endregion}
{$region "Declaraciones"}
procedure TParser.ParseVarDeclaration(declars: TDeclarations);
  procedure ReadNamesList;
  //Lee una lista de identificadores en la lista "NamesList".
  begin
    NamesList.Clear;
    // Leer lista de identificadores
    while not HayError do begin
      if lex.tokType<>tkIdentifier then begin
        GenError('Se esperaba un identificador');
        Break;
      end;
      NamesList.Add(lex.token);
      Next;
      // Verificar si hay más variables
      if tokIdent = tiCOMMA then begin
        Next;  // Consumir coma
        if lex.tokType<>tkIdentifier then
          GenError('Se esperaba un identificador después de ","');
        // Continuar con la siguiente variable
      end else
        Break;  // No hay más variables en esta línea
    end;
  end;
var
  SrcPos: TSrcPos;
  DataTypeName: string;
  i: Integer;
begin
  Next;  //Consume VAR
  repeat
    //Lee un bloque de declaraciones: VAR a, b, c: byte;
    ReadNamesList;
    if HayError then Exit;

    if not ConsumeTok(tiCOLON, 'Se esperaba ":" después de las variables') then Exit;

    // Leer el tipo
    if lex.tokType<>tkIdentifier then begin
      GenError('Se esperaba un tipo de dato');
      Exit;
    end;
    DataTypeName := lex.token;
    // Crear declaraciones para cada variable
    SrcPos := lex.GetSrcPos;   //Usa una sola ubicación
    for i := 0 to NamesList.Count - 1 do begin
      declars.Add(TVarDecl.Create(NamesList[i], DataTypeName, SrcPos));
    end;
    Next;  //Pasa el nombre del tipo
    if tokIdent = tiSEMIC then begin
      // Consumir ';' opcional
      Next;
    end else begin
      //Puede que siga otro tipo de declaración o sea un error.
      Break;
    end;
  until lex.tokType = tkKeyword;  //Sige otra declaración o BEGIN
end;
procedure TParser.ParseConstDeclaration(declars: TDeclarations);
var
  ConstName: string;
  ConstValue: TExpression;
  SrcPos: TSrcPos;
begin
  Next;  //Pasa al siguiente token.
  // Parsear constantes hasta que se acaben
  while not HayError do begin
    // Leer nombre de la constante
    if lex.tokType <> tkIdentifier then begin
      // Si no hay más identificadores, es porque terminaron las constantes
      Break;
    end;
    SrcPos := lex.GetSrcPos;
    ConstName := lex.token;
    Next;  // Consumir el nombre
    // Verificar '='
    if tokIdent <> tiEQUAL then begin
      GenError('Se esperaba "=" en la declaración de constante', SrcPos);
      Break;
    end;
    Next;  // Consumir '='
    // Parsear el valor de la constante (puede ser cualquier expresión constante)
    ConstValue := ParseExpression;
    if HayError then begin
      ConstValue.Free;
      Break;
    end;
    // Crear la declaración de constante
    declars.Add(TConstDecl.Create(ConstName, ConstValue, SrcPos));
    // Consumir ';' opcional
    if tokIdent = tiSEMIC then
      Next
    else
      Break;  // Si no hay ';', asumimos que terminaron las constantes
  end;
end;
procedure TParser.ParseProcedureDeclaration(declars: TDeclarations);
{Analiza la declaración de un procedimiento.}
var
  Proc: TProcDecl;
begin
  Next;  // Consume PROCEDURE
  if lex.tokType <> tkIdentifier then begin
    GenError('Se esperaba un identificador para el procedimiento');
    Exit;
  end;
  Proc := TProcDecl.Create(lex.token, lex.GetSrcPos);
  Next;
  // Parsear parámetros
  if tokIdent = tiPAREN_OP then begin   //"("
    Next;
    ParseParameters(Proc.Parameters);
    if not HayError then begin
      if tokIdent <> tiPAREN_CL then  //")"
        GenError('Se esperaba ")" después de los parámetros');
      Next;
    end;
  end;
  if HayError then begin
    Proc.Destroy;
    Exit;
  end;
  if not ConsumeTok(tiSEMIC, 'Se esperaba ";".') then begin
    Proc.Destroy;
    Exit;
  end;
  //Procesar declaraciones
  ParseDeclarations(Proc.Declarations);
  if HayError then begin
    Proc.Destroy;
    Exit;
  end;
  //Parsear cuerpo
  ParseBody(Proc.Body);
  if HayError then begin
    Proc.Destroy;
  end else begin
    declars.Add(Proc);
  end;
end;
procedure TParser.ParseFunctionDeclaration(declars: TDeclarations);
var
  Func: TFunctDecl;
begin
  Next;  // Consume FUNCTION
  if lex.tokType <> tkIdentifier then begin
    GenError('Se esperaba un identificador para la función');
    Exit;
  end;
  Func := TFunctDecl.Create(lex.token, lex.GetSrcPos);
  Next;
  // Parsear parámetros
  if tokIdent = tiPAREN_OP then begin   //'('
    Next;
    ParseParameters(Func.Parameters);
    if not HayError then begin
      if tokIdent <> tiPAREN_CL then  //")"
        GenError('Se esperaba ")" después de los parámetros');
      Next;
    end;
  end;
  if HayError then begin
    Func.Destroy;
    Exit;
  end;
  if tokIdent <> tiCOLON  then begin  //":"
    GenError('Se esperaba ":" después del nombre');
    Func.Destroy;
    Exit;
  end;
  Next;
  //Lee tipo devuelto
  if lex.tokType <> tkIdentifier then begin
    GenError('Se esperaba el tipo de retorno');
    Func.Destroy;
    Exit;
  end;
  Func.ReturnTypeName := lex.token;
  Next;
  if not ConsumeTok(tiSEMIC, 'Se esperaba ";".') then begin
    Func.Destroy;
    Exit;
  end;
  //Procesar declaraciones
  ParseDeclarations(Func.Declarations);
  if HayError then begin
    Func.Destroy;
    Exit;
  end;
  //Parsear cuerpo
  ParseBody(Func.Body);
  if HayError then begin
    Func.Destroy;
  end else begin
    declars.Add(Func);
  end;
end;
procedure TParser.ParseTypeDeclaration(declars: TDeclarations);
var
  TypeName: string;
  TypeDef: TTypeDef;
begin
  Next;  //Consume TYPE
  while not HayError do begin
    if lex.tokType <> tkIdentifier then
      Break;  // No hay más declaraciones de tipo
    TypeName := lex.token;
    Next;
    if tokIdent <> tiEQUAL then begin
      GenError('Se esperaba "=" en la definición del tipo');
      Break;
    end;
    Next;
    TypeDef := ParseTypeDefinition;
    if HayError then begin
      TypeDef.Free;
      Break;
    end;
    TypeDef.TypeName := TypeName;
    declars.Add(TypeDef);        //Agrega la declaración
    if tokIdent = tiSEMIC then  // ";"
      Next
    else
      Break;
  end;
end;
{$endregion}
{$region "Instrucciones"}
procedure TParser.ParseAssigOrProcedureCall(var Block: TBlock);
var
  Operand1, Value: TExpression;
begin
  Operand1 := ParseIdentifier;
  if HayError then begin
    if Operand1<> Nil then Operand1.Destroy;
    Exit;
  end;
  if tokIdent = tiASSIGN then begin
    //Se trata de una asignación.
    Next;  //Pasamos el ":="
    Value := ParseExpression;
    if HayError then begin
      Operand1.Destroy;
      if Value<>nil then Value.Destroy;
    end else begin
      //Target := TVariableRef.Create(token, SrcPos);
      Block.AddStatement(TAssignment.Create(Operand1, Value, Operand1.SrcPos));
    end;
  end else if tokIdent in [tiSEMIC, tiELSE, tiEND] then begin
    //Sigue un delimitador de instrucción ";", "else" o "end". Debe ser una llamada a
    //procedimiento o función.
    Block.AddStatement(Operand1);
  end else begin
    GenError('Se esperaba ":=" o ";".', lex.GetSrcPos);
    Operand1.Destroy;
    Exit;
  end;
end;
procedure TParser.ParseIfStatement(var Block: TBlock);
var
  Condition: TExpression;
  ThenBranch, ElseBranch: TBlock;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  if not ConsumeTok(tiIF, 'Se esperaba "if"') then Exit;
  Condition := ParseExpression;
  if HayError then Exit;
  if not ConsumeTok(tiTHEN, 'Se esperaba "then"') then Exit;
  // Rama THEN
  ThenBranch := TBlock.Create(lex.GetSrcPos);
  ParseStatement(ThenBranch);
  if HayError then Exit;
  // Rama Else opcional)
  if tokIdent = tiELSE then begin
    Next;
    ElseBranch := TBlock.Create(lex.GetSrcPos);
    ParseStatement(ElseBranch);
  end else begin
    ElseBranch := nil;
  end;
  if not HayError then begin
    Block.AddStatement(TIfStatement.Create(Condition, ThenBranch, ElseBranch, SrcPos));
  end;
end;
procedure TParser.ParseWhileLoop(var Block: TBlock);
var
  Condition: TExpression;
  Body: TBlock;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  if not ConsumeTok(tiWHILE, 'Se esperaba "while"') then Exit;

  Condition := ParseExpression;

  if HayError then Exit;

  if not ConsumeTok(tiDO, 'Se esperaba "do"') then Exit;

  Body := TBlock.Create(lex.GetSrcPos);
  ParseStatement(Body);

  if not HayError then
    Block.AddStatement(TWhileLoop.Create(Condition, Body, SrcPos));
end;
procedure TParser.ParseForLoop(var Block: TBlock);
var
  ControlVar: TVariableRef;
  Direction: TForDirection;
  StartExpr, EndExpr: TExpression;
  Body: TBlock;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  if not ConsumeTok(tiFOR, 'Se esperaba "for"') then Exit;

  if lex.tokType <> tkIdentifier then begin
    GenError('Se esperaba una variable de control');
    Exit;
  end;

  ControlVar := TVariableRef.Create(lex.token, lex.GetSrcPos);
  Next;

  if tokIdent <> tiASSIGN then begin
    GenError('Se esperaba ":=" en el bucle FOR');
    Exit;
  end;
  Next;

  StartExpr := ParseExpression;

  if HayError then Exit;

  if tokIdent = tiTO  then begin
    Direction := fdUpTo;
    Next;
  end else if tokIdent = tiDOWNTO then begin
    Direction := fdDownTo;
    Next;
  end else begin
    GenError('Se esperaba "to" o "downto" en el bucle FOR');
    Exit;
  end;

  EndExpr := ParseExpression;

  if HayError then Exit;

  if not ConsumeTok(tiDO, 'Se esperaba "do"') then Exit;

  Body := TBlock.Create(lex.GetSrcPos);
  ParseStatement(Body);

  if not HayError then
    Block.AddStatement(TForLoop.Create(ControlVar, Direction, StartExpr, EndExpr, Body, SrcPos));
end;
procedure TParser.ParseRepeatUntil(var Block: TBlock);
var
  Body: TBlock;
  Condition: TExpression;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  if not ConsumeTok(tiREPEAT, 'Se esperaba "repeat"') then Exit;

  Body := TBlock.Create(lex.GetSrcPos);

  // Parsear instrucciones hasta encontrar 'until'
  while not (HayError or (tokIdent = tiUNTIL)) do
    ParseStatement(Body);

  if HayError then Exit;

  if not ConsumeTok(tiUNTIL, 'Se esperaba "until"') then Exit;

  Condition := ParseExpression;

  if not HayError then
    Block.AddStatement(TRepeatUntil.Create(Body, Condition, SrcPos));
end;
function TParser.ParseCaseBranch: TCaseBranch;
var
  Branch: TCaseBranch;
  SrcPos: TSrcPos;
  Expr: TExpression;
  LowExpr, HighExpr: TExpression;
begin
  SrcPos := lex.GetSrcPos;
  Branch := TCaseBranch.Create(SrcPos);
  //Leer lista de constantes / rangos: 1, 3, 5..10, 15
  while not HayError do begin
    // CASO 1: Rango: 1..10
    if tokIdent = tiDOTDOT then begin
      // Rango sin límite inferior explícito? (error)
      GenError('Se esperaba un valor antes de ".."');
      Branch.Free;
      Result := nil;
      Exit;
    end;
    // Parsear el límite inferior
    LowExpr := ParseExpression;
    if HayError then begin
      LowExpr.Free;
      Branch.Free;
      Result := nil;
      Exit;
    end;
    // Verificar si es un rango: 1..10
    if tokIdent = tiDOTDOT then begin
      Next;  // Consumir '..'
      // Parsear el límite superior
      HighExpr := ParseExpression;
      if HayError then begin
        LowExpr.Free;
        HighExpr.Free;
        Branch.Free;
        Result := nil;
        Exit;
      end;
      // Crear un rango como una expresión binaria especial
      // O podemos crear un nodo específico para rangos
      // Por ahora, usamos TBinaryOp con operador '..'
      Expr := TBinaryOp.Create('..', LowExpr, HighExpr, SrcPos);
      Branch.AddConstant(Expr);
    end else begin
      // Es una constante individual
      Branch.AddConstant(LowExpr);
    end;
    // Verificar si hay más elementos en la lista
    if tokIdent = tiCOMMA then
      Next  // Consumir coma y continuar
    else
      Break;  // No hay más elementos
  end;
  if HayError then begin
    Branch.Free;
    Result := nil;
    Exit;
  end;
  // Verificar ':'
  if tokIdent <> tiCOLON then begin
    GenError('Se esperaba ":"');
    Branch.Free;
    Result := nil;
    Exit;
  end;
  Next;  // Consumir ':'

  // Parsear la instrucción
  Branch.Statement := TBlock.Create(lex.GetSrcPos);
  ParseStatement(Branch.Statement);
  if HayError then begin
    Branch.Free;
    Result := nil;
    Exit;
  end;
  Result := Branch;
end;
procedure TParser.ParseCaseStatement(var Block: TBlock);
var
  Selector: TExpression;
  CaseStmt: TCaseStatement;
  Branch: TCaseBranch;
  ElseBlock: TBlock;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  Next;  //Consume "CASE"
  Selector := ParseExpression;
  if HayError then begin
    Selector.Free;
    Exit;
  end;
  if not ConsumeTok(tiOF, 'Se esperaba "of"') then begin
    Selector.Free;
    Exit;
  end;
  CaseStmt := TCaseStatement.Create(Selector, SrcPos);
  // Parsear ramas normales
  while not (HayError or (tokIdent = tiEND) or (tokIdent = tiELSE)) do begin
    Branch := ParseCaseBranch;
    if not HayError then
      CaseStmt.AddBranch(Branch)
    else
      Branch.Free;
  end;
  if HayError then begin
    CaseStmt.Free;
    Exit;
  end;
  // Parsear ELSE (opcional)
  if tokIdent = tiELSE then begin
    Next;  // Consumir 'else'
    ElseBlock := TBlock.Create(lex.GetSrcPos);
    ParseStatement(ElseBlock);
    if not HayError then
      CaseStmt.ElseBranch := ElseBlock
    else
      ElseBlock.Free;
  end;
  if HayError then begin
    CaseStmt.Free;
    Exit;
  end;
  // Verificar END
  if not ConsumeTok(tiEND, 'Se esperaba "end"') then begin
    CaseStmt.Free;
    Exit;
  end;
  Block.AddStatement(CaseStmt);
end;
procedure TParser.ParseWithStatement(var Block: TBlock);
var
  SrcPos: TSrcPos;
  RecordVar: TExpression;
  Body: TBlock;
begin
  SrcPos := lex.GetSrcPos;
  if not ConsumeTok(tiWITH, 'Se esperaba "with"') then Exit;
  // Parsear la variable registro (puede ser compuesta)
  // Ejemplos: persona, persona.nombre, personas[i], empleado.persona
  RecordVar := ParseExpression;
  if HayError then begin
    RecordVar.Free;
    Exit;
  end;
  // Verificar 'do'
  if not ConsumeTok(tiDO, 'Se esperaba "do" después de "with"') then begin
    RecordVar.Free;
    Exit;
  end;
  // Parsear el cuerpo
  Body := TBlock.Create(lex.GetSrcPos);
  ParseStatement(Body);
  if HayError then begin
    RecordVar.Free;
    Body.Free;
    Exit;
  end;
  Block.AddStatement(TWithStatement.Create(RecordVar, Body, SrcPos));
end;
procedure TParser.ParseExitStatement(var Block: TBlock);
var
  SrcPos: TSrcPos;
  ReturnValue: TExpression;
  ExitStmt: TExitStatement;
begin
  SrcPos := lex.GetSrcPos;
  if not ConsumeTok(tiEXIT, 'Se esperaba "exit"') then Exit;
  // Verificar si hay valor de retorno entre paréntesis
  if tokIdent = tiPAREN_OP then begin
    // Exit(5); → función con valor de retorno
    Next;  // Consumir '('
    ReturnValue := ParseExpression;
    if HayError then begin
      ReturnValue.Free;
      Exit;
    end;
    if not ConsumeTok(tiPAREN_CL, 'Se esperaba ")"') then begin
      ReturnValue.Free;
      Exit;
    end;
    ExitStmt := TExitStatement.Create(ReturnValue, SrcPos);
  end else begin
    // Exit; → procedimiento o función sin valor
    ExitStmt := TExitStatement.Create(SrcPos);
  end;
  Block.AddStatement(ExitStmt);
end;
{$endregion}
{$region "Sentencia, bloque y programa"}
procedure TParser.ParseStatement(Body: TBlock);
begin
  if Body = nil then begin
    Body := TBlock.Create(lex.GetSrcPos);
  end;
  // Identificar el tipo de instrucción
  if tokIdent = tiEXIT then begin
    //Se valida primero porque "exit" es también un identificador.
    ParseExitStatement(Body)
  end else if lex.tokType = tkIdentifier then begin
    //Puede ser una asignación o una llamada a procedimiento.
    ParseAssigOrProcedureCall(Body);
  end else if tokIdent = tiIF then begin
    ParseIfStatement(Body)
  end else if tokIdent = tiWHILE then begin
    ParseWhileLoop(Body)
  end else if tokIdent = tiFOR then begin
    ParseForLoop(Body)
  end else if tokIdent = tiREPEAT then begin
    ParseRepeatUntil(Body)
  end else if tokIdent = tiCASE then begin
    ParseCaseStatement(Body)
  end else if tokIdent = tiWITH then begin
    ParseWithStatement(Body)
  end else if tokIdent = tiBEGIN then begin
    // Bloque anidado - se convierte en parte del bloque actual
    ParseBody(Body);
  end else if tokIdent = tiSEMIC then begin
    // Instrucción vacía
    Next;
  end else begin
    GenError('Instrucción no reconocida', lex.GetSrcPos);
  end;
  if HayError then Exit;

  // Opcional: ; después de la instrucción
  if tokIdent = tiSEMIC then begin
    Next;
  end;
end;
procedure TParser.ParseDeclarations(Declars: TDeclarations);
begin
  while not HayError do begin
    if tokIdent = tiVAR then
      ParseVarDeclaration(Declars)
    else if tokIdent = tiCONST then
      ParseConstDeclaration(Declars)
    else if tokIdent = tiPROCED then
      ParseProcedureDeclaration(Declars)
    else if tokIdent = tiFUNCT  then
      ParseFunctionDeclaration(Declars)
    else if tokIdent = tiTYPE then
      ParseTypeDeclaration(Declars)
    else
      Break;  // No hay más declaraciones
  end;
end;
procedure TParser.ParseBody(Body: TBlock);
begin
  if tokIdent<>tiBEGIN then begin
    GenError('Se esperaba "begin"');
    exit;
  end;
  Next;
  // Parsear instrucciones hasta 'end'
  while not (HayError or (tokIdent=tiEND)) do begin
    ParseStatement(Body);
  end;
  // end
  if tokIdent<>tiEND then begin
    GenError('Se esperaba "end"');
  end;
  Next;   //Toma el "End".
  if not HayError then begin
    // Si hay ';' después de "End" (opcional en Pascal).
    if tokIdent = tiSEMIC then begin
      Next;
    end;
  end;
end;
procedure TParser.ParseProgram;
{Realiza en análisis sintáctico de un programa y construye el AST.
El lexer debe haber sido cargado previamente con el código fuente del programa, y el AST
debe haber sido limpiado}
  procedure ParseProgramHeader;
  begin
    //Captura el encabezado, solo si existe.
    if tokIdent = tiPROGRAM then begin
      Next;  //pasa al nombre
      if lex.atEof then begin
        GenError('Program name expected.');
        exit;
      end;
      ast.Name := lex.token;
      ast.srcDec := lex.GetSrcPos;
      Next;  //Toma el nombre y pasa al siguiente
      if not CaptureSemicolon then exit;
    end;
    if lex.atEof then begin
      GenError('Expected "program", "begin", "var", "type" or "const".');
      exit;
    end;
  end;
begin
  // program <nombre> ;
  SkipWhites;
  ParseProgramHeader;
  if HayError then Exit;
  // Analizar las declaraciones
  ParseDeclarations(ast.Declarations);
  if HayError then Exit;

  // Analizar el cuerpo principal
  ParseBody(ast.Body);
  if HayError then Exit;

  // Consumir el punto final
  if tokIdent<>tiDOT then
     GenError('Se esperaba "." al final del programa');
  Next;
  // Verificar que no queden tokens.
  if not HayError then begin
    if not lex.atEof then
      GenError('Código extra después del final del programa');
  end;
end;
{$endregion}
{$region "Inicialización"}
procedure TParser.Clear;
begin
  ClearError;
  ast.Clear;
end;
constructor TParser.Create(msg0: TMessageManager);
begin
  //inherited;
  lex := TAleLexer.Create(msg0);
  msg := msg0;
  ast := TProgram.Create('test', lex.GetSrcPos);
  NamesList := TStringList.Create;
  ClearError;   //inicia motor de errores
end;
destructor TParser.Destroy;
begin
  NamesList.Destroy;
  ast.Destroy;
  lex.Destroy;
  inherited Destroy;
end;
{$endregion}
end.
