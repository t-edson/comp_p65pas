{ParserPas
Clase para la creación de un analizador sintáctico en Pascal.
Todas las rutinas definidas aquí son independientes de la de CPU, a excepción del AST que
incluye un tipo de nodo que representa a los bloques ensamblador.
}
//{$Define LogExpres}
unit ParserPas;
interface
uses
  Classes, SysUtils, LazLogger, alexiaLex, ASTunit;
type  //Tipos generales
//Primary location for elements
{Current location for scan. This tells the compiler where it's scanning. It useful because
some declarations have to be interpreted in different ways according to the location.}
TElemLocation = (
              locMain,       //En el programa principal.
              locInterface,  //En INTERFACE de una unidad.
              locImplement   //En IMPLEMENTATION de una unidad.
);

type  //TParserPas
{Clase que implementa al analizador sintáctico (Parser).}
TParserPas = class
private
  lex    : TAleLexer;       //Analizador léxico
  msg    : TMessageManager; //Referencia al gestor de mensajes
  function ParseVariableBlockDeclar(varContainer: TASTNodeList;
    paramType: TParamType): byte;
public    //Componentes principales del compilador
  astProg: TProgram;        //Árbol de sintaxis abstracto de un programa
  astUnit: TUnit;           //Árbol de sintaxis abstracto de una unidad
private   //Messages
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
public    //Eventos
  {Se separa en eventos las llamadas a módulos separados del Parser que son muy
  dependientes del hardware.}
  //LLamada para procesar directivas
  callProcDIRline  : procedure(const AsmLin: string; out ctxChanged: boolean) of object;
  //Llamada para procesar bloques ASM
  callParseASMblock: procedure(Body: TBlock) of object;
  //Llamada para procesar parámetros adicionales declaración de variables
  callParseAdicVarDec: function(varDecl: TVarDecl): boolean of object;
protected // Métodos auxiliares para el parser
  function tokIdent: TTokenIdent; inline;
  function ConsumeSemicolon: boolean;
  procedure SkipWhites;
  procedure SkipWhitesNoDirect;
  procedure Next;
  function ConsumeTok(tokId: TTokenIdent; const msgErr: string): boolean; inline;
  function ConsumeIdent(out token: string; const msgErr: string): boolean; inline;
private   // Expresiones
  function ParseNumberLiteral: TNumberLiteral;
  function ParseIdentifier: TExpression;
  function ParseStringLiteral: TStringLiteral;
  function ParseArrayLiteral: TArrayLiteral;
  function ParseRecordLiteral(firstIdent: string): TRecordLiteral;
  function ParseFactor: TExpression;
  function ParseTerm: TExpression;
  function ParseSimpleExpression: TExpression;
  function ParseExpression(AllowFormat: Boolean = False): TExpression;
private   // Métodos auxiliares para las declaraciones
  procedure ParseParameters(var Params: TASTNodeList);
  function ParseSubrangeType: TSubrangeTypeDef;
  function ParseEnumType: TEnumTypeDef;
  function ParseArrayTypeDef: TArrayTypeDef;
  function ParseRecordTypeDef: TRecordTypeDef;
  function ParsePointerType: TPointerTypeDef;
  function ParseProceduralType: TProceduralType;
  function ParseTypeDefinition: TTypeDef;
private   // Declaraciones
  procedure ParseUsesClause(const unitContainer: TUnitRefList);
  procedure ParseVarDeclaration(declars: TDeclarations);
  procedure ParseConstDeclaration(declars: TDeclarations);
  procedure ParseProcedureDeclaration(declars: TDeclarations);
  procedure ParseTypeDeclaration(declars: TDeclarations);
private   // Instrucciones
  procedure ParseAssigOrProcedureCall(var Block: TBlock);
  procedure ParseIfStatement(var Block: TBlock);
  procedure ParseWhileLoop(var Block: TBlock);
  procedure ParseForLoop(var Block: TBlock);
  procedure ParseRepeatUntil(var Block: TBlock);
  procedure ParseCaseSelector(constants: TExpressionList);
  function ParseCaseBranch: TCaseBranch;
  procedure ParseCaseStatement(var Block: TBlock);
  procedure ParseWithStatement(var Block: TBlock);
  procedure ParseExitStatement(var Block: TBlock);
public    // Sentencia, bloque y programa
  procedure ParseStatement(Body: TBlock);
  procedure ParseDeclarations(Declars: TDeclarations);
  procedure ParseBody(Body: TBlock);
  procedure ParseProgramHeader;
  procedure ParseProgram;
  procedure ParseUnit;
public    // Inicialización
  function GetUnitDeclaration: boolean;
  procedure Clear;  // Reinicia el compilador para un nuevo programa
  constructor Create(msg0: TMessageManager; lex0: TAleLexer);
  destructor Destroy; override;
end;

implementation
{$region "Messages"}
procedure TParserPas.ClearError;
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
function TParserPas.HayError: boolean;
begin
  exit(msg.nErrors>0);
end;
procedure TParserPas.GenInfo(txt: string; const srcPos: TSrcPos);
{Genera un mensaje de información, en la posición indicada.}
begin
  msg.info(lex.GetMsgInfo(txt, srcPos));
end;
procedure TParserPas.GenInfo(txt: string);
{Genera un mensaje de Información, en la posición actual del contexto. }
begin
  msg.info(lex.GetMsgInfo(txt));
end;
procedure TParserPas.GenWarn(txt: string; const srcPos: TSrcPos);
{Genera un mensaje de advertencia en la posición indicada.}
{ #todo : Considerar usar directamente un parámetro de tipo TMsgInfo}
begin
  msg.warn(lex.GetMsgInfo(txt, srcPos));
end;
procedure TParserPas.GenWarn(txt: string);
{Genera un mensaje de Advertencia, en la posición actual del contexto. }
begin
  msg.warn(lex.GetMsgInfo(txt));
end;
procedure TParserPas.GenError(txt: string; const srcPos: TSrcPos);
{Genera un mensaje de error en la posición indicada.}
begin
  msg.error(lex.GetMsgInfoE(txt, srcPos));
end;
procedure TParserPas.GenError(txt: String; const Args: array of const; const srcPos: TSrcPos);
{Versión con parámetros de GenError.}
begin
  msg.error(lex.GetMsgInfoE(Format(txt, Args), srcPos));
end;
procedure TParserPas.GenError(txt: string);
{Genera un mensaje de error en la posición actual a la posición del contexto actual.}
begin
  msg.error(lex.GetMsgInfoE(txt));
end;
procedure TParserPas.GenError(txt: String; const Args: array of const);
{Genera un mensaje de error en la posición actual del contexto.}
begin
  msg.error(lex.GetMsgInfoE(Format(txt, Args)));
end;
{$EndRegion}
{$region "Métodos auxiliares para el parser"}
function TParserPas.tokIdent: TTokenIdent;
begin
  exit(lex.curCtx.tokIdent);
end;
procedure TParserPas.SkipWhites;
{Consume comentarios y directivas del código fuente.
Notar que este procedimiento puede detectar varios errores en el mismo bloque, y que
pasa al siguiente token, aún cuando detecta errores. }
var
  ctxChanged: Boolean;  //Manejamos variables locales para permitir recursividad
begin
  lex.SkipWhites;
  while (lex.curCtx.tokType = tkDirective) do begin
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
procedure TParserPas.SkipWhitesNoDirect;
{Similar a SkipWhites(), pero no ejecuta directivas.}
begin
  lex.SkipWhites;
  while (lex.curCtx.tokType = tkDirective) do begin
    //Pasa a siguiente
    Next;
  end;
end;
procedure TParserPas.Next;
{Versión de SkipWhites() que primero consume un token con lex.Next. Se evita llamar a
SkipWhites(), y se duplica parte del código,  para evitar la sobrecarga de una llamada
adiconal.}
var
  ctxChanged: Boolean;  //Manejamos variables locales para permitir recursividad
begin
  lex.Next;
  lex.SkipWhites;
  while (lex.curCtx.tokType = tkDirective) do begin
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
function TParserPas.ConsumeSemicolon: boolean;
//Verifica si sigue el delimitador de expresión ";". Si no encuentra devuelve false.
begin
  if lex.curCtx.tokIdent = tiSEMIC then begin //encontró
    Next;   //pasa al siguiente
    exit(true);
  end else begin   //es un error
    GenError('Se esperaba ";".');
    exit(false);  //sale con error
  end;
end;
function TParserPas.ConsumeTok(tokId: TTokenIdent; const msgErr: string): boolean;
{Consume el token identificado por "tokIdent", y pasa al siguiente token saltando
blancos, comentarios o directivas.
Si no encuentra al token "tokIdent", genera el mensaje de error "msgErr", en la posición
actual y devuelve el valor FALSE.}
begin
  if lex.curCtx.tokIdent = tokId then begin
    //Se encontró el token buscado
    Next;
    exit(True);
  end else begin
    GenError(msgErr);
    exit(False);
  end;
end;
function TParserPas.ConsumeIdent(out token: string; const msgErr: string): boolean;
{Consume un token de tipo identificador, lo devuelve en "token", y pasa al siguiente
token saltando blancos, comentarios o directivas.
Si no encuentra un identificador, genera el mensaje de error "msgErr", en la posición
actual y devuelve el valor FALS.}
begin
  if lex.curCtx.tokIdent = tiIDENTIF then begin
    //Se encontró un identificador
    token := lex.token;
    Next;
    exit(True);
  end else begin
    GenError(msgErr);
    exit(False);
  end;
end;
{$endregion}
{$region "Expresiones"}
function TParserPas.ParseNumberLiteral: TNumberLiteral;
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
function TParserPas.ParseIdentifier: TExpression;
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
      if not ConsumeIdent(FieldName, 'Se esperaba un nombre de campo') then begin
        BaseExpr.Destroy;
        Exit(nil);
      end;
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
        idxExpr := ParseSimpleExpression;
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
        functCall.AddArgument(ParseExpression(True));  //Permitimos formato
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
function TParserPas.ParseStringLiteral: TStringLiteral;
var
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  Result := TStringLiteral.Create(lex.token, SrcPos);
  Next;
end;
function TParserPas.ParseArrayLiteral: TArrayLiteral;
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
    Value := ParseExpression;
    if HayError then begin
      ArrayLit.Free;
      Exit(Nil);
    end;
    ArrayLit.AddValue(Value);
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
    Exit(Nil);
  end;
  Next;  // Consumir ']'
  Result := ArrayLit;
end;
function TParserPas.ParseRecordLiteral(firstIdent: string): TRecordLiteral;
{Analiza un literal de registro (RECORD), de la forma: "(a: valor; b: valor)" y devuelve
un objeto TRecordLiteral.
Si encuentra algún errror, devuelve NIL.}
var
  RecordLit: TRecordLiteral;
  FieldName: string;
  Value: TExpression;
begin
  FieldName := firstIdent;   //Ya se leyó el primer identificador
  RecordLit := TRecordLiteral.Create(lex.GetSrcPos);
  // Parsear inicializadores de campos
  while not HayError do begin
    // Verificar ':'
    if not ConsumeTok(tiCOLON, 'Se esperaba ":" después del nombre del campo') then begin
      RecordLit.Destroy;
      Exit(Nil);
    end;
    // Parsear el valor
    Value := ParseExpression;
    if HayError then begin
      RecordLit.Destroy;
      Exit(Nil);
    end;
    // Crear inicializador
    RecordLit.AddInitializer(TFieldInitializer.Create(FieldName, Value, lex.GetSrcPos));
    if tokIdent = tiPAREN_CL then Break;   //No hay más campos
    if tokIdent = tiSEMIC then begin
      Next;       //Hay más campos
      // Leer nombre del campo
      if not ConsumeIdent(FieldName, 'Se esperaba un nombre de campo') then begin;
        RecordLit.Destroy;
        Exit(Nil);
      end;
    end else begin
      Break; //Sigue otra cosa. Debe ser un error.
    end;
  end;
  // Verificar cierre ')'
  if tokIdent <> tiPAREN_CL then begin
    GenError('Se esperaba ")" para cerrar el literal de registro');
    RecordLit.Free;
    Exit(Nil);
  end;
  Next;  // Consumir ')'
  Result := RecordLit;
end;
function TParserPas.ParseFactor: TExpression;
{Analiza un operando, o factor, que puede ser de diversos tipos.
Si no reconoce al operando, devuelve NIL }
var
  SrcPos: TSrcPos;
  UnaryOp, firstIdent: string;
  Expr, Value: TExpression;
  ArrayLit: TArrayLiteral;
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
  if tokIdent in [tiLitNumbI, tiLitNumbF] then begin
    Result := ParseNumberLiteral
  end else if tokIdent = tiIDENTIF then begin
    Result := ParseIdentifier;
  end else if tokIdent = tiLitString then begin
    Result := ParseStringLiteral
  end else if tokIdent = tiTRUE then begin
    Next;
    Result := TBooleanLiteral.Create(True, SrcPos);
  end else if tokIdent = tiFALSE then begin
    Next;
    Result := TBooleanLiteral.Create(False, SrcPos);
  end else if tokIdent = tiPAREN_OP then begin  //'('
    {Puede ser una expresión entre paréntesis, un literal de arreglo (válido para
    inicializar constantes arreglo) o un literal de registro (válido para inicializar
    constantes registro).}
    Next;
    Result := ParseExpression;  //Asumimos una expresión simple:
    if HayError then Exit;
    if tokIdent = tiCOMMA then begin
      //Se debe tratar de un literal de arreglo.
      Next;
      ArrayLit := TArrayLiteral.Create(SrcPos);
      ArrayLit.AddValue(Result);    //Agregamos el primer elemento
      while not HayError do begin
        Value := ParseExpression;
        if HayError then begin
          ArrayLit.Free;
          Exit(nil);
        end;
        ArrayLit.AddValue(Value);
        // Verificar si hay más elementos
        if tokIdent = tiCOMMA then Next else Break;
      end;
      if tokIdent <> tiPAREN_CL then begin
        GenError('Se esperaba ")" para cerrar el literal.');
        ArrayLit.Free;
        Exit(Nil);
      end;
      Next;  // Consumir ')'
      Exit(ArrayLit);   //Devuelve el arreglo
    end else if (Result.NodeType = ntVariableRef) and  //Es una forma de detectar que la expresión es solo un identificador
                (tokIdent = tiCOLON) then begin  // Sigue ":". Debe ser literal RECORD.
      firstIdent := TVariableRef(Result).Name;  //Debe ser un identificador.
      Result.Destroy;  //Ya no nos sirve esta expresión.
      Exit(ParseRecordLiteral(firstIdent));  //Puede devolver NIL, si hay error.
    end;
    if tokIdent <> tiPAREN_CL then begin
      GenError('Se esperaba ")"');
    end;
    Next;
  end else if tokIdent = tiBRACK_OP then begin
    Result := ParseArrayLiteral;
  end else if tokIdent = tiNIL then begin
    Next;
    Result := TPointerLiteral.Create(SrcPos);   //Crea NIL
  end else begin
    GenError('Operando no reconocido', SrcPos);
    Result := nil;
  end;
end;
function TParserPas.ParseTerm: TExpression;
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
function TParserPas.ParseSimpleExpression: TExpression;
var
  Left, Right: TExpression;
  Op: string;
  SrcPos: TSrcPos;
begin
  Left := ParseTerm;  //Toma expresiones que sean productos de factores
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
function TParserPas.ParseExpression(AllowFormat: Boolean = False): TExpression;
{Analiza una expresión y devuelve un objeto "TExpression" (un árbol sintáctico) que
representa a la expresión analizada.}
var
  Left, Right: TExpression;
  Op: string;
  SrcPos: TSrcPos;
begin
  Left := ParseSimpleExpression;  //Toma expresiones que sean suma de términos.
  if HayError then Exit(Left);  //En este caso deberái devolver NIL.
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
  //Valida si hay parámetros de formato
  if AllowFormat and (tokIdent = tiCOLON) then begin
    Next;  // Consumir ':'
    // Parsear el ancho (debe ser un número)
    if tokIdent <> tiLitNumbI then begin
      GenError('Se esperaba un número entero para el ancho del formato');
      Result.Free;
      Exit(Nil);
    end;
    Result.FormatWidth := StrToInt(lex.token);
    Next;  // Consumir el número
    // Verificar si hay decimales: :decimales
    if tokIdent = tiCOLON then begin
      Next;  // Consumir ':'
      if tokIdent <> tiLitNumbI then begin
        GenError('Se esperaba un número entero para los decimales del formato');
        Result.Free;
        Exit(Nil);
      end;
      Result.FormatDecimals := StrToInt(lex.token);
      Next;  // Consumir el número
    end;
  end;
end;
{$endregion}
{$region "Métodos auxiliares para las declaraciones"}
function TParserPas.ParseVariableBlockDeclar(varContainer: TASTNodeList;
                                             paramType: TParamType): byte;
{Analiza una declaracíon de tipo:
  a, b, c: tipo_simple;
o también:
  a, b, c: <declaración de tipo estructurado>
Conforme va reconociendo los ítems, va creando las variables correspondientes en la lista
"varContainer".
Devuelve la cantidad de variables procesadas en el bloque.}
var
  i, idxVarIni: Integer;
  typeDef: TTypeDef;
  varDecl: TVarDecl;
begin
  //Explora la lista de identificadores y crea las variables.
  idxVarIni := varContainer.Count;  //Guardamos el índice de la primera variable.
  repeat
    if tokIdent <> tiIDENTIF then begin
      GenError('Se esperaba un identificador.');
      Exit(0);
    end;
    //Hay un identificador. Vamos creando la variable.
    varDecl := TVarDecl.Create(lex.token, lex.GetSrcPos);
    varDecl.ParamType := paramType;   //Define el tipo de parámetro
    varContainer.Add(varDecl);   //La agregamos
    Next;
    if tokIdent <> tiCOMMA then Break; //Se asume que termina la lista de identificadores.
    Next;  //Toma la coma
  until false;
  if not ConsumeTok(tiCOLON, 'Se esperaba ":" después de la variable(s).') then
    Exit(0);   //No es necesario limpiar nada adicional
  //Lee el tipo y completa esa información en las variables creadas.
  if tokIdent  = tiIDENTIF then begin  //Debe ser un tipo simple: byte, mi_tipo, ...
    //Actualiza el tipo en todas las variables creadas.
    for i := idxVarIni to varContainer.Count-1 do begin
      //Todos estos ítems deben ser los que hemos agregados
      varDecl := TVarDecl(varContainer[i]);   //Todas deben ser TVarDecl
      varDecl.TypeName := lex.token;  //Es tipo simple
      //varDecl.TypeDef := Nil;  //No es necesario actualizar
    end;
    Next;   //Consume el identificador de tipo
  end else begin //Debe ser una definición Inline: record ... end
    typeDef := ParseTypeDefinition;
    if HayError then begin
      typeDef.Free; //Por si acaso
      Exit(0);
    end;
    //Actualiza el tipo en todas las variables creadas, haciendo que todas las variables
    //creadas en un solo bloque, apunten al mismo tipo definido "typeDef".
    for i := idxVarIni to varContainer.Count-1 do begin
      //Todos estos ítems deben ser los que hemos agregados
      varDecl := TVarDecl(varContainer[i]);   //Todas deben ser TVarDecl
      //varDecl.TypeName := '';   //No es necesario actualizar
      varDecl.TypeDef := typeDef;  //No es tipo estructurado o anónimo.
      if i = idxVarIni then begin
        //Ponemos, como propietario del tipo, solo a la primera declaración, para evitar
        //que varios objetos intenten destruirlo.
        varDecl.TypeOwner := true
      end;
    end;
  end;
  //Devuelve la cantidad de variables agregadas
  Exit(varContainer.Count - idxVarIni);
end;
procedure TParserPas.ParseParameters(var Params: TASTNodeList);
{Lee parámetros de un procedimiento o función en la lista "Params", que debe ser solo una
referencia a TVarDeclList, pero sin instanciar.
Si se encuentra al menos un parámetro, se crea la lista "Params" y se le agregan los
parámetros.
Si no se encuentran parámetros, se devuelve NIL en "Params".
Si se encuentra algún error, se libera "Params" (si se creó) y se pone a NIL.}
var
  paramType: TParamType;
begin
  Params := nil;
  if tokIdent <> tiPAREN_OP then Exit;   //"("
  Next;       //Consume "(".
  if tokIdent = tiPAREN_CL then begin
    Next;
    Exit;
  end;
  while not HayError do begin
    // Verificar si es parámetro var
    if tokIdent = tiVAR then begin       //VAR
      paramType := ptyVar;
      Next;
    end else if tokIdent = tiCONST then begin //CONST
      paramType := ptyConst;
      Next;
    end else if tokIdent = tiOUT then begin   //OUT
      paramType := ptyOut;
      Next;
    end;
    // Leer identificadores y tipo
    if Params = nil then Params:= TASTNodeList.Create(true);
    ParseVariableBlockDeclar(Params, paramType);
    if HayError then Break;
    // Verificar si hay más parámetros
    if tokIdent = tiSEMIC then begin
      Next;
      Continue;
    end else begin
      Break;
    end;
  end;
  //Ya no se encuentran más parámetros o hay error.
  if HayError then begin
    FreeAndNil(Params);
  end else begin
    if not ConsumeTok(tiPAREN_CL, 'Se esperaba ")" después de los parámetros') then begin
      FreeAndNil(Params);
    end;
  end;
end;
function TParserPas.ParseSubrangeType: TSubrangeTypeDef;
var
  LowExpr, HighExpr: TExpression;
begin
  LowExpr := ParseFactor;
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
  HighExpr := ParseFactor;
  if HayError then begin
    LowExpr.Free;
    Result := nil;
    Exit;
  end;
  Result := TSubrangeTypeDef.Create(LowExpr, HighExpr, lex.GetSrcPos);
end;
function TParserPas.ParseEnumType: TEnumTypeDef;
var
  EnumType: TEnumTypeDef;
  enumName: string;
begin
  Next;     //Consume "("
  EnumType := TEnumTypeDef.Create(lex.GetSrcPos);
  while not HayError do begin
    if not ConsumeIdent(enumName, 'Se esperaba un identificador en el enumerado') then begin
      EnumType.Free;
      Exit(Nil);
    end;
    EnumType.AddValue(enumName);
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
function TParserPas.ParseArrayTypeDef: TArrayTypeDef;
{Procesa la declaración de tipos arreglo de la forma:
ARRAY [<valor_ini>..<valor_fin>] OF <tipo>;
Opcionalmente, y aunque no es estándar en Pascal, se acepta también la forma:
[<valor_ini>..<valor_fin>] OF <tipo>;
}
var
  ArrayType: TArrayTypeDef;
  LowExpr, HighExpr: TExpression;
begin
  ArrayType := TArrayTypeDef.Create(lex.GetSrcPos);
  if tokIdent = tiARRAY then Next;     //Consume ARRAY, pero se acepta también que vaya "["
  if tokIdent = tiBRACK_OP then begin    //Es un arreglo estático: ARRAY[1..3] OF ...
    Next;     //Consume "[".
    while not HayError do begin
      LowExpr := ParseFactor;
      if HayError then Break;
      if tokIdent = tiDOTDOT then begin
        Next;
        HighExpr := ParseFactor;
        if HayError then begin
          LowExpr.Free;
          ArrayType.Free;
          Exit(nil);
        end;
        ArrayType.AddRange(TArrayRange.Create(LowExpr, HighExpr, lex.GetSrcPos));
      end else if tokIdent = tiBRACK_CL then begin
        //Es una definición corta: ARRAY[5] OF ...
        HighExpr := Nil;   //Indica de esta forma que es la forma simplificada
        //LowExpr := TNumberLiteral.Create(0, lex.GetSrcPos);   //Índice menor = 0
        ArrayType.AddRange(TArrayRange.Create(LowExpr, HighExpr, lex.GetSrcPos));
      end else begin   //Sigue otra cosa
        GenError('Se esperaba ".." o "]" en el rango del arreglo');
        LowExpr.Free;
        ArrayType.Free;
        Exit(nil);
      end;
      if tokIdent = tiCOMMA then Next else Break;   //Valida si sigue otra dimensión
    end;
    if not ConsumeTok(tiBRACK_CL, 'Se esperaba "]"') then begin
      ArrayType.Free;
      Exit(nil);
    end;
  end;
  //if not ConsumeTok(tiOF, 'Se esperaba "of"') then begin
  //  ArrayType.Free;
  //  Exit(nil);
  //end;
  if tokIdent = tiOF then Next;   //Es opcional en P65Pas
  //Lee tipo de los elementos (puede ser cualquier tipo)
  if tokIdent = tiIDENTIF then begin
    ArrayType.ElementTypeName := lex.token;
    Next;
  end else begin
    // Definición Inline: array[1..10] of record ... end)
    ArrayType.ElementTypeDef := ParseTypeDefinition;  //Llamada recursiva
    if HayError then begin
      ArrayType.Free;
      Exit(nil);
    end;
  end;
  Result := ArrayType;
end;
function TParserPas.ParseRecordTypeDef: TRecordTypeDef;
{Analiza la definición de un tipo RECORD y devuelve un objeto "TRecordTypeDef" con la
estructura del tipo analizado.
Si se encuentra algún error, se devuelve NIL.}
  procedure ParseVariantBlock(RecordType: TRecordTypeDef);
  {Analiza la parte variante de una declaración RECORD.}
  var
    selectorName: string;
    varDecl: TVarDecl;
    branch: TVariantBranch;
    SrcPos: TSrcPos;
    typeDef: TTypeDef;
  begin
    //Es la sección variante del RECORD
    Next;  //Consume RECORD
    //Analizamos la sintaxis
    SrcPos := lex.GetSrcPos;
    if not ConsumeIdent(selectorName, 'Se esperaba un identificador.') then Exit;
    if not ConsumeTok(tiCOLON, 'Se esperaba ":".') then Exit;
    //Leemos el tipo
    if tokIdent = tiIDENTIF then begin  //Debe ser un tipo simple: byte, mi_tipo, ...
      //Creamos la variable selector con su tipo.
      varDecl := TVarDecl.Create(selectorName, SrcPos);
      varDecl.TypeName := lex.token;  //No pemitiremos tipos complejos aquí
      Next;
    end else begin //Debe ser una definición Inline: record ... end
      typeDef := ParseTypeDefinition;
      if HayError then begin
        typeDef.Free; //Por si acaso
        Exit;
      end;
      varDecl := TVarDecl.Create(selectorName, SrcPos);
      varDecl.TypeDef := typeDef;  //No es tipo estructurado o anónimo.
      varDecl.TypeOwner := true;   //Es el propietario del tipo
    end;
    if not ConsumeTok(tiOf, 'Se esperaba "of".') then Exit;
    RecordType.VarSelector := varDecl;  //De la detrucción de "varDecl" se encargará RecordType.
    //Analizamos las ramas
    RecordType.Branches := TVariantBranchList.Create(True);  //Creamos contenedor
    while not HayError and (tokIdent <> tiEND) do begin
      branch := TVariantBranch.Create(lex.GetSrcPos);
      //Analizamos el selector:
      ParseCaseSelector(branch.SelectorValues);
      if HayError then begin
        branch.Destroy;
        Exit;
      end;
      //Analizamos los campos
      if not ConsumeTok(tiPAREN_OP, 'Se esperaba "(".') then begin
        branch.Destroy;
        Exit;
      end;
      ParseVariableBlockDeclar(branch.Fields, ptyNone);   //Por ahora, solo soportamos un bloque de campos
      if not ConsumeTok(tiPAREN_CL, 'Se esperaba ")".') then begin
        branch.Destroy;
        Exit;
      end;
      if not ConsumeSemicolon then begin
        branch.Destroy;
        Exit;
      end;
      RecordType.Branches.Add(branch);  //FInalmente, agregamos la rama
    end;
  end;
var
  RecordType: TRecordTypeDef;
begin
  //Creamos el tipo. La posición TSrcPos no es relevante ahora.
  RecordType := TRecordTypeDef.Create(lex.GetSrcPos);
  Next;  //Toma el token "RECORD"
  //Explora los campos y los agrega a "RecordType".
  while not (HayError or (tokIdent = tiEND)) do begin
    if tokIdent = tiCASE then begin   //Es la parte variante (CASE) de un RECORD.
      ParseVariantBlock(RecordType);
      if HayError then begin
        RecordType.Free;
        Exit(nil);
      end;
      Break;  //Ya no debe seguir nada después de la parte variante.
    end else begin
      ParseVariableBlockDeclar(RecordType.Fields, ptyNone);
    end;
    if tokIdent = tiSEMIC then begin  //Es ";"
      Next;   //Tomamos ";" y seguimos explorando
    end else begin      //Sigue otra cosa
      Break;   //Asumimos que aquí terminan los campos
    end;
  end;
  if HayError then begin
    RecordType.Free;
    Exit(nil);
  end;
  if not ConsumeTok(tiEND, 'Se esperaba "end"') then begin
    RecordType.Free;
    Exit(nil);
  end;
  Exit(RecordType);
end;
function TParserPas.ParsePointerType: TPointerTypeDef;
var
  TargetTypeName: string;
begin
  Next;   //Consume "^"
  if not ConsumeIdent(TargetTypeName, 'Se esperaba el tipo al que apunta el puntero') then begin
    Exit(Nil);
  end;
  Result := TPointerTypeDef.Create(TargetTypeName, lex.GetSrcPos);
end;
function TParserPas.ParseProceduralType: TProceduralType;
var
  ProcType: TProceduralType;
  IsFunction: Boolean;
begin
  // Verificar si es function
  if tokIdent = tiFUNCT then begin
    IsFunction := True;
  end else begin
    IsFunction := False;
  end;
  ProcType := TProceduralType.Create(IsFunction, lex.GetSrcPos);
  Next;  //Consumir 'procedure' o 'function'.
  // Parsear parámetros (si hay paréntesis)
  ParseParameters(ProcType.Parameters);
  if HayError then begin
    ProcType.Free;
    Exit(Nil);
  end;
  // Si es función, parsear el tipo de retorno
  if IsFunction then begin
    if not ConsumeTok(tiCOLON, 'Se esperaba ":" para el tipo de retorno') then begin
      ProcType.Free;
      Exit(Nil);
    end;
    if not ConsumeIdent(ProcType.ReturnTypeName, 'Se esperaba el tipo de retorno') then begin
      GenError('Se esperaba el tipo de retorno');
      ProcType.Free;
      Exit(Nil);
    end;
  end;
  Result := ProcType;
end;
function TParserPas.ParseTypeDefinition: TTypeDef;
{Analiza la definición de un tipo, simple o estructurado. Devuelve un nodo "TTypeDef" con
la estructura del tipo analizado.}
var
  SrcPos: TSrcPos;
  TypeName: string;
begin
  SrcPos := lex.GetSrcPos;
  case tokIdent of
    tiIDENTIF: begin         //Alias: = integer, byte, TPersona, etc.
      TypeName := lex.token;
      Next;
      Result := TAliasTypeDef.Create(TypeName, SrcPos);
    end;
    tiLitNumbI: begin        //Subrango:  = 1..10
      Result := ParseSubrangeType;
    end;
    tiLitString: begin       //Subrango:  = 'a'..'z'
      Result := ParseSubrangeType;
    end;
    tiPAREN_OP: begin        //Enumerado: = (Rojo, Verde, Azul)
      Result := ParseEnumType;
    end;
    tiARRAY, tiBRACK_OP: begin //Arreglo: = array[1..10] of integer
      Result := ParseArrayTypeDef;
    end;
    tiRECORD: begin          //Registro: = record ... end
      Result := ParseRecordTypeDef;
    end;
    tiOBJECT: begin          //Objeto : = object ... end
      Result := ParseRecordTypeDef;   //Por ahora es similar a los registros
    end;
    tiPOINTER: begin         //Puntero: = ^integer
      Result := ParsePointerType;
    end;
    tiPROCED, tiFUNCT: begin //TipProc := PROCEDURE();
      Result := ParseProceduralType;
    end;
  else
    GenError('Definición de tipo no reconocida', SrcPos);
    Result := nil;
  end;
end;
{$endregion}
{$region "Declaraciones"}
procedure TParserPas.ParseUsesClause(const unitContainer: TUnitRefList);
var
  untName: string;  //Nombre de la unidad.
begin
  if not ConsumeTok(tiUSES, 'Se esperaba "uses"') then Exit;
  // Parsear lista de unidades separadas por comas
  while not HayError do begin
    //Lee nombre de la unidad
    if not ConsumeIdent(untName, 'Se esperaba un nombre de unidad') then Break;
    // Añadir la unidad al programa
    unitContainer.Add(TUnitRef.Create(untName, lex.GetSrcPos));
    // Verificar si hay más unidades
    if tokIdent = tiCOMMA then
      Next   //Consumir ',' y continuar
    else
      Break; //No hay más unidades
  end;
  if HayError then Exit;
  // Verificar ';' después de la lista
  if tokIdent <> tiSEMIC then
    GenError('Se esperaba ";" después de la sección USES');
  Next;  //Consumir ';'
end;
procedure TParserPas.ParseVarDeclaration(declars: TDeclarations);
{Analiza la sección de declaración de variables. Esta sección puede incluri varios
bloques de variables:
VAR
  a, b, c: Byte;  //Bloque 1
  d, e: word;     //Bloque 2
}
var
  nvars: Byte;    //Número de variables declaradas en un bloque
  varDecl: TVarDecl;
begin
  Next;  //Consume VAR
  repeat
    nvars := ParseVariableBlockDeclar(declars.Items, ptyNone);
    if HayError then Exit;
    //Puede seguir un modificador de declaración
    if not(tokIdent in [tiSEMIC, tiEQUAL]) then begin
      if nvars>1 then begin
        GenError('No se puede aplicar este modificador a más de una variable.');
        Exit;
      end;
      //Hay una sola variable declarada
      varDecl := TVarDecl(declars.Items[declars.Items.Count-1]);  //La variable
      //Procesa modificadores ABSOLUTE, REGISTER, ...
      if tokIdent in [tiABSOLUTE, tiADDRESS] then begin
        // Hay especificación de dirección absoluta
        Next;
        varDecl.hasAdic := DEC_ABSOL;    //marca bandera
        varDecl.absAddr := ParseSimpleExpression;  //Leemos expresión de dirección
        if HayError then exit;
      end else begin
        //No es ABSOLUTE, debe ser un modificador adicional
        if not callParseAdicVarDec(varDecl) then begin
          Exit;  //Hubo error
        end;
      end;
    end;
    //Puede seguir una sección de inicialización: var: char = 'A';
    if tokIdent = tiEQUAL then begin  //"="
      Next;   //Toma "="
      if nvars>1 then begin
        GenError('No se puede inicializar a más de una variable.');
        Exit;
      end;
      //Hay una sola variable declarada
      varDecl := TVarDecl(declars.Items[declars.Items.Count-1]);  //La variable
      //Aquí debe seguir el valor inicial constante.
      varDecl.initVal := ParseExpression(False);
      if HayError then Exit;
    end;
    if not ConsumeSemicolon then Exit;   //Debe terminar con ";".
  until tokIdent<>tiIDENTIF;     //Sige otra declaración o BEGIN
end;
procedure TParserPas.ParseConstDeclaration(declars: TDeclarations);
var
  ConstName, TypeName: string;
  ConstValue: TExpression;
  SrcPos: TSrcPos;
  ConstDecl: TConstDecl;
  TypeDef: TTypeDef;
begin
  Next;  //Pasa al siguiente token.
  // Parsear constantes hasta que se acaben
  while not HayError do begin
    // Leer nombre de la constante
    if tokIdent <> tiIDENTIF then begin
      // Si no hay más identificadores, es porque terminaron las constantes
      Break;
    end;
    SrcPos := lex.GetSrcPos;
    ConstName := lex.token;
    Next;  // Consumir el nombre
    if tokIdent = tiCOLON then begin    //Constante con tipo
      Next;  // Consume ':'
      // Leer el tipo
      TypeDef := ParseTypeDefinition;
      TypeName := 'Tipo';
      //Continua con la asignación del valor.
      if not ConsumeTok(tiEQUAL, 'Se esperaba "=" en la declaración.') then Break;
      // Parsear el valor de la constante
      ConstValue := ParseExpression;  //Puede ser cualquier expresión constante
      if HayError then begin
        ConstValue.Free;
        Break;
      end;
      //Crea la declaración de constante y la agrega
      ConstDecl := TConstDecl.Create(ConstName, TypeName, TypeDef, ConstValue, SrcPos);
      declars.Add(ConstDecl);
    end else if tokIdent = tiEQUAL then begin  //Constante simple
      Next;  // Consume '='
      // Parsear el valor de la constante
      ConstValue := ParseExpression;  //Puede ser cualquier expresión constante
      if HayError then begin
        ConstValue.Free;
        Break;
      end;
      //Crea la declaración de constante y la agrega
      ConstDecl := TConstDecl.Create(ConstName, ConstValue, SrcPos);
      declars.Add(ConstDecl);
    end else begin
      GenError('Se esperaba "=" o ":" en la declaración.');
      Break;
    end;
    //Consumimos ';', y generamos el error si se omite.
    ConsumeSemicolon;
  end;
end;
procedure TParserPas.ParseProcedureDeclaration(declars: TDeclarations);
{Realiza el análisis de un procedimiento o función.}
var
  Proc: TProcDecl;
  SrcPos: TSrcPos;
  procName, returnType: string;
  Params: TASTNodeList;
  isFunction, IsAssembler: Boolean;
begin
  SrcPos := lex.GetSrcPos;  //Posición donde empieza el proc/función.
  isFunction := (tokIdent = tiFUNCT);   //Identifica
  Next;  // Consume PROCEDURE o FUNCTION
  if not ConsumeIdent(procName, 'Se esperaba un identificador.') then Exit;
  // Parsear parámetros
  ParseParameters(Params);
  if HayError then Exit;
  //Lee tipo devuelto si es una función
  if isFunction then begin
      if not ConsumeTok(tiCOLON, 'Se esperaba ":" después del nombre') then Exit;
      if not ConsumeIdent(returnType, 'Se esperaba el tipo de retorno.') then Exit;
  end;
  if not ConsumeTok(tiSEMIC, 'Se esperaba ";".') then Exit;
  //Verifica ASSEMBLER
  IsAssembler := False;
  if tokIdent = tiASSEMBLER then begin
    Next;     //Consume
    if not ConsumeTok(tiSEMIC, 'Se esperaba ";".') then Exit;
    IsAssembler := True;
  end;
  if tokIdent = tiFORWARD then begin      //Es declaración FORWARD
    Next;
    if not ConsumeTok(tiSEMIC, 'Se esperaba ";".') then Exit;
    Proc := TProcDecl.Create(procName, SrcPos, True);
    Proc.Parameters := Params;  //Puede ser NIL.
    Proc.ReturnTypeName := returnType;
    Proc.IsAssembler := IsAssembler;
    declars.Add(Proc);
  end else begin
    //Es declaración con cuerpo.
    Proc := TProcDecl.Create(procName, SrcPos, False);
    Proc.Parameters := Params;  //Puede ser NIL.
    Proc.ReturnTypeName := returnType;
    if IsAssembler or (tokIdent=tiASM) then begin
       //Es proc/función ASSEMBLER.
      if tokIdent <> tiASM then begin
        GenError('Se esperaba "ASM".');
        Proc.Destroy;
        Exit;
      end;
      Proc.IsAssembler := true;
      //Procesa el bloque ASM
      callParseASMblock(Proc.Body);  //Procesa el único bloque ASM permitido.
      if HayError then begin
        Proc.Destroy;
        Exit;
      end;
      if Not ConsumeSemicolon then begin
        Proc.Destroy;
        Exit;
      end;
      declars.Add(Proc);
    end else begin
      //Es proc/función normal.
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
  end;
end;
procedure TParserPas.ParseTypeDeclaration(declars: TDeclarations);
var
  TypeName: string;
  TypeDef: TTypeDef;
begin
  Next;  //Consume TYPE
  while not HayError do begin
    if tokIdent <> tiIDENTIF then
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
procedure TParserPas.ParseAssigOrProcedureCall(var Block: TBlock);
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
procedure TParserPas.ParseIfStatement(var Block: TBlock);
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
procedure TParserPas.ParseWhileLoop(var Block: TBlock);
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
procedure TParserPas.ParseForLoop(var Block: TBlock);
var
  ControlVar: TVariableRef;
  Direction: TForDirection;
  StartExpr, EndExpr: TExpression;
  Body: TBlock;
  SrcPos: TSrcPos;
  ControlVarName: string;
  forLoop: TForLoop;
begin
  if not ConsumeTok(tiFOR, 'Se esperaba "for"') then Exit;
  SrcPos := lex.GetSrcPos;
  if not ConsumeIdent(ControlVarName, 'Se esperaba una variable de control') then Exit;
  if not ConsumeTok(tiASSIGN, 'Se esperaba ":=" en el bucle FOR') then Exit;
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
    StartExpr.Free;
    Exit;
  end;
  EndExpr := ParseExpression;
  if HayError then begin
    StartExpr.Free;
    Exit;
  end;
  if not ConsumeTok(tiDO, 'Se esperaba "do"') then Exit;
  ControlVar := TVariableRef.Create(ControlVarName, SrcPos);
  Body := TBlock.Create(lex.GetSrcPos);
  ParseStatement(Body);
  if HayError then begin
    StartExpr.Free;
    EndExpr.Free;
    ControlVar.Destroy;
    Body.Destroy;
    Exit;
  end;
  //No hay error
  forLoop := TForLoop.Create(ControlVar, Direction, StartExpr, EndExpr, Body, SrcPos);
  Block.AddStatement(forLoop);
end;
procedure TParserPas.ParseRepeatUntil(var Block: TBlock);
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
procedure TParserPas.ParseCaseSelector(constants: TExpressionList);
{Analiza la sección del selector de una sentencia CASE, y actualiza la lista "constants"
con los valores del selector.
El selector puede ser:
- Una constante como 1 o 'A'. En este caso, se devuelve una expresion constante en la
  lista "constants".
- Un rango como 5..10 o 'a'..'z'. En este caso, se devuelve una operación binaria ".." en
  la lista "constants".
- La unión de los casos anteriores: 1, 3, 5..10, 15. En este caso, se devuelve una
  expresion, por cada caso encontrado, en la lista "constants".
}
var
  LowExpr, HighExpr: TExpression;
  Expr: TBinaryOp;
begin
  while not HayError do begin
    // Parsear el límite inferior
    LowExpr := ParseFactor;
    if HayError then begin
      LowExpr.Free;
      Exit;
    end;
    // Verificar si es un rango: 1..10
    if tokIdent = tiDOTDOT then begin
      Next;  // Consumir '..'
      // Parsear el límite superior
      HighExpr := ParseFactor;
      if HayError then begin
        LowExpr.Free;
        HighExpr.Free;
        Exit;
      end;
      //Por ahora, al rango lo representamos como una expresión TBinaryOp con operador '..'
      Expr := TBinaryOp.Create('..', LowExpr, HighExpr, lex.GetSrcPos);
      constants.Add(Expr);
    end else begin
      // Es una constante individual
      constants.Add(LowExpr);
    end;
    // Verificar si hay más elementos en la lista
    if tokIdent = tiCOMMA then
      Next  // Consumir coma y continuar
    else
      Break;  // No hay más elementos
  end;
  //Si llegó aquí es porque no hubo errores.
  // Verificar ':'
  if not ConsumeTok(tiCOLON, 'Se esperaba ":".') then begin
    Exit;
  end;
end;
function TParserPas.ParseCaseBranch: TCaseBranch;
var
  Branch: TCaseBranch;
begin
  Branch := TCaseBranch.Create(lex.GetSrcPos);
  ParseCaseSelector(Branch.Constants);
  if HayError then begin
    Branch.Free;
    Exit(Nil);
  end;
  // Parsear la instrucción
  Branch.Statement := TBlock.Create(lex.GetSrcPos);
  ParseStatement(Branch.Statement);
  if HayError then begin
    Branch.Free;
    Exit(Nil);
  end;
  Result := Branch;
end;
procedure TParserPas.ParseCaseStatement(var Block: TBlock);
var
  Selector: TExpression;
  CaseStmt: TCaseStatement;
  Branch: TCaseBranch;
  ElseBlock: TBlock;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  Next;  //Consume "CASE"
  Selector := ParseSimpleExpression;
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
    Branch := ParseCaseBranch;  //**** Se puede poner INLINE.
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
procedure TParserPas.ParseWithStatement(var Block: TBlock);
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
procedure TParserPas.ParseExitStatement(var Block: TBlock);
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
procedure TParserPas.ParseStatement(Body: TBlock);
begin
  if Body = nil then begin
    Body := TBlock.Create(lex.GetSrcPos);
  end;
  // Identificar el tipo de instrucción
  if tokIdent = tiEXIT then begin
    //Se valida primero porque "exit" es también un identificador.
    ParseExitStatement(Body)
  end else if tokIdent = tiIDENTIF then begin
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
  end else if tokIdent = tiASM then begin
    //Inicio de bloque ASM
    callParseASMblock(Body);  //LLama a procedimiento externo
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
procedure TParserPas.ParseDeclarations(Declars: TDeclarations);
begin
  while not HayError do begin
    if tokIdent = tiVAR then
      ParseVarDeclaration(Declars)
    else if tokIdent = tiCONST then
      ParseConstDeclaration(Declars)
    else if (tokIdent = tiPROCED) or (tokIdent = tiFUNCT) then
      ParseProcedureDeclaration(Declars)
    else if tokIdent = tiTYPE then
      ParseTypeDeclaration(Declars)
    else
      Break;  // No hay más declaraciones
  end;
end;
procedure TParserPas.ParseBody(Body: TBlock);
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
procedure TParserPas.ParseProgramHeader;
begin
  //Captura el encabezado, solo si existe.
  if tokIdent = tiPROGRAM then begin
    Next;  //pasa al nombre
    if tokIdent<>tiIDENTIF then begin
      GenError('Program name expected.');
      exit;
    end;
    astProg.Name := lex.token;
    astProg.SrcPos := lex.GetSrcPos;
    Next;  //Toma el nombre y pasa al siguiente
    if not ConsumeSemicolon then exit;
  end;
  if lex.atEof then begin
    GenError('Expected "program", "begin", "var", "type" or "const".');
    exit;
  end;
end;
procedure TParserPas.ParseProgram;
{Realiza en análisis sintáctico de un programa y construye el AST.
El lexer debe haber sido cargado previamente con el código fuente del programa, y el AST
debe haber sido limpiado}
begin
  // program <nombre> ;
  SkipWhites;
  ParseProgramHeader;
  if HayError then Exit;
  //Parsear sección USES (opcional)
  if tokIdent = tiUSES then
    ParseUsesClause(astProg.UsedUnits);
  if HayError then Exit;

  // Analizar las declaraciones
  ParseDeclarations(astProg.Declarations);
  if HayError then Exit;

  // Analizar el cuerpo principal
  ParseBody(astProg.Body);
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
procedure TParserPas.ParseUnit;
var
  untName: string;
begin
  SkipWhites;
  //Encabezado: unit Nombre;
  if not ConsumeTok(tiUNIT, 'Se esperaba "unit"') then Exit;
  if not ConsumeIdent(untName, 'Se esperaba un nombre para la unidad') then Exit;
  ConsumeSemicolon;
  //Sección INTERFACE
  if not ConsumeTok(tiINTERF, 'Se esperaba "interface"') then begin
    Exit;
  end;
  //USES en interface (opcional)
  if tokIdent = tiUSES then ParseUsesClause(astUnit.InterfaceUses);
  // Declaraciones de interface
  ParseDeclarations(astUnit.InterfaceDecls);
  if HayError then begin
    Exit;
  end;
  //Sección IMPLEMENTATION
  if not ConsumeTok(tiIMPLEM, 'Se esperaba "implementation"') then begin
    Exit;
  end;
  //USES en implementation (opcional)
  if tokIdent = tiUSES then ParseUsesClause(astUnit.ImplementationUses);
  // Declaraciones de implementation
  ParseDeclarations(astUnit.ImplementationDecls);
  if HayError then begin
    Exit;
  end;
  //Sección INITIALIZATION (opcional)
  if tokIdent = tiINITIALI then begin
    Next;  // Consumir 'initialization'
    astUnit.InitializationBlock := TBlock.Create(lex.GetSrcPos);
    ParseBody(astUnit.InitializationBlock);
    if HayError then begin
      Exit;
    end;
  end;
  //Sección FINALIZATION (opcional)
  if tokIdent = tiFINALIZA then begin
    Next;  // Consumir 'finalization'
    astUnit.FinalizationBlock := TBlock.Create(lex.GetSrcPos);
    ParseBody(astUnit.FinalizationBlock);
    if HayError then begin
      Exit;
    end;
  end;
  //Punto final
  if tokIdent <> tiDOT then
    GenError('Se esperaba "." al final de la unidad');
  Next;
  if not HayError then begin
    if not lex.atEof then
      GenError('Código extra después del final de la unidad');
  end;
end;
{$endregion}
{$region "Inicialización"}
function TParserPas.GetUnitDeclaration: boolean;
{Indica si el archivo del contexto actual, es una unidad. Debe llamarse al inico de la
exploración del archivo.}
begin
  //Salta blancos sin ejecutar directivas
  SkipWhitesNoDirect;
  //Busca UNIT
  if tokIdent = tiUNIT then begin
    lex.curCtx.StartScan;   //retorna al inicio
    exit(true);
  end;
  lex.curCtx.StartScan;   //retorna al inicio
  exit(false);
end;
procedure TParserPas.Clear;
begin
  astProg.Clear;
  astUnit.Clear;
end;
constructor TParserPas.Create(msg0: TMessageManager; lex0: TAleLexer);
begin
  //inherited;
  lex := lex0;
  msg := msg0;
  astProg := TProgram.Create('prog', lex.GetSrcPos);
  astUnit := TUnit.Create('unit', lex.GetSrcPos);
  ClearError;   //inicia motor de errores
end;
destructor TParserPas.Destroy;
begin
  astUnit.Destroy;
  astProg.Destroy;
  inherited Destroy;
end;
{$endregion}
end.
