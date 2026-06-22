{Parser

Clase base para la creación de un compilador como el P65Pas.
La idea es tener aquí todas las rutinas que en lo posible sean independientes del
lenguaje y del modelo de CPU.
}
//{$Define LogExpres}
unit CompBase;
interface
uses
  Classes, SysUtils, Types, LazLogger, alexiaLex,
  MirList, CompGlobals, ASTunit;
type
//Expression type, according the position it appears.  ***¿Se usa?
TPosExpres = (pexINDEP,  //Expresión independiente
              pexASIG,   //Expresión de asignación
              pexPROC,   //Expresión de procedimiento
              pexSTRUC,  //Expresión de estructura
              pexPARAM   //Expresión de parámetro de función
              );
TOperType = (operUnary,  //Operación Unaria
             operBinary  //Operación Binaria
             );
TTypeLocat = (
             tlCurrNode,    //Type at the current node.
             tlCurrCodeCon  //At the current Code conatainer.
           );
TCompileLevel = (
  clNull,        //Do nothing
  clAnalys,      //Only Analysis
  clAnalOptim,   //Analysis and Optimization
  clComplete     //Analysis, Optimization and Synthesis
);
TBootloader = (
  bldNone,  //No bootloader code
  bldJMP,   //Insert a JMP to the start of the code
  bldC64,   //Insert a bootlaoder for run from Commodore64 system
  bldCustom //Insert a custom bootlaoder
);

{ TCompilerBase }
{Clase base para crear a los objetos compiladores.
Esta clase debe ser el ancestro común de todos los compialdores a usar en PicPas.
Contiene métodos abstractos que deben ser impleemntados en las clases descendeintes.}
TCompilerBase = class
public  //Componentes principales del compilador
  lex  : TAleLexer;        //Analizador léxico
  msg  : TMessageManager;  //Referencia al gestor de mensajes
  ast  : TProgram;         //Árbol de sintaxis abstracto
public    //Public attributes of compiler
  ID        : integer;     //Identificador para el compilador.
  IsUnit    : boolean;     //Flag to identify a Unit
  //Variables públicas del compilador
  ejecProg  : boolean;     //Indicates the compiler is working
  stopEjec  : boolean;     //To stop compilation
protected //Command line options.
  mainFile    : string;    //Archivo inicial que se compila.
  hexFile     : string;    //Nombre de archivo de salida.
  comp_level  : TCompileLevel; //Compilation level.
  ForToRepeat : boolean;   //COnvert FOR loop to REPEAT loop.

  //  incDetComm  : boolean;   //Incluir Comentarios detallados.
  enabDirMsgs : boolean;   //Bandera para permitir generar mensajes desde las directivas.
protected //Compiling Options. Set by directives.
  syntaxMode  : (modPascal, modPicPas);
  bootloader  : TBootloader;  //Bootloader code for the compiled binary.
  loaderBytes : array of integer; //Custom Bootloader bytes.
  str_nullterm: boolean;   //Flag to activate the Null-terminated string for literals.
public  //Messages
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
public    //Files
  function hexFilePath: string;
  function mainFilePath: string;
  function ExpandRelPathToMain(FileName: string): string;
  procedure setHexFile(newHexFile: string);



protected //Calls to Directive Module (ParserDirec.pas)
  callProcDIRline  : procedure(const AsmLin: string; out ctxChanged: boolean) of object;
protected  // Métodos auxiliares para el parser
  function CaptureSemicolon: boolean;
  procedure SkipWhites;
  procedure SkipWhitesNoDirect;
  procedure Next;
  function ConsumeTok(tokIdent: TTokenIdent; const msgErr: string): boolean;
private  // Expresiones
  function ParseNumberLiteral: TNumberLiteral;
  function ParseIdentifier: TVariableRef;
  function ParseStringLiteral: TStringLiteral;
  function ParseFactor: TExpression;
  function ParseTerm: TExpression;
  function ParseSimpleExpression: TExpression;
  function ParseExpression: TExpression;
private  // Declaraciones
  procedure ParseVarDeclaration;
  procedure ParseProcedureDeclaration;
  procedure ParseFunctionDeclaration;
  procedure ParseParameters(Params: TVarDeclList);
  procedure ParseTypeDeclaration;
  function ParseTypeDefinition: string;
private  // Instrucciones
  procedure ParseAssigOrProcedureCall(var Block: TBlock);
  procedure ParseIfStatement(var Block: TBlock);
  procedure ParseWhileLoop(var Block: TBlock);
  procedure ParseForLoop(var Block: TBlock);
  procedure ParseRepeatUntil(var Block: TBlock);
  procedure ParseCaseStatement(var Block: TBlock);
public     // Sentencia, bloque y programa
  procedure ParseStatement(Block: TBlock);
  procedure ParseBlock(Block: TBlock; EndToken: TTokenKind = tkBlkDelim);
  procedure ParseProgram;
public     // Initialization
  procedure Clear;  // Reinicia el compilador para un nuevo programa
  constructor Create(msg0: TMessageManager);
  destructor Destroy; override;
end;

implementation

{TCompilerBase}
{%region "Messages"}
procedure TCompilerBase.ClearError;
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
function TCompilerBase.HayError: boolean;
begin
  exit(msg.nErrors>0);
end;
procedure TCompilerBase.GenInfo(txt: string; const srcPos: TSrcPos);
{Genera un mensaje de información, en la posición indicada.}
begin
  msg.info(lex.GetMsgInfo(txt, srcPos));
end;
procedure TCompilerBase.GenInfo(txt: string);
{Genera un mensaje de Información, en la posición actual del contexto. }
begin
  msg.info(lex.GetMsgInfo(txt));
end;
procedure TCompilerBase.GenWarn(txt: string; const srcPos: TSrcPos);
{Genera un mensaje de advertencia en la posición indicada.}
{ #todo : Considerar usar directamente un parámetro de tipo TMsgInfo}
begin
  msg.warn(lex.GetMsgInfo(txt, srcPos));
end;
procedure TCompilerBase.GenWarn(txt: string);
{Genera un mensaje de Advertencia, en la posición actual del contexto. }
begin
  msg.warn(lex.GetMsgInfo(txt));
end;
procedure TCompilerBase.GenError(txt: string; const srcPos: TSrcPos);
{Genera un mensaje de error en la posición indicada.}
begin
  msg.error(lex.GetMsgInfoE(txt, srcPos));
end;
procedure TCompilerBase.GenError(txt: String; const Args: array of const; const srcPos: TSrcPos);
{Versión con parámetros de GenError.}
begin
  msg.error(lex.GetMsgInfoE(Format(txt, Args), srcPos));
end;
procedure TCompilerBase.GenError(txt: string);
{Genera un mensaje de error en la posición actual a la posición del contexto actual.}
begin
  msg.error(lex.GetMsgInfoE(txt));
end;
procedure TCompilerBase.GenError(txt: String; const Args: array of const);
{Genera un mensaje de error en la posición actual del contexto.}
begin
  msg.error(lex.GetMsgInfoE(Format(txt, Args)));
end;
{%EndRegion}
{%region "Files"}
function TCompilerBase.hexFilePath: string;
begin
  Result := ExpandRelPathTo(mainFile, hexfile); //Convierte a ruta absoluta
end;
function TCompilerBase.mainFilePath: string;
begin
  Result := mainFile;
end;
function TCompilerBase.ExpandRelPathToMain(FileName: string): string;
{Convert a relative path to absolute path, considering the base path is "mainFile".}
begin
  Result := ExpandRelPathTo(mainFile, FileName);
end;
procedure TCompilerBase.setHexFile(newHexFile: string);
var
  filPath: String;
begin
  filPath := ExpandRelPathTo(mainFile, newHexFile);  //Completa ruta, si es relativa
  hexfile := filPath;
end;
{%endregion}

// Métodos auxiliares para el parser
function TCompilerBase.CaptureSemicolon: boolean;
//Verifica si sigue el delimitador de expresión ";". Si no encuentra devuelve false.
begin
  lex.SkipWhites;
  if lex.curCtx.tokIdent = tiSEMIC then begin //encontró
    Next;   //pasa al siguiente
    exit(true);
  end else begin   //es un error
    GenError('Se esperaba delimitador ";".');
    exit(false);  //sale con error
  end;
end;
procedure TCompilerBase.SkipWhites;
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
procedure TCompilerBase.SkipWhitesNoDirect;
{Similar a SkipWhites(), pero no ejecuta directivas.}
begin
  lex.SkipWhites;
  while (lex.tokType = tkDirective) do begin
    //Pasa a siguiente
    Next;
  end;
end;
procedure TCompilerBase.Next;
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
function TCompilerBase.ConsumeTok(tokIdent: TTokenIdent; const msgErr: string): boolean;
{Consume el token identificado por "tokIdent", y pasa al siguiente token saltando
blancos, comentarios o directivas.
Si no encuentra al token "tokIdent", genera el mensaje de error "msgErr", en la posición
en donde se espera encontrar el token y devuelve el valor FALS.}
begin
  if lex.curCtx.tokIdent = tokIdent then begin
    //Se ecnontró el token buscado
    Next;
    exit(True);
  end else begin
    GenError(msgErr);
    exit(False);
  end;
end;
// Expresiones
function TCompilerBase.ParseNumberLiteral: TNumberLiteral;
var
  Value: Integer;
  SrcPos: TSrcPos;
begin
  if lex.tokType<>tkLitNumber then begin
    GenError('Se esperaba un número');
    Exit(nil);
  end;

  SrcPos := lex.GetSrcPos;
  Value := StrToInt(lex.token);
  Next;
  Result := TNumberLiteral.Create(Value, SrcPos);
end;
function TCompilerBase.ParseIdentifier: TVariableRef;
var
  SrcPos: TSrcPos;
begin
  if lex.tokType <> tkIdentifier then begin
    GenError('Se esperaba un identificador');
    Exit(nil);
  end;

  SrcPos := lex.GetSrcPos;
  Result := TVariableRef.Create(lex.token, SrcPos);
  Next;
end;
function TCompilerBase.ParseStringLiteral: TStringLiteral;
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
function TCompilerBase.ParseFactor: TExpression;
var
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;

  if lex.tokType = tkLitNumber then begin
    Result := ParseNumberLiteral
  end else if lex.tokType = tkIdentifier then begin
    Result := ParseIdentifier
  end else if lex.tokType = tkString then begin
    Result := ParseStringLiteral
  end else if CompareText(lex.token, 'true')=0 then begin
    Next;
    Result := TBooleanLiteral.Create(True, SrcPos);
  end else if CompareText(lex.token, 'false')=0 then begin
    Next;
    Result := TBooleanLiteral.Create(False, SrcPos);
  end else if lex.curCtx.tokIdent = tiPAREN_OP then begin  //'('
    Next;
    Result := ParseExpression;
    if not HayError then begin
      if lex.curCtx.tokIdent <> tiPAREN_CL then begin
        GenError('Se esperaba ")"');
      end;
      Next;
    end;
  end else begin
    GenError('Factor no reconocido', SrcPos);
    Result := nil;
  end;
end;
function TCompilerBase.ParseTerm: TExpression;
var
  Left, Right: TExpression;
  Op: string;
  SrcPos: TSrcPos;
begin
  Left := ParseFactor;

  if HayError then
    Exit(Left);

  // Operadores *, /, div, mod, and
  while lex.curCtx.tokIdent in [tiMULT, tiDIV, tiIDIV, tiMOD, tiAnd] do begin
    if lex.curCtx.tokIdent in [tiMULT, tiDIV] then begin
      Op := lex.token;
    end else begin
      Op := LowerCase(lex.token);
    end;
    SrcPos := lex.GetSrcPos;
    Next;

    Right := ParseFactor;
    if not HayError then begin
      Left := TBinaryOp.Create(Op, Left, Right, SrcPos);
    end;
  end;

  Result := Left;
end;
function TCompilerBase.ParseSimpleExpression: TExpression;
var
  Left, Right: TExpression;
  Op: string;
  SrcPos: TSrcPos;
  UnaryOp: string;
begin
  // Operador unario opcional
  if lex.curCtx.tokIdent in [tiPLUS, tiMINUS] then begin
    UnaryOp := lex.token;
    SrcPos := lex.GetSrcPos;
    lex.Next;
    Left := ParseTerm;
    if not HayError then
      Result := TUnaryOp.Create(UnaryOp, Left, SrcPos)
    else
      Result := Left;
  end else begin
    Left := ParseTerm;
  end;

  if HayError then begin
    Exit(Left);
  end;

  // Operadores +, -, or
  while lex.curCtx.tokIdent in [tiPLUS, tiMINUS] do begin
    Op := lex.token;
    SrcPos := lex.GetSrcPos;
    Next;
    Right := ParseTerm;
    if not HayError then begin
      Left := TBinaryOp.Create(Op, Left, Right, SrcPos);
    end;
  end;

  Result := Left;
end;
function TCompilerBase.ParseExpression: TExpression;
var
  Left, Right: TExpression;
  Op: string;
  SrcPos: TSrcPos;
begin
  Left := ParseSimpleExpression;

  if HayError then begin
    Exit(Left);
  end;

  // Operadores relacionales
  if lex.curCtx.tokIdent in [tiEQUAL, tiLESS, tiGREAT, tiNOT_EQ, tiLESS_E, tiGREAT_E] then begin
    Op := lex.token;
    SrcPos := lex.GetSrcPos;
    Next;
    Right := ParseSimpleExpression;
    if not HayError then
      Result := TBinaryOp.Create(Op, Left, Right, SrcPos)
    else
      Result := Left;
  end else begin
    Result := Left;
  end;
end;
// Declaraciones
procedure TCompilerBase.ParseVarDeclaration;
var
  VarNames: TStringList;  //**** Se podría usar una lista estática en TCompilerBase, para evitar construir objetos
  SrcPos: TSrcPos;
  DataTypeName: string;
  i: Integer;
begin
  Next;  //Pasa al siguiente token.
  VarNames := TStringList.Create;  //**** Se podría usar una lista estática en TCompilerBase, para evitar construir objetos
  try
    // Leer lista de identificadores
    while not HayError do begin
      if lex.tokType<>tkIdentifier then begin
        GenError('Se esperaba un identificador');
        Break;
      end;
      VarNames.Add(lex.token);
      Next;
      // Verificar si hay más variables
      if lex.curCtx.tokIdent = tiCOMMA then begin
        Next;  // Consumir coma
        if lex.tokType<>tkIdentifier then
          GenError('Se esperaba un identificador después de ","');
        // Continuar con la siguiente variable
      end else
        Break;  // No hay más variables en esta línea
    end;
    if HayError then Exit;

    // Verificar el tipo
    if lex.curCtx.tokIdent <> tiCOLON then begin
      GenError('Se esperaba ":" después de las variables');
      Exit;
    end;
    Next;
    // Leer el tipo
    if lex.tokType<>tkIdentifier then begin
      GenError('Se esperaba un tipo de dato');
      Exit;
    end;
    DataTypeName := lex.token;
    // Crear declaraciones para cada variable
    SrcPos := lex.GetSrcPos;   //Usa una sola ubicación
    for i := 0 to VarNames.Count - 1 do begin
      ast.AddGlobalDecl(TVarDecl.Create(VarNames[i], DataTypeName, SrcPos));
    end;
    // Consumir ';' opcional
    Next;  //Pasa el nombre del tipo
    if lex.curCtx.tokIdent = tiSEMIC then begin
      Next;
    end;
  finally
    VarNames.Free;
  end;
end;
procedure TCompilerBase.ParseProcedureDeclaration;
var
  Proc: TProcDecl;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  if not ConsumeTok(tiPROCED, 'Se esperaba "procedure"') then Exit;
  if lex.tokType <> tkIdentifier then begin
    GenError('Se esperaba un identificador para el procedimiento');
    Exit;
  end;

  Proc := TProcDecl.Create(lex.token, SrcPos);
  Next;

  // Parsear parámetros
  if lex.curCtx.tokIdent = tiPAREN_OP then begin   //"("
    Next;
    ParseParameters(Proc.Parameters);
    if not HayError then begin
      if lex.curCtx.tokIdent <> tiPAREN_CL then  //")"
        GenError('Se esperaba ")" después de los parámetros');
      Next;
    end;
  end;

  if HayError then Exit;

  // Opcional: ; (Pascal permite ; después de los parámetros)
  if lex.curCtx.tokIdent = tiSEMIC then
    Next;

  // Parsear cuerpo
  ParseBlock(Proc.Body);

  if not HayError then
    ast.AddProcedure(Proc);
end;
procedure TCompilerBase.ParseFunctionDeclaration;
var
  Func: TFunctionDecl;
  SrcPos: TSrcPos;
  ReturnTypeName: string;
begin
  SrcPos := lex.GetSrcPos;
  if not ConsumeTok(tiFUNCT, 'Se esperaba "function"') then Exit;

  if lex.tokType <> tkIdentifier then begin
    GenError('Se esperaba un identificador para la función');
    Exit;
  end;

  // Parsear parámetros
  if lex.curCtx.tokIdent = tiPAREN_OP then begin   //'('
    Next;
    // Nota: ParseParameters necesita ser modificado para aceptar TFunctionDecl
    // Por simplicidad, aquí lo omitimos
    GenError('Funciones con parámetros no implementadas en este ejemplo', SrcPos);
    Exit;
  end;

  if lex.curCtx.tokIdent <> tiCOLON  then begin  //":"
    GenError('Se esperaba ":" después del nombre');
    Exit;
  end;
  Next;

  if lex.tokType <> tkIdentifier then begin
    GenError('Se esperaba el tipo de retorno');
    Exit;
  end;
  ReturnTypeName := lex.token;
  Next;

  // Opcional: ; (Pascal permite ; después del tipo)
  if lex.curCtx.tokIdent = tiSEMIC then
    Next;

  Func := TFunctionDecl.Create(lex.token, ReturnTypeName, SrcPos);
  ParseBlock(Func.Body);

  if not HayError then
    ast.AddFunction(Func);
end;
procedure TCompilerBase.ParseParameters(Params: TVarDeclList);
var
  Param: TVarDecl;
  SrcPos: TSrcPos;
  VarNames: TStringList;
  DataTypeName: string;
  i: Integer;
  IsVarParam: Boolean;
begin
  VarNames := TStringList.Create;
  try
    while not HayError do begin
      // Verificar si es parámetro var
      IsVarParam := False;
      if lex.curCtx.tokIdent = tiVAR then begin
        IsVarParam := True;
        Next;
      end;

      // Leer lista de identificadores
      VarNames.Clear;
      while not HayError do begin
        if lex.tokType <> tkIdentifier then begin
          GenError('Se esperaba un identificador para el parámetro');
          Break;
        end;

        VarNames.Add(lex.token);
        Next;

        if lex.curCtx.tokIdent = tiCOMMA then begin
          Next;
          if lex.tokType<>tkIdentifier then
            GenError('Se esperaba un identificador después de ","');
        end else
          Break;
      end;

      if HayError then Exit;

      // Verificar el tipo
      if lex.curCtx.tokIdent <> tiCOLON then begin
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

      // Crear parámetros
      for i := 0 to VarNames.Count - 1 do
      begin
        SrcPos := lex.GetSrcPos;
        Param := TVarDecl.Create(VarNames[i], DataTypeName, SrcPos);
        Param.IsParameter := True;
        Param.IsByReference := IsVarParam;
        Params.Add(Param);
      end;

      // Verificar si hay más parámetros
      if lex.curCtx.tokIdent = tiSEMIC then begin
        Next;
        Continue;
      end
      else
        Break;
    end;
  finally
    VarNames.Free;
  end;
end;
procedure TCompilerBase.ParseTypeDeclaration;
var
  TypeName: string;
  TypeDef: string;
  SrcPos: TSrcPos;
  TypeDecl: TTypeDecl;
begin
  while not HayError do begin
    // Leer nombre del tipo
    if lex.tokType <> tkIdentifier then begin
      GenError('Se esperaba un identificador para el tipo');
      Break;
    end;

    SrcPos := lex.GetSrcPos;
    TypeName := lex.token;
    Next;

    // Verificar '='
    if lex.curCtx.tokIdent <> tiEQUAL then begin
      GenError('Se esperaba "=" en la definición del tipo');
      Break;
    end;
    Next;

    // Leer la definición del tipo (como string simplificado)
    // En un parser real, aquí se parsearía la definición completa
    TypeDef := ParseTypeDefinition;

    // Crear nodo
    TypeDecl := TTypeDecl.Create(TypeName, TypeDef, SrcPos);
    ast.AddTypeDecl(TypeDecl);

    // Consumir ';' opcional
    if lex.curCtx.tokIdent = tiSEMIC then
      Next;
  end;
end;
function TCompilerBase.ParseTypeDefinition: string;
// Simplificado: leer la definición del tipo como token
var
  Def: string;
begin
  // Ejemplo simplificado - en un parser real parsearías la estructura
  // Por ahora, solo leemos hasta encontrar ';'
  Def := '';
  while not (HayError or (lex.curCtx.tokIdent = tiSEMIC)) do begin
    Def := Def + lex.token + ' ';
    Next;
  end;

  Result := Trim(Def);
end;
// Instrucciones
procedure TCompilerBase.ParseAssigOrProcedureCall(var Block: TBlock);
var
  Target: TVariableRef;
  Value: TExpression;
  SrcPos: TSrcPos;
  token: String;
  ProcCall: TProcedureCall;
begin
  // Guardar el identificador para verificar si es assignment o procedure call
  SrcPos := lex.GetSrcPos;
  token := lex.token;
  Next;   //Miramos el siguiente token.
  // Verificar el operador de asignación :=
  if lex.curCtx.tokIdent = tiASSIGN then begin
    //Se trata de una asignación.
    Next;  //Pasamos el ":="
    Value := ParseExpression;
    if not HayError then begin
      Target := TVariableRef.Create(token, SrcPos);
      Block.AddStatement(TAssignment.Create(Target, Value, SrcPos));
    end;
  end else if lex.curCtx.tokIdent in [tiPAREN_OP, tiSEMIC]  then begin
    //Sigue "(" o ";", debe ser una llamada a procedimiento.
    ProcCall := TProcedureCall.Create(token, SrcPos);
    // Parsear argumentos (si hay paréntesis)
    if lex.curCtx.tokIdent = tiPAREN_OP then begin // "("
      Next;  //Pasamos el "("
      while not (HayError or (lex.curCtx.tokIdent = tiPAREN_CL)) do begin  // ")"
        ProcCall.AddArgument(ParseExpression);
        if not HayError and (lex.curCtx.tokIdent = tiCOMMA) then  // ","
          Next;
      end;
      if not HayError then begin
        if lex.curCtx.tokIdent <> tiPAREN_CL then
          GenError('Se esperaba ")"');
        Next;
      end;
    end;
    if HayError then begin
      ProcCall.Destroy;  //Elimina el objeto no usado
    end else begin
      Block.AddStatement(ProcCall);
    end;

  end else begin
    GenError('Se esperaba ":=", "(" o ";".', lex.GetSrcPos);
    Exit;
  end;
end;
procedure TCompilerBase.ParseIfStatement(var Block: TBlock);
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

  // Then branch
  ThenBranch := TBlock.Create(lex.GetSrcPos);
  ParseStatement(ThenBranch);

  if HayError then Exit;

  // Else branch (opcional)
  if lex.curCtx.tokIdent = tiELSE then begin
    Next;
    ElseBranch := TBlock.Create(lex.GetSrcPos);
    ParseStatement(ElseBranch);
  end else
    ElseBranch := nil;

  if not HayError then
    Block.AddStatement(TIfStatement.Create(Condition, ThenBranch, ElseBranch, SrcPos));
end;
procedure TCompilerBase.ParseWhileLoop(var Block: TBlock);
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
procedure TCompilerBase.ParseForLoop(var Block: TBlock);
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

  if lex.curCtx.tokIdent <> tiASSIGN then begin
    GenError('Se esperaba ":=" en el bucle FOR');
    Exit;
  end;
  Next;

  StartExpr := ParseExpression;

  if HayError then Exit;

  if lex.curCtx.tokIdent = tiTO  then begin
    Direction := fdUpTo;
    Next;
  end else if lex.curCtx.tokIdent = tiDOWNTO then begin
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
procedure TCompilerBase.ParseRepeatUntil(var Block: TBlock);
var
  Body: TBlock;
  Condition: TExpression;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  if not ConsumeTok(tiREPEAT, 'Se esperaba "repeat"') then Exit;

  Body := TBlock.Create(lex.GetSrcPos);

  // Parsear instrucciones hasta encontrar 'until'
  while not (HayError or (lex.curCtx.tokIdent = tiUNTIL)) do
    ParseStatement(Body);

  if HayError then Exit;

  if not ConsumeTok(tiUNTIL, 'Se esperaba "until"') then Exit;

  Condition := ParseExpression;

  if not HayError then
    Block.AddStatement(TRepeatUntil.Create(Body, Condition, SrcPos));
end;
procedure TCompilerBase.ParseCaseStatement(var Block: TBlock);
// CASE STATEMENT (Simplificado)
var
  Selector: TExpression;
  CaseStmt: TCaseStatement;
  Branch: TCaseBranch;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  if not ConsumeTok(tiCASE, 'Se esperaba "case"') then Exit;

  Selector := ParseExpression;

  if HayError then Exit;

  if not ConsumeTok(tiOF, 'Se esperaba "of"') then Exit;

  CaseStmt := TCaseStatement.Create(Selector, SrcPos);

  // Parsear ramas
  while not (HayError or (lex.curCtx.tokIdent = tiEND)) do
  begin
    Branch := TCaseBranch.Create(lex.GetSrcPos);

    // Leer constantes: 1, 2, 3:
    while not HayError do begin
      if lex.tokType <> tkLitNumber then begin
        GenError('Se esperaba una constante en CASE');
        Break;
      end;

      Branch.AddConstant(TNumberLiteral.Create(StrToInt(lex.token), lex.GetSrcPos));
      lex.Next;

      if lex.curCtx.tokIdent = tiCOMMA then
        lex.Next
      else
        Break;
    end;

    if HayError then Break;

    if lex.curCtx.tokIdent <> tiCOLON then begin
      GenError('Se esperaba ":"');
      Break;
    end;
    lex.Next;

    // Parsear instrucción
    ParseStatement(Branch.Statement);

    if not HayError then
      CaseStmt.AddBranch(Branch);
  end;

  if HayError then Exit;

  ConsumeTok(tiEND, 'Se esperaba "end"');

  if not HayError then
    Block.AddStatement(CaseStmt);
end;
// Sentencia, bloque y programa
procedure TCompilerBase.ParseStatement(Block: TBlock);
begin
  if Block = nil then begin
    Block := TBlock.Create(lex.GetSrcPos);
  end;
  // Identificar el tipo de instrucción
  if lex.tokType = tkIdentifier then begin
    //Puede ser una asignación o una llamada a procedimiento.
    ParseAssigOrProcedureCall(Block);
  end else if lex.curCtx.tokIdent = tiIF then begin
    ParseIfStatement(Block)
  end else if lex.curCtx.tokIdent = tiWHILE then begin
    ParseWhileLoop(Block)
  end else if lex.curCtx.tokIdent = tiFOR then begin
    ParseForLoop(Block)
  end else if lex.curCtx.tokIdent = tiREPEAT then begin
    ParseRepeatUntil(Block)
  end else if lex.curCtx.tokIdent = tiCASE then begin
    ParseCaseStatement(Block)
  end else if lex.curCtx.tokIdent = tiBEGIN then begin
    // Bloque anidado - se convierte en parte del bloque actual
    ParseBlock(Block);
  end else if lex.curCtx.tokIdent = tiSEMIC then begin
    // Instrucción vacía
    Next;
  end else begin
    GenError('Instrucción no reconocida', lex.GetSrcPos);
  end;
  if HayError then Exit;

  // Opcional: ; después de la instrucción
  if lex.curCtx.tokIdent = tiSEMIC then begin
    Next;
  end;
end;
procedure TCompilerBase.ParseBlock(Block: TBlock; EndToken: TTokenKind = tkBlkDelim);
begin
  if Block = nil then Block := TBlock.Create(lex.GetSrcPos);
  if lex.curCtx.tokIdent<>tiBEGIN then begin
    GenError('Se esperaba "begin"');
    exit;
  end;
  Next;
  // Parsear instrucciones hasta 'end'
  while not (HayError or (lex.curCtx.tokIdent=tiEND)) do begin
    ParseStatement(Block);
  end;

  // end
  if lex.curCtx.tokIdent<>tiEND then begin
    GenError('Se esperaba "end"');
  end;
  Next;   //Toma el "End".

  if not HayError then begin
    // Si hay ';' después de "End" (opcional en Pascal).
    if lex.curCtx.tokIdent = tiSEMIC then begin
      Next;
    end;
  end;
end;
procedure TCompilerBase.ParseProgram;
{Realiza en análisis sintáctico de un programa y construye el AST.
El lexer debe haber sido cargado previamente con el código fuente del programa, y el AST
debe haber sido limpiado}
  procedure ParseProgramHeader;
  begin
    //Captura el encabezado, solo si existe.
    if lex.curCtx.tokIdent = tiPROGRAM then begin
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
  // Parse declarations
  while not HayError do begin
    if lex.curCtx.tokIdent = tiVAR then
      ParseVarDeclaration
    else if lex.curCtx.tokIdent = tiPROCED then
      ParseProcedureDeclaration
    else if lex.curCtx.tokIdent = tiFUNCT  then
      ParseFunctionDeclaration
    else if lex.curCtx.tokIdent = tiTYPE then
      ParseTypeDeclaration
    else
      Break;  // No hay más declaraciones
  end;
  if HayError then Exit;

  // Parsear cuerpo principal
  ParseBlock(ast.MainBody);
  if HayError then Exit;

  // Consumir el punto final
  if lex.curCtx.tokIdent<>tiDOT then
     GenError('Se esperaba "." al final del programa');
  Next;
  // Verificar que no queden tokens.
  if not HayError then begin
    if not lex.atEof then
      GenError('Código extra después del final del programa');
  end;
end;
// Inixialización
procedure TCompilerBase.Clear;
begin
  ClearError;
  ast.Clear;
end;
constructor TCompilerBase.Create(msg0: TMessageManager);
begin
  //inherited;
  lex := TAleLexer.Create(msg0);
  msg := msg0;
  ast := TProgram.Create('test', lex.GetSrcPos);
  ClearError;   //inicia motor de errores
  //Crea arbol de elementos y listas
  ejecProg := false;
end;
destructor TCompilerBase.Destroy;
begin
  ast.Destroy;
  lex.Destroy;
  inherited Destroy;
end;

end. //2183
