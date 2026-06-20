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
private
  procedure ParseTypeDeclaration;
  function ParseTypeDefinition: string;
public  //Componentes principales del compilador
  lex   : TAleLexer;
  msg   : TMessageManager;    //Referencia al gestor de mensajes
  FAst  : TProgram;    //Nuevo AST
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
  function CaptureDelExpres: boolean;
  procedure SkipWhites;
  procedure NextAndSkipWhites;
  procedure SkipWhitesNoDirect;


protected
  // Métodos auxiliares para el parser
  procedure CheckToken(Expected: TTokenKind; const txt: string = '');
  procedure CheckKeyword(const Expected: string; const txt: string = '');
  procedure Consume(Expected: TTokenKind; const txt: string = '');
  procedure ConsumeKeyword(const Expected: string; const txt: string = '');
  function Match(ATokenKind: TTokenKind): Boolean;
  function MatchKeyword(const AKeyword: string): Boolean;



  // Métodos de parseo
  procedure ParseVarDeclaration;
  procedure ParseProcedureDeclaration;
  procedure ParseFunctionDeclaration;
  procedure ParseParameters(Params: TVarDeclList);
  procedure ParseBlock(Block: TBlock; EndToken: TTokenKind = tkBlkDelim);
  procedure ParseStatement(Block: TBlock);
  procedure ParseAssignment(var Block: TBlock);
  procedure ParseIfStatement(var Block: TBlock);
  procedure ParseWhileLoop(var Block: TBlock);
  procedure ParseForLoop(var Block: TBlock);
  procedure ParseRepeatUntil(var Block: TBlock);
  procedure ParseCaseStatement(var Block: TBlock);
  procedure ParseProcedureCall(var Block: TBlock);
  function ParseExpression: TExpression;
  function ParseSimpleExpression: TExpression;
  function ParseTerm: TExpression;
  function ParseFactor: TExpression;
  function ParseNumberLiteral: TNumberLiteral;
  function ParseIdentifier: TVariableRef;
  function ParseStringLiteral: TStringLiteral;

  // Análisis Sintáctico
  procedure ParseProgram;
  procedure Clear;  // Reinicia el compilador para un nuevo programa

  // Propiedades
  property Ast: TProgram read FAst;

public    //Initialization
  constructor Create(msg0: TMessageManager);
  destructor Destroy; override;
end;

implementation

resourcestring
  ER_IDEN_EXPECT  = 'Identifier expected.';
  ER_DUPLIC_IDEN  = 'Duplicated identifier: "%s"';
  ER_UNDEF_TYPE_  = 'Undefined type "%s"';
  ER_SEMIC_EXPEC  = '";" expected.';
  ER_STR_EXPECTED = '"%s" expected.';
  ER_IN_EXPRESSI  = 'Error in expression. ")" expected.';
  ER_OPERAN_EXPEC = 'Operand expected.';
  ER_UND_OPER_TY_ = 'Undefined operator: %s for type: %s';
  ER_CAN_AP_OPER_ = 'Cannot apply the operator "%s" to type "%s"';
  ER_RA_HAV_USED  = 'Register A has been used.';
  ER_RX_HAV_USED  = 'Register X has been used.';
  ER_RY_HAV_USED  =  'Register Y has been used.';
  ER_CON_EXP_EXP  = 'Constant expression expected.';
  ER_ILLEG_OPERA_ = 'Illegal Operation: %s';
  ER_UNKNOWN_IDE_ = 'Unknown identifier: %s';
  ER_TYP_PARM_ER_ = 'Type parameters error on %s';
  ER_IN_CHARACTER = 'Error in character.';
  ER_INV_COD_CHAR = 'Invalid code for char.';
  ER_NOTYPDEF_NU  = 'No type defined to allocate this number.';

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





function TokenKindToString(Kind: TTokenKind): string;
begin
  case Kind of
    tkNull      : Result := 'tkNull';
    tkEol       : Result := 'tkEol';
    tkSymbol    : Result := 'tkSymbol';
    tkSpace     : Result := 'tkSpace';
    tkIdentifier: Result := 'tkIdentifier';
    tkLitNumber : Result := 'tkLitNumber';
    tkString    : Result := 'tkString';
    tkComment   : Result := 'tkComment';
    tkOperator  : Result := 'tkOperator';
    tkDirective : Result := 'tkDirective';
    tkBlkDelim  : Result := 'tkBlkDelim';
    tkChar      : Result := 'tkChar';
    tkKeyword   : Result := 'tkKeyword';
    tkDirDelim  : Result := 'tkDirDelim';
    tkOthers    : Result := 'tkOthers';
    else          Result := 'tkUnknown';
  end;
end;

// ============================================================
// MÉTODOS AUXILIARES DEL PARSER
// ============================================================
procedure TCompilerBase.CheckToken(Expected: TTokenKind; const txt: string);
begin
  if lex.tokType <> Expected then
  begin
    if txt = '' then
      GenError('Se esperaba "%s" pero se encontró "%s"', [TokenKindToString(Expected), lex.token])
    else
      GenError(txt);
  end;
end;

procedure TCompilerBase.CheckKeyword(const Expected: string; const txt: string);
begin
  if not MatchKeyword(Expected) then
  begin
    if txt = '' then
      GenError('Se esperaba "%s" pero se encontró "%s"', [Expected, lex.token])
    else
      GenError(txt);
  end;
end;

procedure TCompilerBase.Consume(Expected: TTokenKind; const txt: string);
begin
  CheckToken(Expected, txt);
  if not HayError then
    lex.Next;
end;

procedure TCompilerBase.ConsumeKeyword(const Expected: string; const txt: string);
begin
  CheckKeyword(Expected, txt);
  if not HayError then
    lex.Next;
end;

function TCompilerBase.Match(ATokenKind: TTokenKind): Boolean;
begin
  Result := lex.tokType = ATokenKind;
end;

function TCompilerBase.MatchKeyword(const AKeyword: string): Boolean;
begin
  Result := (lex.tokType = tkKeyword) and (CompareText(lex.token, AKeyword) = 0);
end;

function TCompilerBase.CaptureDelExpres: boolean;
//Verifica si sigue el delimitador de expresión ";". Si no encuentra devuelve false.
begin
  lex.SkipWhites;
  if lex.curCtx.tokIdent = tiSemic then begin //encontró
    lex.Next;   //pasa al siguiente
    SkipWhites;
    exit(true);
  end else begin   //es un error
    GenError(ER_SEMIC_EXPEC);
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
procedure TCompilerBase.NextAndSkipWhites;
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
procedure TCompilerBase.SkipWhitesNoDirect;
{Similar a SkipWhites(), pero no ejecuta directivas.}
begin
  lex.SkipWhites;
  while (lex.tokType = tkDirective) do begin
    //Pasa a siguiente
    lex.Next;
    lex.SkipWhites;  //limpia blancos
  end;
end;


// ============================================================
// MÉTODO PRINCIPAL DE PARSEO
// ============================================================

procedure TCompilerBase.Clear;
begin
  ClearError;
  FAst.Clear;
end;

// ============================================================
// DECLARACIONES
// ============================================================

procedure TCompilerBase.ParseVarDeclaration;
var
  VarNames: TStringList;  //**** Se podría usar una lista estática en TCompilerBase, para evitar construir objetos
  SrcPos: TSrcPos;
  DataTypeName: string;
  i: Integer;
begin
  lex.Next;  //Pasa al siguiente token.
  SkipWhites;
  VarNames := TStringList.Create;  //**** Se podría usar una lista estática en TCompilerBase, para evitar construir objetos
  try
    // Leer lista de identificadores
    while not HayError do begin
      if lex.tokType<>tkIdentifier then begin
        GenError('Se esperaba un identificador');
        Break;
      end;
      VarNames.Add(lex.token);
      lex.Next;
      SkipWhites;
      // Verificar si hay más variables
      if lex.curCtx.tokIdent = tiComma then begin
        lex.Next;  // Consumir coma
        SkipWhites;
        if lex.tokType<>tkIdentifier then
          GenError('Se esperaba un identificador después de ","');
        // Continuar con la siguiente variable
      end else
        Break;  // No hay más variables en esta línea
    end;
    if HayError then Exit;

    // Verificar el tipo
    if lex.curCtx.tokIdent <> tiColon then begin
      GenError('Se esperaba ":" después de las variables');
      Exit;
    end;
    lex.Next;
    SkipWhites;
    // Leer el tipo
    if lex.tokType<>tkIdentifier then begin
      GenError('Se esperaba un tipo de dato');
      Exit;
    end;
    DataTypeName := lex.token;
    // Crear declaraciones para cada variable
    SrcPos := lex.GetSrcPos;   //Usa una sola ubicación
    for i := 0 to VarNames.Count - 1 do begin
      FAst.AddGlobalDecl(TVarDecl.Create(VarNames[i], DataTypeName, SrcPos));
    end;
    // Consumir ';' opcional
    lex.Next;  //Pasa el nombre del tipo
    SkipWhites;
    if lex.curCtx.tokIdent = tiSemic then begin
      lex.Next;
      SkipWhites;
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
  ConsumeKeyword('procedure', 'Se esperaba "procedure"');

  if HayError then Exit;

  if not Match(tkIdentifier) then
  begin
    GenError('Se esperaba un identificador para el procedimiento');
    Exit;
  end;

  Proc := TProcDecl.Create(lex.token, SrcPos);
  lex.Next;

  // Parsear parámetros
  if Match(tkSymbol) and (lex.token = '(') then
  begin
    lex.Next;
    ParseParameters(Proc.Parameters);
    if not HayError then
    begin
      if not (Match(tkSymbol) and (lex.token = ')')) then
        GenError('Se esperaba ")" después de los parámetros');
      lex.Next;
    end;
  end;

  if HayError then Exit;

  // Opcional: ; (Pascal permite ; después de los parámetros)
  if Match(tkSymbol) and (lex.token = ';') then
    lex.Next;

  // Parsear cuerpo
  ParseBlock(Proc.Body);

  if not HayError then
    FAst.AddProcedure(Proc);
end;

procedure TCompilerBase.ParseFunctionDeclaration;
var
  Func: TFunctionDecl;
  SrcPos: TSrcPos;
  ReturnTypeName: string;
begin
  SrcPos := lex.GetSrcPos;
  ConsumeKeyword('function', 'Se esperaba "function"');

  if HayError then Exit;

  if not Match(tkIdentifier) then
  begin
    GenError('Se esperaba un identificador para la función');
    Exit;
  end;

  // Parsear parámetros
  if Match(tkSymbol) and (lex.token = '(') then
  begin
    lex.Next;
    // Nota: ParseParameters necesita ser modificado para aceptar TFunctionDecl
    // Por simplicidad, aquí lo omitimos
    GenError('Funciones con parámetros no implementadas en este ejemplo', SrcPos);
    Exit;
  end;

  if not (Match(tkSymbol) and (lex.token = ':')) then
  begin
    GenError('Se esperaba ":" después del nombre');
    Exit;
  end;
  lex.Next;

  if not Match(tkIdentifier) then
  begin
    GenError('Se esperaba el tipo de retorno');
    Exit;
  end;
  ReturnTypeName := lex.token;
  lex.Next;

  // Opcional: ; (Pascal permite ; después del tipo)
  if Match(tkSymbol) and (lex.token = ';') then
    lex.Next;

  Func := TFunctionDecl.Create(lex.token, ReturnTypeName, SrcPos);
  ParseBlock(Func.Body);

  if not HayError then
    FAst.AddFunction(Func);
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
    while not HayError do
    begin
      // Verificar si es parámetro var
      IsVarParam := False;
      if MatchKeyword('var') then
      begin
        IsVarParam := True;
        lex.Next;
      end;

      // Leer lista de identificadores
      VarNames.Clear;
      while not HayError do
      begin
        if not Match(tkIdentifier) then
        begin
          GenError('Se esperaba un identificador para el parámetro');
          Break;
        end;

        VarNames.Add(lex.token);
        lex.Next;

        if Match(tkSymbol) and (lex.token = ',') then
        begin
          lex.Next;
          if not Match(tkIdentifier) then
            GenError('Se esperaba un identificador después de ","');
        end
        else
          Break;
      end;

      if HayError then Exit;

      // Verificar el tipo
      if not (Match(tkSymbol) and (lex.token = ':')) then
      begin
        GenError('Se esperaba ":" después de los parámetros');
        Exit;
      end;
      lex.Next;

      if not Match(tkIdentifier) then
      begin
        GenError('Se esperaba un tipo de dato');
        Exit;
      end;

      DataTypeName := lex.token;
      lex.Next;

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
      if Match(tkSymbol) and (lex.token = ';') then
      begin
        lex.Next;
        Continue;
      end
      else
        Break;
    end;
  finally
    VarNames.Free;
  end;
end;
// En CompilerBase - NUEVO MÉTODO
procedure TCompilerBase.ParseTypeDeclaration;
var
  TypeName: string;
  TypeDef: string;
  SrcPos: TSrcPos;
  TypeDecl: TTypeDecl;
begin
  ConsumeKeyword('type', 'Se esperaba "type"');

  if HayError then Exit;

  while not HayError do
  begin
    // Leer nombre del tipo
    if not Match(tkIdentifier) then
    begin
      GenError('Se esperaba un identificador para el tipo');
      Break;
    end;

    SrcPos := lex.GetSrcPos;
    TypeName := lex.token;
    lex.Next;

    // Verificar '='
    if not (Match(tkSymbol) and (lex.token = '=')) then
    begin
      GenError('Se esperaba "=" en la definición del tipo');
      Break;
    end;
    lex.Next;

    // Leer la definición del tipo (como string simplificado)
    // En un parser real, aquí se parsearía la definición completa
    TypeDef := ParseTypeDefinition;

    // Crear nodo
    TypeDecl := TTypeDecl.Create(TypeName, TypeDef, SrcPos);
    FAst.AddTypeDecl(TypeDecl);

    // Consumir ';' opcional
    if Match(tkSymbol) and (lex.token = ';') then
      lex.Next;
  end;
end;

// Simplificado: leer la definición del tipo como token
function TCompilerBase.ParseTypeDefinition: string;
var
  Def: string;
begin
  // Ejemplo simplificado - en un parser real parsearías la estructura
  // Por ahora, solo leemos hasta encontrar ';'
  Def := '';
  while not (HayError or (Match(tkSymbol) and (lex.token = ';'))) do
  begin
    Def := Def + lex.token + ' ';
    lex.Next;
  end;

  Result := Trim(Def);
end;

// INSTRUCCIONES

procedure TCompilerBase.ParseStatement(Block: TBlock);
var
  SrcPos: TSrcPos;
begin
  if Block = nil then
    Block := TBlock.Create(lex.GetSrcPos);

  SrcPos := lex.GetSrcPos;
  // Identificar el tipo de instrucción
  if lex.tokType = tkIdentifier then begin
    // Guardar el identificador para verificar si es assignment o procedure call
    // Por simplicidad, asumimos que es asignación si sigue :=
    // y procedure call si sigue ( o ; o )
    // Necesitamos lookahead: guardamos el estado actual del lexer
    // Como simplificación, usamos un enfoque más simple
    ParseAssignment(Block);
  end else if lex.curCtx.tokIdent = tiIf then begin
    ParseIfStatement(Block)
  end else if lex.curCtx.tokIdent = tiWhile then begin
    ParseWhileLoop(Block)
  end else if lex.curCtx.tokIdent = tiFor then begin
    ParseForLoop(Block)
  end else if lex.curCtx.tokIdent = tiRepeat then begin
    ParseRepeatUntil(Block)
  end else if lex.curCtx.tokIdent = tiCase then begin
    ParseCaseStatement(Block)
  end else if lex.curCtx.tokIdent = tiBegin then begin
    // Bloque anidado - se convierte en parte del bloque actual
    ParseBlock(Block);
  end else if Match(tkSymbol) and (lex.token = ';') then begin
    // Instrucción vacía
    lex.Next;
  end else begin
    GenError('Instrucción no reconocida', SrcPos);
  end;
  if HayError then Exit;

  // Opcional: ; después de la instrucción
  if lex.curCtx.tokIdent = tiSemic then begin
    lex.Next;
    SkipWhites;
  end;
end;

// Expresiones
function TCompilerBase.ParseNumberLiteral: TNumberLiteral;
var
  Value: Integer;
  SrcPos: TSrcPos;
begin
  if not Match(tkLitNumber) then begin
    GenError('Se esperaba un número');
    Exit(nil);
  end;

  SrcPos := lex.GetSrcPos;
  Value := StrToInt(lex.token);
  lex.Next;
  SkipWhites;
  Result := TNumberLiteral.Create(Value, SrcPos);
end;
function TCompilerBase.ParseIdentifier: TVariableRef;
var
  SrcPos: TSrcPos;
begin
  if not Match(tkIdentifier) then begin
    GenError('Se esperaba un identificador');
    Exit(nil);
  end;

  SrcPos := lex.GetSrcPos;
  Result := TVariableRef.Create(lex.token, SrcPos);
  lex.Next;
  SkipWhites;
end;
function TCompilerBase.ParseStringLiteral: TStringLiteral;
var
  SrcPos: TSrcPos;
begin
  if not Match(tkString) then
  begin
    GenError('Se esperaba una cadena');
    Exit(nil);
  end;

  SrcPos := lex.GetSrcPos;
  Result := TStringLiteral.Create(lex.token, SrcPos);
  lex.Next;
  SkipWhites;
end;
function TCompilerBase.ParseFactor: TExpression;
var
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;

  if Match(tkLitNumber) then begin
    Result := ParseNumberLiteral
  end else if Match(tkIdentifier) then begin
    Result := ParseIdentifier
  end else if Match(tkString) then begin
    Result := ParseStringLiteral
  end else if MatchKeyword('true') then begin
    lex.Next;
    SkipWhites;
    Result := TBooleanLiteral.Create(True, SrcPos);
  end else if MatchKeyword('false') then begin
    lex.Next;
    SkipWhites;
    Result := TBooleanLiteral.Create(False, SrcPos);
  end else if lex.curCtx.tokIdent = tiParOpen then begin  //'('
    lex.Next;
    SkipWhites;
    Result := ParseExpression;
    if not HayError then begin
      if lex.curCtx.tokIdent <> tiParClos then begin
        GenError('Se esperaba ")"');
      end;
      lex.Next;
      SkipWhites;
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
  while lex.curCtx.tokIdent in [tiTimes, tiDiv, tiIDiv, tiMod, tiAnd] do begin
    if lex.curCtx.tokIdent in [tiTimes, tiDiv] then begin
      Op := lex.token;
    end else begin
      Op := LowerCase(lex.token);
    end;
    SrcPos := lex.GetSrcPos;
    lex.Next;
    SkipWhites;

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
  if lex.curCtx.tokIdent in [tiPlus, tiMinus] then begin
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
  while lex.curCtx.tokIdent in [tiPlus, tiMinus] do begin
    Op := lex.token;
    SrcPos := lex.GetSrcPos;
    lex.Next;
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
  if Match(tkSymbol) and (
    (lex.token = '=') or (lex.token = '<') or (lex.token = '>') or
    (lex.token = '<>') or (lex.token = '<=') or (lex.token = '>=')
  ) then begin
    Op := lex.token;
    SrcPos := lex.GetSrcPos;
    lex.Next;
    Right := ParseSimpleExpression;
    if not HayError then
      Result := TBinaryOp.Create(Op, Left, Right, SrcPos)
    else
      Result := Left;
  end else begin
    Result := Left;
  end;
end;

// ASIGNACIÓN
procedure TCompilerBase.ParseAssignment(var Block: TBlock);
var
  Target: TVariableRef;
  Value: TExpression;
  SrcPos: TSrcPos;
begin
  if lex.tokType <> tkIdentifier then begin
    GenError('Se esperaba un identificador');
    Exit;
  end;
  SrcPos := lex.GetSrcPos;
  Target := TVariableRef.Create(lex.token, SrcPos);
  lex.Next;
  SkipWhites;

  // Verificar el operador de asignación :=
  if lex.curCtx.tokIdent = tiAssign then begin
    lex.Next;
    SkipWhites;
  end else begin
    GenError('Se esperaba ":=" en la asignación', SrcPos);
    Exit;
  end;

  Value := ParseExpression;

  if not HayError then begin
    Block.AddStatement(TAssignment.Create(Target, Value, SrcPos));
  end;
end;
// IF STATEMENT
procedure TCompilerBase.ParseIfStatement(var Block: TBlock);
var
  Condition: TExpression;
  ThenBranch, ElseBranch: TBlock;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  ConsumeKeyword('if', 'Se esperaba "if"');

  if HayError then Exit;

  Condition := ParseExpression;

  if HayError then Exit;

  ConsumeKeyword('then', 'Se esperaba "then"');

  if HayError then Exit;

  // Then branch
  ThenBranch := TBlock.Create(lex.GetSrcPos);
  ParseStatement(ThenBranch);

  if HayError then Exit;

  // Else branch (opcional)
  if MatchKeyword('else') then
  begin
    lex.Next;
    ElseBranch := TBlock.Create(lex.GetSrcPos);
    ParseStatement(ElseBranch);
  end
  else
    ElseBranch := nil;

  if not HayError then
    Block.AddStatement(TIfStatement.Create(Condition, ThenBranch, ElseBranch, SrcPos));
end;
// WHILE LOOP
procedure TCompilerBase.ParseWhileLoop(var Block: TBlock);
var
  Condition: TExpression;
  Body: TBlock;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  ConsumeKeyword('while', 'Se esperaba "while"');

  if HayError then Exit;

  Condition := ParseExpression;

  if HayError then Exit;

  ConsumeKeyword('do', 'Se esperaba "do"');

  if HayError then Exit;

  Body := TBlock.Create(lex.GetSrcPos);
  ParseStatement(Body);

  if not HayError then
    Block.AddStatement(TWhileLoop.Create(Condition, Body, SrcPos));
end;
// FOR LOOP
procedure TCompilerBase.ParseForLoop(var Block: TBlock);
var
  ControlVar: TVariableRef;
  Direction: TForDirection;
  StartExpr, EndExpr: TExpression;
  Body: TBlock;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  ConsumeKeyword('for', 'Se esperaba "for"');

  if HayError then Exit;

  if not Match(tkIdentifier) then
  begin
    GenError('Se esperaba una variable de control');
    Exit;
  end;

  ControlVar := TVariableRef.Create(lex.token, lex.GetSrcPos);
  lex.Next;

  if not (Match(tkSymbol) and (lex.token = ':=')) then
  begin
    GenError('Se esperaba ":=" en el bucle FOR');
    Exit;
  end;
  lex.Next;

  StartExpr := ParseExpression;

  if HayError then Exit;

  if MatchKeyword('to') then
  begin
    Direction := fdUpTo;
    lex.Next;
  end
  else if MatchKeyword('downto') then
  begin
    Direction := fdDownTo;
    lex.Next;
  end
  else
  begin
    GenError('Se esperaba "to" o "downto" en el bucle FOR');
    Exit;
  end;

  EndExpr := ParseExpression;

  if HayError then Exit;

  ConsumeKeyword('do', 'Se esperaba "do"');

  if HayError then Exit;

  Body := TBlock.Create(lex.GetSrcPos);
  ParseStatement(Body);

  if not HayError then
    Block.AddStatement(TForLoop.Create(ControlVar, Direction, StartExpr, EndExpr, Body, SrcPos));
end;
// REPEAT UNTIL
procedure TCompilerBase.ParseRepeatUntil(var Block: TBlock);
var
  Body: TBlock;
  Condition: TExpression;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  ConsumeKeyword('repeat', 'Se esperaba "repeat"');

  if HayError then Exit;

  Body := TBlock.Create(lex.GetSrcPos);

  // Parsear instrucciones hasta encontrar 'until'
  while not (HayError or MatchKeyword('until')) do
    ParseStatement(Body);

  if HayError then Exit;

  ConsumeKeyword('until', 'Se esperaba "until"');

  if HayError then Exit;

  Condition := ParseExpression;

  if not HayError then
    Block.AddStatement(TRepeatUntil.Create(Body, Condition, SrcPos));
end;
// CASE STATEMENT (Simplificado)
procedure TCompilerBase.ParseCaseStatement(var Block: TBlock);
var
  Selector: TExpression;
  CaseStmt: TCaseStatement;
  Branch: TCaseBranch;
  SrcPos: TSrcPos;
begin
  SrcPos := lex.GetSrcPos;
  ConsumeKeyword('case', 'Se esperaba "case"');

  if HayError then Exit;

  Selector := ParseExpression;

  if HayError then Exit;

  ConsumeKeyword('of', 'Se esperaba "of"');

  if HayError then Exit;

  CaseStmt := TCaseStatement.Create(Selector, SrcPos);

  // Parsear ramas
  while not (HayError or MatchKeyword('end')) do
  begin
    Branch := TCaseBranch.Create(lex.GetSrcPos);

    // Leer constantes: 1, 2, 3:
    while not HayError do
    begin
      if not Match(tkLitNumber) then
      begin
        GenError('Se esperaba una constante en CASE');
        Break;
      end;

      Branch.AddConstant(TNumberLiteral.Create(StrToInt(lex.token), lex.GetSrcPos));
      lex.Next;

      if Match(tkSymbol) and (lex.token = ',') then
        lex.Next
      else
        Break;
    end;

    if HayError then Break;

    if not (Match(tkSymbol) and (lex.token = ':')) then
    begin
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

  ConsumeKeyword('end', 'Se esperaba "end"');

  if not HayError then
    Block.AddStatement(CaseStmt);
end;
// PROCEDURE CALL (Simplificado)
procedure TCompilerBase.ParseProcedureCall(var Block: TBlock);
var
  ProcCall: TProcedureCall;
  SrcPos: TSrcPos;
begin
  if not Match(tkIdentifier) then
  begin
    GenError('Se esperaba un identificador');
    Exit;
  end;

  SrcPos := lex.GetSrcPos;
  ProcCall := TProcedureCall.Create(lex.token, SrcPos);
  lex.Next;

  // Parsear argumentos (si hay paréntesis)
  if Match(tkSymbol) and (lex.token = '(') then
  begin
    lex.Next;
    while not (HayError or (Match(tkSymbol) and (lex.token = ')'))) do
    begin
      ProcCall.AddArgument(ParseExpression);
      if not HayError and Match(tkSymbol) and (lex.token = ',') then
        lex.Next;
    end;
    if not HayError then
    begin
      if not (Match(tkSymbol) and (lex.token = ')')) then
        GenError('Se esperaba ")"');
      lex.Next;
    end;
  end;

  if not HayError then
    Block.AddStatement(ProcCall);
end;

// Bloque y programa
procedure TCompilerBase.ParseBlock(Block: TBlock; EndToken: TTokenKind = tkBlkDelim);
begin
  if Block = nil then Block := TBlock.Create(lex.GetSrcPos);
  if lex.curCtx.tokIdent<>tiBegin then begin
    GenError('Se esperaba "begin"');
    exit;
  end;
  lex.Next;
  SkipWhites;
  // Parsear instrucciones hasta 'end'
  while not (HayError or (lex.curCtx.tokIdent=tiEnd)) do begin
    ParseStatement(Block);
  end;

  // end
  if lex.curCtx.tokIdent<>tiEnd then begin
    GenError('Se esperaba "end"');
  end;
  lex.Next;   //Toma el "End".
  SkipWhites;

  if not HayError then begin
    // Si hay ';' después de "End" (opcional en Pascal).
    if lex.curCtx.tokIdent = tiSemic then begin
      lex.Next;
      SkipWhites;
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
    if lex.curCtx.tokIdent = tiProgram then begin
      lex.Next;  //pasa al nombre
      SkipWhites;
      if lex.atEof then begin
        GenError('Program name expected.');
        exit;
      end;
      FAst.Name := lex.token;
      FAst.srcDec := lex.GetSrcPos;
      lex.Next;  //Toma el nombre y pasa al siguiente
      if not CaptureDelExpres then exit;
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
    if lex.curCtx.tokIdent = tiVar then
      ParseVarDeclaration
    else if lex.curCtx.tokIdent = tiProced then
      ParseProcedureDeclaration
    else if lex.curCtx.tokIdent = tiFunct  then
      ParseFunctionDeclaration
    else
      Break;  // No hay más declaraciones
  end;
  if HayError then Exit;

  // Parsear cuerpo principal
  ParseBlock(FAst.MainBody);
  if HayError then Exit;

  // Consumir el punto final
  if lex.curCtx.tokIdent<>tiDot then
     GenError('Se esperaba "." al final del programa');
  lex.Next;
  SkipWhites;
  // Verificar que no queden tokens.
  if not HayError then begin
    if not lex.atEof then
      GenError('Código extra después del final del programa');
  end;
end;

//Initialization
constructor TCompilerBase.Create(msg0: TMessageManager);
begin
  //inherited;
  lex := TAleLexer.Create(msg0);
  msg := msg0;
  FAst := TProgram.Create('test', lex.GetSrcPos);
  ClearError;   //inicia motor de errores
  //Crea arbol de elementos y listas
  ejecProg := false;
end;
destructor TCompilerBase.Destroy;
begin
  FAst.Destroy;
  lex.Destroy;
  inherited Destroy;
end;

end. //2183
