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
  lex   : TAleLexer;
  msg   : TMessageManager;    //Referencia al gestor de mensajes
  Prog  : TProgram;    //Nuevo AST
  mirRep: TMirList;    //Container for MIR representation
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
  procedure GenWarn(txt: string; const Args: array of const; const srcPos: TSrcPos);
  procedure GenWarn(txt: string);
  procedure GenWarn(txt: string; const Args: array of const);
  //Rutinas de generación de error. Envolturas para llamar al gestor de mensajes.
  procedure GenError(txt: string; const srcPos: TSrcPos);
  procedure GenError(txt: String; const Args: array of const; const srcPos: TSrcPos);
  procedure GenError(txt: string);
  procedure GenError(txt: String; const Args: array of const);
protected  //Parser routines
  ExprLevel  : Integer;  //Nivel de anidamiento de la rutina de evaluación de expresiones
  function EOExpres: boolean;
  function EOBlock: boolean;
  function CaptureDelExpres: boolean;
  procedure ProcCommentsNoExec;
  procedure ProcComments;
protected //Calls to Directive Module (ParserDirec.pas)
  callProcDIRline  : procedure(const AsmLin: string; out ctxChanged: boolean) of object;
public    //Types to implement
//  typByte : TAstTypeDec;
//  typBool : TAstTypeDec;
//  typChar : TAstTypeDec;
//  typWord : TAstTypeDec;
//  typDWord : TAstTypeDec;
//  typTriplet: TAstTypeDec;
public    //Files
  function hexFilePath: string;
  function mainFilePath: string;
  function ExpandRelPathToMain(FileName: string): string;
  procedure setHexFile(newHexFile: string);
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
//Errors and warnings
//Messages
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
procedure TCompilerBase.GenWarn(txt: string; const Args: array of const; const srcPos: TSrcPos);
begin
  msg.warn(lex.GetMsgInfo(Format(txt, Args), srcPos));
end;
procedure TCompilerBase.GenWarn(txt: string);
{Genera un mensaje de Advertencia, en la posición actual del contexto. }
begin
  msg.warn(lex.GetMsgInfo(txt));
end;
procedure TCompilerBase.GenWarn(txt: string; const Args: array of const);
{Genera un mensaje de Advertencia, en la posición actual del contexto. }
begin
  msg.warn(lex.GetMsgInfo(Format(txt, Args)));
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
{%region "Parser routines"}
function TCompilerBase.EOExpres: boolean; inline;
//Indica si se ha llegado al final de una expresión.
begin
  Result := lex.token = ';';  //en este caso de ejemplo, usamos punto y coma
  {En la práctica, puede ser conveniente definir un tipo de token como "tkExpDelim", para
   mejorar el tiempo de respuesta del procesamiento, de modo que la condición sería:
     Result := tokType = tkExpDelim;
  }
end;
function TCompilerBase.EOBlock: boolean; inline;
//Indica si se ha llegado el final de un bloque
begin
  Result := lex.tokType = tkBlkDelim;
end;
function TCompilerBase.CaptureDelExpres: boolean;
//Verifica si sigue un delimitador de expresión. Si encuentra devuelve false.
begin
  lex.SkipWhites;
  if EOExpres then begin //encontró
    lex.Next;   //pasa al siguiente
    exit(true);
  end else begin   //es un error
    GenError(ER_SEMIC_EXPEC);
    exit(false);  //sale con error
  end;

end;
procedure TCompilerBase.ProcComments;
{Procesa comentarios y directivas.
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
      ProcComments;   {En el nuevo contexto puede haber nuevos comentarios.}
      exit;
    end;
  //Pasa a siguiente
    lex.Next;
    lex.SkipWhites;  //limpia blancos
  end;
end;
procedure TCompilerBase.ProcCommentsNoExec;
{Similar a ProcComments(), pero no ejecuta directivas.}
begin
  lex.SkipWhites;
  while (lex.tokType = tkDirective) do begin
    //Pasa a siguiente
    lex.Next;
    lex.SkipWhites;  //limpia blancos
  end;
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
//Initialization
constructor TCompilerBase.Create(msg0: TMessageManager);
begin
  //inherited;
  lex := TAleLexer.Create(msg0);
  msg := msg0;
  Prog := TProgram.Create('test', lex.GetSrcPos);
  ClearError;   //inicia motor de errores
  //Crea arbol de elementos y listas
  mirRep    := TMirList.Create;
  ejecProg := false;
end;
destructor TCompilerBase.Destroy;
begin
  mirRep.Destroy;
  Prog.Destroy;
  lex.Destroy;
  inherited Destroy;
end;

end. //2183
