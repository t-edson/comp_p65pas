unit Analyzer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types, alexiaLex, ParserPas, ParserASM_6502, ParserDirec,
  CompGlobals, AstPascal, MirList, CompOptions, UnitManager, SemAnalizer,
  LazLogger;
type

  { TAnalyzer }
  TAnalyzer = class
  public    //Public attributes of compiler
    ID        : integer;          //Identificador para el compilador.
    //Variables públicas del compilador
    ejecProg  : boolean;          //Indicates the compiler is working
    stopEjec  : boolean;          //To stop compilation
    CompiledUnit: boolean;        //Activated when a Unit is compiled
  public   //Componentes del compilador
    msg      : TMessageManager;   //Gestor de mensajes
    lexer    : TAleLexer;         //Analizador léxico
    parser   : TParserPas;        //Analizador sintáctico
    parserASM: TParserAsm6502;    //Parser para ensamblador
    parserDir: TParserDirective;  //Parser para directivas
    astProg  : TProgram;          //AST al compilar un programa.
    astUnit  : TUnit;             //AST al compilar una unidad.
    unitmgr  : TUnitManager;      //Gestor de unidades.
    checker  : TSemanticAnalyzer; //Analizador semántico
    options  : TCompOptions;      //Opciones del compilador.
  public  //Mensajes
    function HayError: boolean;
    procedure ClearError;
    procedure GenError(txt: string);
    procedure GenError(txt: string; const srcPos: TSrcPos);
  public
    mirRep: TMirList;    //Container for MIR representation
  protected  //processing
    procedure DoAnalyze;
  public     //Incialización
    constructor Create(msg0: TMessageManager);
    destructor Destroy; override;
  end;

implementation
function TAnalyzer.HayError: boolean;
begin
  exit(msg.nErrors>0);
end;
procedure TAnalyzer.ClearError;
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
procedure TAnalyzer.GenError(txt: string);
{Genera un mensaje de error en la posición actual a la posición del contexto actual.}
begin
  msg.error(lexer.GetMsgInfoE(txt));
end;
procedure TAnalyzer.GenError(txt: string; const srcPos: TSrcPos);
{Genera un mensaje de error en la posición indicada.}
begin
  msg.error(lexer.GetMsgInfoE(txt, srcPos));
end;
//Procesamiento
procedure TAnalyzer.DoAnalyze;
{Performs the Analysis (Lexical, syntactic and semantic).
Input: The current context.
Output: The AST.}
var
  unt: TCompiledUnit;
  i: Integer;
begin
  //Preparación
  ClearError;
  parserDir.ClearMacros; //Limpia las macros
  unitmgr.Clear;
  //Análisis sintáctico.
  parser.ParseFile(options.mainFile, astProg, astUnit);   //Puede generar errores
  CompiledUnit := parser.IsUnit;   //Identifica lo que ha compilado
  if HayError then Exit;    //No continuamos
  //Muestra el orden de las unidades:
  //DebugLn('Orden de creación de las unidades:');
  //for i := 0 to unitmgr.Units.Count - 1 do begin
  //  unt := TCompiledUnit(unitmgr.Units.Objects[i]);
  //  DebugLn('Unidad: ' + unt.UnitName + ', Idx=' + IntToStr(unt.Order));
  //end;

  //Análisis semántico
  checker.Analyze(astProg);
end;
//Inicialización
constructor TAnalyzer.Create(msg0: TMessageManager);
begin
  //Crea componentes del compilador
  msg       := msg0;
  lexer     := TAleLexer.Create(msg);
  parser    := TParserPas.Create(msg, lexer);
  parserASM := TParserAsm6502.Create(msg, lexer);
  parserDir := TParserDirective.Create(msg, lexer, options);
  unitmgr   := TUnitManager.Create(msg, parser);
  checker   := TSemanticAnalyzer.Create(msg, lexer);
  options   := TCompOptions.Create;
  mirRep    := TMirList.Create;
  //Conecta el parser a los otros componentes del compilador.
  parser.callProcDIRline     := @parserDir.ProcDIRline;
  parser.callParseASMblock   := @parserASM.ParseASMblock;
  parser.callParseAdicVarDec := @parserASM.ParseAdicVarDec;
  parser.callUnitAdded       := @unitmgr.LoadUnit;
  //Inicializa variables
  ejecProg := false;
end;
destructor TAnalyzer.Destroy;
begin
  mirRep.Destroy;
  options.Destroy;
  checker.Destroy;
  unitmgr.Destroy;
  parserDir.Destroy;
  parserASM.Destroy;
  astUnit.Free;       //Destruye si se creó
  astProg.Free;       //Destruye si se creó
  parser.Destroy;
  lexer.Destroy;
  inherited Destroy;
end;
end.

