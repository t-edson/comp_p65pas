unit Analyzer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types, alexiaLex, ParserPas, ParserASM_6502, ParserDirec,
  CompGlobals, AstPascal, MirList, CompOptions, UnitManager;
type

  { TAnalyzer }
  TAnalyzer = class
  public    //Public attributes of compiler
    ID        : integer;         //Identificador para el compilador.
    //Variables públicas del compilador
    ejecProg  : boolean;         //Indicates the compiler is working
    stopEjec  : boolean;         //To stop compilation
    CompiledUnit: boolean;       //Activated when a Unit is compiled
  public   //Componentes del compilador
    msg      : TMessageManager;  //Gestor de mensajes
    lexer    : TAleLexer;        //Analizador léxico
    parser   : TParserPas;       //Analizador sintáctico
    parserASM: TParserAsm6502;   //Parser para ensamblador
    parserDir: TParserDirective; //Parser para directivas
    astProg  : TProgram;         //AST al compilar un programa.
    astUnit  : TUnit;            //AST al compilar una unidad.
    unitmgr  : TUnitManager;     //Gestor de las unidades.
    options  : TCompOptions;     //Opciones del compilador.
  public  //Mensajes
    procedure ClearError;
    procedure GenError(txt: string);
    procedure GenError(txt: string; const srcPos: TSrcPos);
  public
    mirRep: TMirList;    //Container for MIR representation
  private
    procedure parsercallUnitAdded(const untName: string);
  protected  //Elements processing
    procedure DoAnalyze;
  public     //Incialización
    procedure CreateSystemUnitInAST;
    constructor Create(msg0: TMessageManager);
    destructor Destroy; override;
  end;

implementation

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
//Compilación de secciones
procedure TAnalyzer.DoAnalyze;
{Performs the Analysis (Lexical, syntactic and semantic).
Input: The current context.
Output: The AST.}
begin
  //Preparación
  ClearError;
  parserDir.ClearMacros; //Limpia las macros
  parser.ParseFile(options.mainFile, astProg, astUnit);   //Puede generar errores
  CompiledUnit := parser.IsUnit;   //Identifica lo que ha compilado
  //TestAllConstructs;   //Llena el astProg con código de ejemplo
end;
procedure TAnalyzer.parsercallUnitAdded(const curFile: string; const untName: string);
begin

end;
//Inicialización
procedure TAnalyzer.CreateSystemUnitInAST;
{Initialize the system elements. Must be executed just one time when compiling.}
//var
//  uni: TAstUnit;
//  pars: TAstParamArray;  //Array of parameters
//  pars1null: TAstParamArray;  //Array of parameters with one Null parameter
//  f, sifDelayMs, sifWord: TAstFunDec;
begin
{  //////// Funciones del sistema ////////////
  //Implement calls to Code Generator
  callDefineArray  := @DefineArray;
  callDefineObject := @DefineObject;
  callDefinePointer:= @DefinePointer;
  callStartProgram := @Cod_StartProgram;
  callEndProgram   := @Cod_EndProgram;
  //////////////////////// Create "System" Unit. //////////////////////
  {Must be done once in First Pass. Originally system functions were created in a special
  list and has a special treatment but it implied a lot of work for manage the memory,
  linking, use of variables, and optimization. Now we create a "system unit" like a real
  unit (more less) and we create the system function here, so we use the same code for
  linking, calling and optimization that we use in common functions. Moreover, we can
  create private functions.}
  uni := CreateEleUnit('System');  //System unit
  astProg.AddElementAndOpen(uni);  //Open Unit
  CreateSystemTypesAndVars;
  lexer.curLocation := locInterface;   {Maybe not needed because element here are created directly.}
  //Creates operations
  CreateBooleanOperations;
  CreateByteOperations;
  CreateCharOperations;
  CreateWordOperations;
  CreateDWordOperations;

  //Fills "pars1null" with one Null parameter. Parameter NULL, allows any type.
  SetLength(pars1null, 0);
  AddParam(pars1null, 'n', srcPosNull, AstTree.typNull, decNone);

  ///////////////// System INLINE functions (SIF) ///////////////
  //Create system function "delay_ms". Too complex as SIF. We better implement as SNF.
//  setlength(pars, 0);  //Reset parameters
//  AddParam(pars, 'ms', srcPosNull, typWord, decRegis);  //Add parameter
//  sifDelayMs :=
//  AddSIFtoUnit('delay_ms', typNull, srcPosNull, pars, @SIF_delay_ms);

  //Create system function "exit"
  setlength(pars, 0);  //Reset parameters
  AddSIFtoUnit('exit', SFI_EXIT0, AstTree.typNull, srcPosNull, pars);  //Versión sin parserámetros
  sifFunInc :=
  AddSIFtoUnit('exit', SFI_EXIT1, AstTree.typNull, srcPosNull, pars1null);
  //Create system function "inc"
  sifFunInc :=
  AddSIFtoUnit('inc', SFI_INC, AstTree.typNull, srcPosNull, pars1null);
  //Create system function "dec"
  AddSIFtoUnit('dec', SFI_DEC, AstTree.typNull, srcPosNull, pars1null);
  //Create system function "ord"
  AddSIFtoUnit('ord', SFI_ORD, typByte, srcPosNull, pars1null);
  //Create system function "chr"
  AddSIFtoUnit('chr', SFI_CHR, typChar, srcPosNull, pars1null);
  //Create system function "byte"
  AddSIFtoUnit('byte', SFI_BYTE, typByte, srcPosNull, pars1null);
  //Create system function "boolean"
  AddSIFtoUnit('boolean', SFI_BOOLEAN, typBool, srcPosNull, pars1null);
  //Create system function "word"
  sifWord :=
  AddSIFtoUnit('word', SFI_WORD, typWord, srcPosNull, pars1null);
//  AddCallerToFrom(H, sifWord.BodyNode);  //Require H
  //Create system function "word"
  //sifWord :=
  AddSIFtoUnit('dword', SFI_DWORD,  typDWord, srcPosNull, pars1null);

  {*** Revisar esto luego

  ///////////////// System Normal functions (SNF) ///////////////
  //Multiply system function
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'A', srcPosNull, typByte, decNone);  //Add parameter
  AddParam(pars, 'B', srcPosNull, typByte, decNone);  //Add parameter
  snfBytMulByt16 :=
  AddSNFtoUnit('byt_mul_byt_16', typWord, srcPosNull, pars, @SNF_byt_mul_byt_16);
  //Division system function
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'A', srcPosNull, typByte, decRegisA);  //Add parameter
  AddParam(pars, 'B', srcPosNull, typByte, decRegisX);  //Add parameter
  snfBytDivByt8 :=
  AddSNFtoUnit('byt_div_byt_8', typByte, srcPosNull, pars, @SNF_byt_div_byt_8);
  AddCallerToFrom(E, snfBytDivByt8.BodyNode);
  //Division system function
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'A', srcPosNull, typWord, decNone);  //Add parameter
  AddParam(pars, 'B', srcPosNull, typWord, decNone);  //Add parameter
  AddLocVar(pars, 'tmp', srcPosNull, typWord, decNone);  //Add local variable
  snfWrdDivWrd16 :=
  AddSNFtoUnit('wrd_div_wrd_16', typWord, srcPosNull, pars, @SNF_wrd_div_wrd_16);
  AddCallerToFrom(E, snfWrdDivWrd16.BodyNode);
  //Word shift left
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typByte, decRegisX);   //Parameter counter shift
  snfWordShift_l :=
  AddSNFtoUnit('word_shift_l', typWord, srcPosNull, pars, @SNF_word_shift_l);
  //Delay system function
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typWord, decRegis);
  snfDelayMs :=
  AddSNFtoUnit('delay_ms', typWord, srcPosNull, pars, @SNF_delay_ms);
  //AddCallerToFrom(snfDelayMs, sifDelayMs.bodyNode);  //Dependency
  AddCallerToFrom(H, snfDelayMs.BodyNode);  //Require H

  //Add dependencies of TByte._mul.
  AddCallerToFrom(snfBytMulByt16, sifByteMulByte.bodyNode);
  AddCallerToFrom(snfWordShift_l, sifByteMulByte.bodyNode);

  AddCallerToFrom(snfBytDivByt8, sifByteDivByte.BodyNode);
  AddCallerToFrom(snfBytDivByt8, sifByteModByte.BodyNode);

  AddCallerToFrom(snfWrdDivWrd16, sifWordDivWord.BodyNode);
  AddCallerToFrom(snfWrdDivWrd16, sifWordModWord.BodyNode);

  AddCallerToFrom(snfWordShift_l, sifWordShlByte.bodyNode);
}
  //Close Unit
  astProg.CloseElement;
}end;
constructor TAnalyzer.Create(msg0: TMessageManager);
begin
  //Crea componentes del compilador
  msg       := msg0;
  lexer     := TAleLexer.Create(msg);
  parser    := TParserPas.Create(msg, lexer);
  options   := TCompOptions.Create;
  parserASM := TParserAsm6502.Create(msg, lexer);
  parserDir := TParserDirective.Create(msg, lexer, options);
  unitmgr   := TUnitManager.Create(msg, parser);
  mirRep    := TMirList.Create;
  //Conecta el parser a los otros componentes del compilador.
  parser.callProcDIRline     := @parserDir.ProcDIRline;
  parser.callParseASMblock   := @parserASM.ParseASMblock;
  parser.callParseAdicVarDec := @parserASM.ParseAdicVarDec;
  parser.callUnitAdded       := @parsercallUnitAdded;
  //Inicializa variables
  ejecProg := false;
end;
destructor TAnalyzer.Destroy;
begin
  astUnit.Free;       //Destruye si se creó
  astProg.Free;       //Destruye si se creó
  mirRep.Destroy;
  unitmgr.Destroy;
  parserDir.Destroy;
  parserASM.Destroy;
  options.Destroy;
  parser.Destroy;
  lexer.Destroy;
  inherited Destroy;
end;
end.

