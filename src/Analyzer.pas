unit Analyzer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types, alexiaLex, ParserPas,
  ParserASM_6502, ParserDirec, CompGlobals, ASTunit, MirList, CompOptions;
type

  { TAnalyzer }
  TAnalyzer = class
  public    //Public attributes of compiler
    ID        : integer;     //Identificador para el compilador.
    IsUnit    : boolean;     //Flag to identify a Unit
    //Variables públicas del compilador
    ejecProg  : boolean;     //Indicates the compiler is working
    stopEjec  : boolean;     //To stop compilation
  public   //Componentes del compilador
    msg      : TMessageManager;  //Gestor de mensajes
    lexer    : TAleLexer;        //Analizador léxico
    parser   : TParserPas;       //Analizador sintáctico
    parserASM: TParserAsm6502;  //Parser para ensamblador
    parserDir: TParserDirective; //Parser para directivas
    options  : TCompOptions;     //Opciones del compilador
  public  //Mensajes
    procedure ClearError;
    procedure GenError(txt: string);
    procedure GenError(txt: string; const srcPos: TSrcPos);
  public
    mirRep: TMirList;    //Container for MIR representation
  private
  protected  //Elements processing
    procedure DoAnalyze;
  public     //Incialización
    procedure CreateSystemUnitInAST;
    procedure TestAllConstructs;
    constructor Create(msg0: TMessageManager);
    destructor Destroy; override;
  end;

implementation
resourcestring
  ER_INV_MEMADDR  = 'Invalid memory address.';
  ER_EXP_VAR_IDE  = 'Identifier of variable expected.';
  ER_NUM_ADD_EXP  = 'Numeric address expected.';
  ER_CON_EXP_EXP  = 'Constant expression expected.';
  ER_EQU_EXPECTD  = '"=" expected.'               ;
  ER_IDEN_EXPECT  = 'Identifier expected.'        ;
  ER_NOT_IMPLEM_  = 'Not implemented: "%s"'       ;
  ER_SEM_COM_EXP  = '":" or "," expected.'        ;
  ER_INV_ARR_SIZ  = 'Invalid array size.';
  ER_ARR_SIZ_BIG  = 'Array size to big.';
  ER_IDE_TYP_EXP  = 'Identifier of type expected.';
  ER_IDE_CON_EXP  = 'Identifier of constant expected.';
  ER_EQU_COM_EXP  = '"=" or "," expected.';
  ER_DUPLIC_IDEN  = 'Duplicated identifier: "%s"';
  ER_BOOL_EXPECT  = 'Boolean expression expected.';
  ER_EOF_END_EXP  = 'Unexpected end of file. "end" expected.';
  ER_ELS_UNEXPEC  = '"else" unexpected.';
  ER_END_EXPECTE  = '"end" expected.';
  ER_NOT_AFT_END  = 'Syntax error. Nothing should be after "END."';
  ER_INST_NEV_EXE = 'Instruction will never execute.';
  ER_UNKN_STRUCT  = 'Unknown structure.'          ;
  ER_DUPLIC_FUNC_ = 'Duplicated function: %s'     ;
  ER_FIL_NOFOUND  = 'File not found: %s'         ;
  ER_PROG_NAM_EX  = 'Program name expected.'      ;
  ER_VARIAB_EXPEC = 'Variable expected.'         ;
  ER_ONL_BYT_WORD = 'Only BYTE or WORD index is allowed in FOR.';
  ER_UNKNOWN_IDE_ = 'Unknown identifier: %s'    ;

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
  lexer.ClearContexts;   //Elimina todos los Contextos de entrada
  parser.Clear;
  parserDir.ClearMacros;         //Limpia las macros
  //Compila el texto indicado
  if not lexer.OpenContextFrom(options.mainFile) then begin
    //No lo encuentra
    GenError(Format(ER_FIL_NOFOUND, [options.mainFile]));
    exit;
  end;
  IsUnit := parser.GetUnitDeclaration();   //Detecta si es unidad
  if IsUnit then begin
    parser.ParseUnit;
  end else begin
    //Es un programa
    CreateSystemUnitInAST;  //Crea los elementos del sistema. 3ms aprox.
    parser.ParseProgram;
  end;
  //TestAllConstructs;  //Llena el astProg con código de ejemplo
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
procedure TAnalyzer.TestAllConstructs;
{CRea un código de prueba para el AST}
var
  SrcPos: TSrcPos;
  VarDeclX, VarDeclY: TVarDecl;
  Proc: TProcDecl;
  Assign1, Assign2: TAssignment;
  VarRef1, VarRef2: TExpression;
  Literal1, Literal2: TNumberLiteral;
  Param: TVarDecl;
  ProcBody, FuncBody: TBlock;
  astProg: TProgram;
  begin
    astProg := parser.astProg;
    astProg.Clear;
    // Inicializar posición (simulando la del lexer)
    SrcPos.idCtx := 1;
    SrcPos.row := 1;
    SrcPos.col := 1;

    // ============================================================
    // 1. Declarar variables globales (orden preservado)
    // ============================================================
    SrcPos.row := 3;
    SrcPos.col := 1;
    VarDeclX := TVarDecl.Create('x', SrcPos);
    VarDeclX.TypeName := 'byte';
    astProg.Declarations.Add(VarDeclX);

    SrcPos.row := 3;
    SrcPos.col := 7;
    VarDeclY := TVarDecl.Create('y', SrcPos);
    VarDeclY.TypeName := 'byte';
    astProg.Declarations.Add(VarDeclY);

    // ============================================================
    // 2. Declarar procedimiento: procedure Sumar(a: byte);
    // ============================================================
    SrcPos.row := 5;
    SrcPos.col := 1;
    Proc := TProcDecl.Create('Sumar', SrcPos, False);

    // Añadir parserámetro
    SrcPos.row := 5;
    SrcPos.col := 15;
    Param := TVarDecl.Create('a', SrcPos);
    Param.TypeName := 'byte';
    Param.IsParameter := True;
    Proc.AddParameter(Param);

    // Cuerpo del procedimiento (vacío)
    SrcPos.row := 6;
    SrcPos.col := 3;
    ProcBody := TBlock.Create(SrcPos);
    Proc.Body := ProcBody;

    astProg.Declarations.Add(Proc);

    // ============================================================
    // 3. Declarar función: function Calcular: integer;
    // ============================================================
    SrcPos.row := 8;
    SrcPos.col := 1;
    Proc:= TProcDecl.Create('Calcular', SrcPos, False);
    Proc.ReturnTypeName := 'integer';

    // Cuerpo de la función (vacío)
    SrcPos.row := 9;
    SrcPos.col := 3;
    FuncBody := TBlock.Create(SrcPos);
    Proc.Body := FuncBody;

    astProg.Declarations.Add(Proc);

    // ============================================================
    // 4. Cuerpo principal: x := 1; y := 2;
    // ============================================================
    // NOTA: astProg.Body ya existe, solo añadimos instrucciones

    // x := 1;
    SrcPos.row := 12;
    SrcPos.col := 3;
    VarRef1 := TVariableRef.Create('x', SrcPos);
    Literal1 := TNumberLiteral.Create(1, SrcPos);
    Assign1 := TAssignment.Create(VarRef1, Literal1, SrcPos);
    astProg.Body.AddStatement(Assign1);

    // y := 2;
    SrcPos.row := 13;
    SrcPos.col := 3;
    VarRef2 := TVariableRef.Create('y', SrcPos);
    Literal2 := TNumberLiteral.Create(2, SrcPos);
    Assign2 := TAssignment.Create(VarRef2, Literal2, SrcPos);
    astProg.Body.AddStatement(Assign2);

    // ============================================================
    // 5. Imprimir el astProg
    // ============================================================
    WriteLn('=== AST DEL PROGRAMA ===');
    astProg.PrintDebug;

    WriteLn;
    WriteLn('Presiona Enter para salir...');
//    ReadLn;
  end;
constructor TAnalyzer.Create(msg0: TMessageManager);
begin
  //Crea componentes del compilador
  msg := msg0;
  lexer := TAleLexer.Create(msg);
  parser := TParserPas.Create(msg, lexer);
  options  := TCompOptions.Create;
  parserASM := TParserAsm6502.Create(msg, lexer);
  parserDir := TParserDirective.Create(msg, lexer, options);
  mirRep   := TMirList.Create;
  //Comenta los Parser de Ensamblador y de directivas
  parser.callParseASMblock := @parserASM.ProcessASMblock;
  parser.callProcDIRline := @parserDir.ProcDIRline;
  //Inicializa variables
  ejecProg := false;
end;
destructor TAnalyzer.Destroy;
begin
  mirRep.Destroy;
  parserDir.Destroy;
  parserASM.Destroy;
  options.Destroy;
  parser.Destroy;
  lexer.Destroy;
  inherited Destroy;
end;
end.

