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
    procedure TestAllConstructs;
  protected  //Elements processing
    procedure DoAnalyze;
  public     //Incialización
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
  IsUnit := parser.GetUnitDeclaration();
  if IsUnit then begin
    //DoAnalyzeUnit(astProg);
  end else begin
    parser.ParseProgram;
  end;
  //TestAllConstructs;  //Llena el astProg con código de ejemplo
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
procedure TAnalyzer.TestAllConstructs;
{CRea un código de prueba para el AST}
var
  SrcPos: TSrcPos;
  VarDeclX, VarDeclY: TVarDecl;
  Proc: TProcDecl;
  Func: TFunctDecl;
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
    VarDeclX := TVarDecl.Create('x', 'byte', SrcPos);
    astProg.Declarations.Add(VarDeclX);

    SrcPos.row := 3;
    SrcPos.col := 7;
    VarDeclY := TVarDecl.Create('y', 'byte', SrcPos);
    astProg.Declarations.Add(VarDeclY);

    // ============================================================
    // 2. Declarar procedimiento: procedure Sumar(a: byte);
    // ============================================================
    SrcPos.row := 5;
    SrcPos.col := 1;
    Proc := TProcDecl.Create('Sumar', SrcPos);

    // Añadir parserámetro
    SrcPos.row := 5;
    SrcPos.col := 15;
    Param := TVarDecl.Create('a', 'byte', SrcPos);
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
    Func := TFunctDecl.Create('Calcular', SrcPos);
    Func.ReturnTypeName := 'integer';

    // Cuerpo de la función (vacío)
    SrcPos.row := 9;
    SrcPos.col := 3;
    FuncBody := TBlock.Create(SrcPos);
    Func.Body := FuncBody;

    astProg.Declarations.Add(Func);

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

end.

