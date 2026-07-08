unit Analyzer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types, alexiaLex, Parser,
  ParserASM_6502, CompGlobals, AstElemP65, ASTunit, MirList;
type

  { TAnalyzer }
  TAnalyzer = class(TParser)
  public    //Public attributes of compiler
    ID        : integer;     //Identificador para el compilador.
    IsUnit    : boolean;     //Flag to identify a Unit
    //Variables públicas del compilador
    ejecProg  : boolean;     //Indicates the compiler is working
    stopEjec  : boolean;     //To stop compilation
  protected //Compiling Options. Set by directives.
    syntaxMode  : (modPascal, modPicPas);
    bootloader  : TBootloader;  //Bootloader code for the compiled binary.
    loaderBytes : array of integer; //Custom Bootloader bytes.
    str_nullterm: boolean;   //Flag to activate the Null-terminated string for literals.
  protected //Command line options.
    mainFile    : string;    //Archivo inicial que se compila.
    hexFile     : string;    //Nombre de archivo de salida.
    comp_level  : TCompileLevel; //Compilation level.
    ForToRepeat : boolean;   //COnvert FOR loop to REPEAT loop.
    //  incDetComm  : boolean;   //Incluir Comentarios detallados.
    enabDirMsgs : boolean;   //Bandera para permitir generar mensajes desde las directivas.
  public    //Files
    function hexFilePath: string;
    function mainFilePath: string;
    function ExpandRelPathToMain(FileName: string): string;
    procedure setHexFile(newHexFile: string);
  public
    mirRep: TMirList;    //Container for MIR representation
  public    //Access to CPU hardware.
    function PICName: string; virtual; abstract;
    function RAMmax: integer; virtual; abstract;
  private
    procedure SetParameter(var funPar: TAstParam; const name: string;
      const srcPos: TSrcPos; typ: TAstTypeDec; const adicVar: TAdicVarDec);
    procedure TestAllConstructs;
  protected
    function StartOfSection: boolean;
  protected  //Elements processing
    procedure AnalyzeInlineDeclar(elemLocat: TElemLocation);
    procedure DoAnalyze;
  public
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

{$region "Files"}
function TAnalyzer.hexFilePath: string;
begin
  Result := ExpandRelPathTo(mainFile, hexfile); //Convierte a ruta absoluta
end;
function TAnalyzer.mainFilePath: string;
begin
  Result := mainFile;
end;
function TAnalyzer.ExpandRelPathToMain(FileName: string): string;
{Convert a relative path to absolute path, considering the base path is "mainFile".}
begin
  Result := ExpandRelPathTo(mainFile, FileName);
end;
procedure TAnalyzer.setHexFile(newHexFile: string);
var
  filPath: String;
begin
  filPath := ExpandRelPathTo(mainFile, newHexFile);  //Completa ruta, si es relativa
  hexfile := filPath;
end;
{$endregion}

function TAnalyzer.StartOfSection: boolean;
var
  tokL: String;
begin
  tokL := lowercase(lex.token);
  Result := (tokL ='var') or (tokL ='const') or
            (tokL ='type') or (tokL ='procedure') or (tokL ='inline');
end;
//Elements processing

procedure TAnalyzer.SetParameter(var funPar: TAstParam;
    const name: string; const srcPos: TSrcPos; typ: TAstTypeDec; const adicVar: TAdicVarDec);
begin
  funPar.name  := name;
  funPar.srcPos:= srcPos;
  funPar.typ   := typ;
  funPar.adicVar := adicVar;
end;


procedure TAnalyzer.AnalyzeInlineDeclar(elemLocat: TElemLocation);
{Compila la declaración de procedimientos INLINE. Tanto procedimientos como funciones
 INLINE se manejan internamente como funciones.
 IsImplementation, se usa para cuando se está compilando en la sección IMPLEMENTATION.}
begin
//  {Este método, solo se ejecutará en la primera pasada, en donde todos los procedimientos
//  se codifican al inicio de la memoria, y las variables y registros se ubican al
//  inicio de la memoria RAM, ya que lo que importa es simplemente recabar información
//  del procedimiento, y no tanto codificarlo. }
//  CallResetRAM;   //Limpia RAM y FLASH, y fija CurrBank
//  case elemLocat of
//  locInterface: begin
//    //Los procedimientos en INTERFACE, no se procesan aquí. Se procesan en CompileUnit().
//  end;
//  locImplement:  begin
//    //Se compila para implementación.
//    {Este proceso es más complejo. La idea es compilar el encabezado de cualquier función,
//    y luego comparar para ver si corresponde a una implementación o no. Si es
//    implementación, se elimina el nodo creado y se trabaja con el de la declaración.}
//    ReadInlineHeader(procName, retType, srcPos, pars);
//    if HayError then exit;
//    //Verifica si es implementación de una función en la INTERFACE o no.
//    ParentElems := astProg.curNode.elements;  //Para comparar
//    {Se supone que esta exploración solo se hará en la primera pasada, así que no hay
//    problema, en hacer una exploración común.}
//    //debugln('Buscando declaración de %s en nodo %s desde 0 hasta %d', [fun.name, ParentElems.name, ParentElems.elements.Count-2]);
//    Found := false;
//    uname := upcase(procName);
//    for ele in ParentElems do begin
//      if ele.location = locInterface then begin
//        //Es elemento de INTERFACE
//        if ele.uname = uname then begin
//          //Hay coincidencia de nombre
//          if ele.idClass = eleFuncImp then begin
//            //Para las funciones, se debe comparar los parámetros
//            fun := TxpEleInlin(ele);
//            if fun.SameParamsType(pars) then begin
//              Found := true;
//              break;
//            end;
//          end else begin
//            //Si tiene el mismo nombre que cualquier otro elemento, es conflicto
//            GenError('Identifier "%s" already defined', [uname]);
//            exit;
//          end;
//        end;
//      end else begin
//        {Debe ser elemento de IMPLEMENTATION, no hay otra opción porque se supone que
//        estamos en la sección de IMPLEMENTATION, así que el Parent, debe ser una unidad.}
//        GenErrorPos(ER_DUPLIC_FUNC_,[procName], srcPos);  //Está duplicada en IMPLEMENTATION
//        exit;
//      end;
//    end;
//    if Found then begin
//      //Es una implementación. No vale la pena tener otro nodo.
//      astProg.OpenElement(fun);  //Abre el nodo anterior
//    end else begin
//      //Debe ser una función privada. No declarada en Interface.
//      //La creamos con seguridad porque ya verificamos que no hay conflicto en IMPLEMENTATION.
//      fun := AddInline(procName, retType, srcPos, pars, CallFunctParam, CallFunctCall);
//      //Un caso especial de proced. declarado solo en IMPLEMENTATION.
//      fun.location := locImplement;
//    end;
//  end;
//  locMain: begin
//    //Es una compilación en el programa principal. ¿Y si es FORWARD?
//    ReadInlineHeader(procName, retType, srcPos, pars);  //Procesa el encabezado
//    if HayError then exit;
//    if astProg.InlineExistInCur(procName, pars) then begin
//      GenErrorPos(ER_DUPLIC_FUNC_,[procName], srcPos);
//      exit;
//    end;
//    fun := AddInline(procName, retType, srcPos, pars, CallFunctParam, CallFunctCall);
//    //Aquí estamos en el entorno de la función.
//    fun.location := locMain;
//  end
//  else
//    GenError(ER_NOT_IMPLEM_, ['locMain in TCompMain.CompileInlineDeclar()']);
//  end;
//  //Aquí ya se tiene "fun" abierta, validada y apuntando a la declaración.
//  //Empiezan las declaraciones VAR, CONST, PROCEDURE, TYPE
//  while StartOfSection do begin
//    if tokL = 'var' then begin
//      Next;    //lo toma
//      while not StartOfSection and (tokL <>'begin') do begin
//        AnalyzeVarDeclar;
//        if HayError then exit;;
//      end;
//    end else if tokL = 'const' then begin
//      Next;    //lo toma
//      while not StartOfSection and (tokL <>'begin') do begin
//        AnalyzeConstDeclar;
//        if HayError then exit;;
//      end;
////    end else if tokL = 'procedure' then begin
////      Next;    //lo toma
////      AnalyzeProcDeclar;
//    end else begin
//      GenError('Expected VAR, CONST or BEGIN.');
//      exit;
//    end;
//  end;
//  if tokL <> 'begin' then begin
//    GenError('Expected "begin", "var", "type" or "const".');
//    exit;
//  end;
//  //Ahora empieza el cuerpo de la función o las declaraciones
//  fun.posCtx := PosAct;  //Guarda posición para la segunda compilación
//  bod := CreateBody;   //crea elemento del cuerpo de la función
//  bod.srcDec := GetSrcPos;
//  astProg.AddElementAndOpen(bod);  //Abre nodo Body
//  CompileInlineBody(fun);
//  astProg.CloseElement;  //Cierra Nodo Body
//  astProg.CloseElement; //cierra espacio de nombres de la función
//  bod.srcEnd := GetSrcPos;  //Fin de cuerpo
////  fun.adrReturn := pic.iRam-1;  //Guarda dirección del i_RETURN
//  if not CaptureTok(';') then exit;
//  ProcComments;  //Quita espacios. Puede salir con error
end;

//Compilación de secciones
procedure TAnalyzer.DoAnalyze;
{Performs the Analysis (Lexical, syntactic and semantic).
Input: The current context.
Output: The AST.}
begin
  IsUnit := GetUnitDeclaration();
  if IsUnit then begin
    //DoAnalyzeUnit(astProg);
  end else begin
    ParseProgram;
  end;
  //TestAllConstructs;  //Llena el astProg con código de ejemplo
end;

constructor TAnalyzer.Create(msg0: TMessageManager);
begin
  //Crea componentes del compilador
  inherited Create(msg0);
  vParserASM_6502 := TParserAsm_6502.Create(msg0, lex);
  //callParseASMblock := @vParserASM_6502.ProcessASMblock;
  ejecProg := false;
end;

destructor TAnalyzer.Destroy;
begin
  vParserASM_6502.Destroy;
  inherited Destroy;
end;

//********************** CÓDIGO DE PRUEBA DEL NUEVO LEXER *****************************
procedure TAnalyzer.TestAllConstructs;
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
  begin
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

    // Añadir parámetro
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

