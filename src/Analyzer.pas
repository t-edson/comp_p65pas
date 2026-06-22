unit Analyzer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types, alexiaLex, CompBase,
  ParserASM_6502, CompGlobals, AstElemP65, AstTree, ASTunit, MirList;
type

  { TAnalyzer }
  TAnalyzer = class(TCompilerBase)
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
    function GetUnitDeclaration: boolean;
    function StartOfSection: boolean;
  protected  //Elements processing
    procedure AnalyzeInlineDeclar(elemLocat: TElemLocation);
    procedure DoAnalyzeUnit(uni: TASTNode);
    procedure DoAnalyzeProgram;
    procedure DoAnalyze;
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
//    ParentElems := ast.curNode.elements;  //Para comparar
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
//      ast.OpenElement(fun);  //Abre el nodo anterior
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
//    if ast.InlineExistInCur(procName, pars) then begin
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
//  ast.AddElementAndOpen(bod);  //Abre nodo Body
//  CompileInlineBody(fun);
//  ast.CloseElement;  //Cierra Nodo Body
//  ast.CloseElement; //cierra espacio de nombres de la función
//  bod.srcEnd := GetSrcPos;  //Fin de cuerpo
////  fun.adrReturn := pic.iRam-1;  //Guarda dirección del i_RETURN
//  if not CaptureTok(';') then exit;
//  ProcComments;  //Quita espacios. Puede salir con error
end;

function TAnalyzer.GetUnitDeclaration: boolean;
{Indica si el archivo del contexto actual, es una unidad. Debe llamarse al inico de la
exploración del archivo.}
begin
  //Salta blancos sin ejecutar directivas
  SkipWhitesNoDirect;
  //Busca UNIT
  if lowercase(lex.token) = 'unit' then begin
    lex.curCtx.StartScan;   //retorna al inicio
    exit(true);
  end;
  lex.curCtx.StartScan;   //retorna al inicio
  exit(false);
end;
//Compilación de secciones
procedure TAnalyzer.DoAnalyzeUnit(uni: TASTNode);
{Realiza la compilación de una unidad}
var
  elem: TAstElement;
  fundec: TAstFunDec;
  tokL: String;
begin
{//debugln('   Ini Unit: %s-%s',[ast.curNode.name, ExtractFIleName(curCon.fileSrc)]);
  ClearError;
  ProcComments;
  //Busca UNIT
  if lowercase(lex.token) = 'unit' then begin
    lex.Next;  //pasa al nombre
    ProcComments;
    if lex.atEof then begin
      GenError('Name of unit expected.');
      exit;
    end;
    if UpCase(lex.token)<>uni.uname then begin
      GenError('Name of unit doesn''t match file name.');
      exit;
    end;
    lex.Next;  //Toma el nombre y pasa al siguiente
    if not CaptureSemicolon then exit;
  end else begin
    GenError('Expected: UNIT');
    exit;
  end;
  ProcComments;
  if lowercase(lex.token) <> 'interface' then begin
    GenError('Expected: INTERFACE');
    exit;
  end;
  lex.Next;   //toma
  ProcComments;
  lex.curLocation := locInterface;
  if lex.atEof then begin
    GenError('Expected "uses", "var", "type", "const" or "implementation".');
    exit;
  end;
  ProcComments;
  //Busca USES
  AnalyzeUsesDeclaration;
  if lex.atEof then begin
    GenError('Expected "var", "type" or "const".');
    exit;
  end;
  lex.curLocation := locInterface;  //Restore right location
  ProcComments;
//  Cod_StartProgram;  //Se pone antes de codificar procedimientos y funciones
  if HayError then exit;
  //Empiezan las declaraciones
  while StartOfSection do begin
    tokL := lowercase(lex.token);
    if tokL = 'var' then begin
      lex.Next;    //lo toma
      while not StartOfSection and (lowercase(lex.token) <>'implementation') do begin
        AnalyzeVarDeclar;  //marca como "IsInterface"
        if HayError then exit;;
      end;
    end else if tokL = 'type' then begin
      lex.Next;    //lo toma
      while not StartOfSection and (lowercase(lex.token) <>'implementation') do begin
        AnalyzeTypeDeclar(locInterface);
        if HayError then exit;
      end;
    end else if tokL = 'const' then begin
      lex.Next;    //lo toma
      while not StartOfSection and (lowercase(lex.token)<>'implementation') do begin
        AnalyzeConstDeclar;
        if HayError then exit;;
      end;
    end else if tokL = 'procedure' then begin
      lex.Next;    //lo toma
      AnalyzeProcDeclar(nil);
      if HayError then exit;
    end else begin
      GenError(ER_NOT_IMPLEM_, [lex.token]);
      exit;
    end;
  end;
  ProcComments;
  if lowercase(lex.token) <> 'implementation' then begin
    GenError('Expected: IMPLEMENTATION');
    exit;
  end;
  lex.Next;   //toma
  /////////////////  IMPLEMENTATION /////////////////////
  ProcComments;
  //Explora las declaraciones e implementaciones
  lex.curLocation := locImplement;
  //Empiezan las declaraciones
  while StartOfSection do begin
    tokL := lowercase(lex.token);
    if tokL = 'var' then begin
      lex.Next;    //lo toma
      while not StartOfSection and (tokL <>'end') do begin
        AnalyzeVarDeclar;
        if HayError then exit;;
      end;
    end else if tokL = 'const' then begin
      lex.Next;    //lo toma
      while not StartOfSection and (lowercase(lex.token) <>'end') do begin
        AnalyzeConstDeclar;
        if HayError then exit;;
      end;
    end else if tokL = 'procedure' then begin
      lex.Next;    //lo toma
      AnalyzeProcDeclar(nil);  //Compila en IMPLEMENTATION
      if HayError then exit;
    end else begin
      GenError(ER_NOT_IMPLEM_, [lex.token]);
      exit;
    end;
  end;
  //Verifica si todas las funciones de INTERFACE, se implementaron
  for elem in ast.curNode.elements do if elem.idClass = eleFuncDec then begin
    fundec := TAstFunDec(elem);
    if fundec.BodyNode = nil then begin  //Sin cuerpo. Debe ser FORWARD.
      if fundec.implem = nil then begin
        GenError('Function %s not implemented.', [fundec.name], fundec.srcDec);
        exit;
      end;
    end;
  end;
  CompileLastEnd;
  if HayError then exit;
//  //procesa cuerpo
//  ResetRAM;  {No es tan necesario, pero para seguir un orden y tener limpio
//                     también, la flash y memoria, después de algún psoible procedimiento.}
//  if tokL = 'begin' then begin
//    bod := CreateBody;
//    bod.srcDec := GetSrcPos;
//    Next;   //coge "begin"
//    //Guardamos la ubicación física, real, en el archivo, después del BEGIN
//    bod.posCtx := PosAct;
//    //codifica el contenido
//    AnalyzeCurBlock;   //compila el cuerpo
//    if HayError then exit;
}end;
procedure TAnalyzer.DoAnalyzeProgram;
{Performs the Analysis (Lexical, syntactic and semantic).
Input: The current context.
Output: The AST.}
var
  bod: TAstBody;
  elem: TAstElement;
  fundec: TAstFunDec;
  tokL: String;
begin
{  ClearError;
  ProcComments;
  //Busca PROGRAM
  tokL := lowercase(lex.token);
  if tokL = 'unit' then begin
    //Se intenta compilar una unidad
    GenError('Expected a program. No a unit.');
    exit;
  end else if tokL = 'program' then begin
    lex.Next;  //pasa al nombre
    ProcComments;
    if lex.atEof then begin
      GenError(ER_PROG_NAM_EX);
      exit;
    end;
    lex.Next;  //Toma el nombre y pasa al siguiente
    if not CaptureSemicolon then exit;
  end;
  if lex.atEof then begin
    GenError('Expected "program", "begin", "var", "type" or "const".');
    exit;
  end;
  ProcComments;
  //Busca USES
  if HayError then exit;  //AnalyzeUsesDeclaration, va a limpiar "HayError"
  AnalyzeUsesDeclaration;
  if lex.atEof then begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  ProcComments;
  {*** De momento, se comenta
  callStartProgram;  //Se pone antes de codificar procedimientos y funciones
  }
  lex.curLocation := locMain;
  if HayError then exit;
  //Empiezan las declaraciones
  while StartOfSection do begin
    tokL := lowercase(lex.token);
    if tokL = 'var' then begin
      lex.Next;    //lo toma
      while not StartOfSection and (lowercase(lex.token) <>'begin') do begin
        AnalyzeVarDeclar;
        if HayError then exit;
      end;
    end else if tokL = 'type' then begin
      lex.Next;    //lo toma
      while not StartOfSection and (lowercase(lex.token) <>'begin') do begin
        AnalyzeTypeDeclar(locMain);
        if HayError then exit;
      end;
    end else if tokL = 'const' then begin
      lex.Next;    //lo toma
      while not StartOfSection and (lowercase(lex.token) <>'begin') do begin
        AnalyzeConstDeclar;
        if HayError then exit;
      end;
    end else if tokL = 'procedure' then begin
      lex.Next;    //lo toma
      AnalyzeProcDeclar(nil);
      if HayError then exit;
    end else if tokL = 'inline' then begin
      lex.Next;    //lo toma
      AnalyzeInlineDeclar(locMain);
      if HayError then exit;
    end else begin
      GenError(ER_NOT_IMPLEM_, [lex.token]);
      exit;
    end;
  end;
  //Procesa cuerpo
  if Upcase(lex.token) <> 'BEGIN' then begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  bod := ast.AddBodyAndOpen(lex.GetSrcPos);  //Abre nodo Body
  lex.Next;   //Takes "BEGIN"
  AnalyzeCurBlock;   //Compiles the body
  ast.CloseElement;   //No debería ser tan necesario.
  bod.srcEnd := lex.GetSrcPos;
  if HayError then exit;
  //Verifica si todas las funciones FORWARD, se implementaron
  for elem in ast.curNode.elements do if elem.idClass = eleFuncDec then begin
    fundec := TAstFunDec(elem);
    if fundec.BodyNode = nil then begin  //Sin cuerpo. Debe ser FORWARD.
      if fundec.implem = nil then begin
        GenError('Function %s not implemented.', [fundec.name], fundec.srcDec);
        exit;
      end;
    end;
  end;
  CompileLastEnd;  //Compila el "END." final
  if HayError then exit;
  //_RTS();   //agrega instrucción final
  callEndProgram;
}end;
procedure TAnalyzer.DoAnalyze;
{Performs the Analysis (Lexical, syntactic and semantic).
Input: The current context.
Output: The AST.}
begin
  if IsUnit then begin
    DoAnalyzeUnit(ast);
  end else begin
    //DoAnalyzeProgram;    //puede dar error
    ParseProgram;
  end;
  //TestAllConstructs;  //Llena el AST con código de ejemplo
end;

//********************** CÓDIGO DE PRUEBA DEL NUEVO LEXER *****************************
procedure TAnalyzer.TestAllConstructs;
var
  SrcPos: TSrcPos;
  VarDeclX, VarDeclY: TVarDecl;
  Proc: TProcDecl;
  Func: TFunctionDecl;
  Assign1, Assign2: TAssignment;
  VarRef1, VarRef2: TVariableRef;
  Literal1, Literal2: TNumberLiteral;
  Param: TVarDecl;
  ProcBody, FuncBody: TBlock;
  begin
    ast.Clear;
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
    ast.AddGlobalDecl(VarDeclX);

    SrcPos.row := 3;
    SrcPos.col := 7;
    VarDeclY := TVarDecl.Create('y', 'byte', SrcPos);
    ast.AddGlobalDecl(VarDeclY);

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

    ast.AddProcedure(Proc);

    // ============================================================
    // 3. Declarar función: function Calcular: integer;
    // ============================================================
    SrcPos.row := 8;
    SrcPos.col := 1;
    Func := TFunctionDecl.Create('Calcular', 'integer', SrcPos);

    // Cuerpo de la función (vacío)
    SrcPos.row := 9;
    SrcPos.col := 3;
    FuncBody := TBlock.Create(SrcPos);
    Func.Body := FuncBody;

    ast.AddFunction(Func);

    // ============================================================
    // 4. Cuerpo principal: x := 1; y := 2;
    // ============================================================
    // NOTA: ast.MainBody ya existe, solo añadimos instrucciones

    // x := 1;
    SrcPos.row := 12;
    SrcPos.col := 3;
    VarRef1 := TVariableRef.Create('x', SrcPos);
    Literal1 := TNumberLiteral.Create(1, SrcPos);
    Assign1 := TAssignment.Create(VarRef1, Literal1, SrcPos);
    ast.MainBody.AddStatement(Assign1);

    // y := 2;
    SrcPos.row := 13;
    SrcPos.col := 3;
    VarRef2 := TVariableRef.Create('y', SrcPos);
    Literal2 := TNumberLiteral.Create(2, SrcPos);
    Assign2 := TAssignment.Create(VarRef2, Literal2, SrcPos);
    ast.MainBody.AddStatement(Assign2);

    // ============================================================
    // 5. Imprimir el AST
    // ============================================================
    WriteLn('=== AST DEL PROGRAMA ===');
    ast.PrintDebug;

    WriteLn;
    WriteLn('Presiona Enter para salir...');
//    ReadLn;
  end;

end.

