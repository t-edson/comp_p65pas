{Unit that defines the compiler building it from its parts.

}
unit Compiler_PIC16;
{$mode objfpc}{$H+}{$modeswitch functionreferences}{$modeswitch anonymousfunctions}
interface
uses
  Classes, SysUtils, LazLogger, P65C02utils, CPUCore, CompBase, ParserDirec,
  GenCodBas_PIC16, GenCod_PIC16, CompGlobals, AstElemP65, ParserASM_6502,
  MirList, LexPas, SIF_P65pas, StrUtils;
type
  { TCompiler_PIC16 }
  TCompiler_PIC16 = class(TGenCod)
  private  //Funciones básicas
    addBootldr: integer;  //Address where start Bootloader.
    addVariab : integer;  //Address where start Variables section.
    addFuncts : integer;  //Address where start function section.
//    procedure ConstantFoldExpr(eleExp: TEleExpress);
    procedure AddParam(var pars: TAstParamArray; parName: string;
      const srcPos: TSrcPos; typ0: TEleTypeDec; adicDec: TAdicDeclar);
    function AddSIFtoUnit(name: string; retType: TEleTypeDec;
      const srcPos: TSrcPos; const pars: TAstParamArray; codSys: TCodSysInline
      ): TEleFunDec;
    function AddSNFtoUnit(name: string; retType: TEleTypeDec;
      const srcPos: TSrcPos; var pars: TAstParamArray; codSys: TCodSysNormal
      ): TEleFunDec;
    procedure bool_LoadToRT(fun: TEleExpress);
    procedure byte_DefineRegisters;
    procedure byte_LoadToWR(fun: TMirOperand);
    procedure byte_SaveToStk;
    procedure CreateBooleanOperations;
    procedure CreateByteOperations;
    procedure CreateCharOperations;
    function CreateInBOMethod(clsType: TEleTypeDec; opr: string; name: string;
      parType: TEleTypeDec; retType: TEleTypeDec; pCompile: TCodSysInline
      ): TEleFunDec;
    function CreateInTerMethod(clsType: TEleTypeDec; name: string; parType1,
      parType2: TEleTypeDec; retType: TEleTypeDec; pCompile: TCodSysInline
      ): TEleFunDec;
    function CreateInUOMethod(clsType: TEleTypeDec; opr: string; name: string;
      retType: TEleTypeDec; pCompile: TCodSysInline; operTyp: TOperatorType =
      opkUnaryPre): TEleFunDec;
    procedure CreateSystemTypesAndVars;
    procedure CreateWordOperations;
    procedure DefineArray(etyp: TEleTypeDec);
    procedure DefineObject(etyp: TEleTypeDec);
    procedure DefinePointer(etyp: TEleTypeDec);
    procedure GenerateMIR;
    function PICName: string;
    procedure word_DefineRegisters;
    procedure word_High(var fun: TMirOperand);
    procedure word_LoadToWR(fun: TMirOperand);
    procedure word_Low(var fun: TMirOperand);
    procedure word_RequireWR;
    procedure word_SaveToStk;
//    procedure PrepareBody(cntBody, sntBlock: TEleCodeCont);
//    procedure PrepareSentences;
    procedure CreateVarsAndPars;
    procedure GenCodeSentences(sentList: TAstElements);
    procedure GenCodeSentences2(sentList: TMirElements);
    procedure GenCodeBlock(block: TEleBlock);

  private  //Compilers steps
//    procedure EvaluateConstantDeclare;
//    procedure ConstantFolding;
//    procedure ConstanPropagation;
    procedure DoOptimize;
    procedure DoGenerateCode;
    procedure DoCompile;
  public      //Events
    OnAfterCompile: procedure of object;   //Al finalizar la compilación.
  public      //Override methods
    procedure DumpCode(lins: TSTrings); override;
    function RAMusedStr: string; override;
    procedure GetResourcesUsed(out ramUse, romUse, stkUse: single); override;
    procedure GenerateListReport(lins: TStrings); override;
  public      //Interfaz for IDE
    procedure Exec(srcFile, outFile: string; pars: string);
  public      //Inicialización
    procedure CreateSystemUnitInAST;
    constructor Create; override;
    destructor Destroy; override;
  end;

procedure SetLanguage;

implementation
var
  ER_DUPLIC_IDEN, ER_NOT_IMPLEM_, ER_IDEN_EXPECT, ER_INVAL_FLOAT: string;
  ER_ERR_IN_NUMB, ER_UNDEF_TYPE_, ER_EXP_VAR_IDE, ER_BIT_VAR_REF: String;
  ER_UNKNOWN_ID_, ER_DUPLIC_FUNC_, ER_IDE_TYP_EXP : String;
  ER_COMPIL_PROC, ER_CON_EXP_EXP: String;
  ER_FIL_NOFOUND, WA_UNUSED_CON_, WA_UNUSED_PRO_: String;
  MSG_RAM_USED, MSG_FLS_USED: String;

  //Funciones básicas
procedure SetLanguage;
begin
//  GenCod_PIC16.SetLanguage;
//  ParserASM_6502.SetLanguage;
  SIF_P65pas.SetLanguage;
  {$I _language\tra_Compiler.pas}
end;
//procedure TCompiler_PIC16.ConstantFoldExpr(eleExp: TEleExpress);
//{Performs:
//- Constant evaluation, for constant nodes that can be evaluated.
//- Constant folding, for expression nodes, that returns constants.
//Note the similarity of this method with GenCodeExpr().}
//var
//  funcBase: TEleFunDec;
//  ele: TAstElement;
//  parExpr: TEleExpress;
//begin
//  if eleExp.opType = otFunct then begin
//    //It's an expression. There should be a function
//    funcBase := eleExp.fundec;
//    if funcBase.callType = ctSysInline then begin
//      //Only INLINE functions can returns constants.
//      if funcBase.idClass = eleFuncDec then begin
//        //It's the declaration. No problem.
//        //Process all parameters.
//        for ele in eleExp.elements do begin
//          parExpr := TEleExpress(ele);
//          ConstantFoldExpr(parExpr);  //Try to evaluate constant.
//          if HayError then exit;
//        end;
//        { TODO : Tal vez sea posible que por optimización, solo llamar a
//        funcBase.codSysInline() cuando los parámetros (o alguno) sean constante,
//        para evitar muchas llamadas }
//        funcBase.codSysInline(eleExp);  //We don't expect this generates code.
//        //Check if we can simplify
//        if eleExp.opType = otConst then begin
//          //Node resulted as a constant.
//          if eleExp.evaluated then begin
//            //We convert it to a simple constant (Constant fold).
//            eleExp.elements.Clear;  //Constants don't have childrens.
//          end else begin
//            //Maybe this constant depend of RAM assigment
//          end;
//        end else if eleExp.opType = otVariab then begin
//          eleExp.elements.Clear;  //Variables don't have childrens.
//        end;
//      end else begin
//        { Los SIF no soportan implementación separada.}
//        GenError('No supported implementing System INLINE functions.');
//      end;
//    end else begin
//      //In Normal subroutine, we scan for parameters
//      //Process all parameters.
//      for ele in eleExp.elements do begin
//        parExpr := TEleExpress(ele);
//        ConstantFoldExpr(parExpr);  //Try to evaluate constant.
//        if HayError then exit;
//      end;
//      { TODO : ¿No debería llamarse también a functCall(). Allí también se genera código.? }
//    end;
//  end else if eleExp.opType = otConst then begin
//    eleExp.Evaluate();   //Try to evaluate
//    //Some constants like @variable or @function cannot be evaluated in optimization
//    //if not eleExp.evaluated then begin
//    //  GenError('Constant not evaluated.', eleExp.srcDec);
//    //  exit;
//    //end;
//  end;
//end;
//procedure TCompiler_PIC16.EvaluateConstantDeclare;
//{Calculates final values from constant declared in CONST sections of program and
//functions. Constants could be expressions:
//  const
//    C1 = 123;
//    C2 = C1 and $FF;
//}
//var
//  cons: TEleConsDec;
//  vard: TEleVarDec;
//  consExpres: TEleExpress;
//begin
//  //Calculate values in CONST sections
//  for cons in TreeElems.AllCons do begin
//    {For optimization we should evaluate only used Constant, but we evaluate alls
//    because constants like the field "length" of arrays, needs to be evaluated in order
//    to get the size defined, before assign RAM. }
//    //if vard.nCalled = 0 then continue; //Skip unused variable.
//     TreeElems.OpenElement(cons);  //To resolve properly identifiers
//     if not cons.evaluated then begin
//       //If it isn't evaluated, must be an expression.
//       consExpres := TEleExpress(cons.elements[0]);  //Takes the expression node.
//       //Should be an expression. Need to be calculated.
//       ConstantFoldExpr(consExpres);
//       if HayError then exit;
//       if consExpres.opType<>otConst then begin
//         //The expression returned a not-constant value.
//         GenError('Expected constant expression.', consExpres.srcDec);
//         exit;
//       end;
//       if not consExpres.evaluated then begin
//         GenError('Constant not evaluated.', consExpres.srcDec);
//         exit;
//       end;
//       //Copy the value.
//       cons.value := @consExpres.value;
//       cons.evaluated := true;
//     end;
//  end;
//  //Calculate values in VAR sections
//  for vard in TreeElems.AllVars do begin
//    if vard.nCalled = 0 then continue; //Skip unused variable.
//    if vard.elements.Count = 0 then continue;  //Skip vars with no initialization.
//    TreeElems.OpenElement(vard);  //To resolve properly identifiers
//    consExpres := TEleExpress(vard.elements[0]);  //Takes the expression node.
//    if not consExpres.evaluated then begin
//      //If it isn't evaluated, must be an expression.
//      //Should be an expression. Need to be calculated.
//      ConstantFoldExpr(consExpres);
//      if HayError then exit;
//      if consExpres.opType<>otConst then begin
//        //The expression returned a not-constant value.
//        GenError('Expected constant expression.', consExpres.srcDec);
//        exit;
//      end;
//      if not consExpres.evaluated then begin
//        GenError('Constant not evaluated.', consExpres.srcDec);
//        exit;
//      end;
//      //Copy the value.
//      consExpres.value := consExpres.value;
//      consExpres.evaluated := true;
//    end;
//  end;
//end;
//procedure TCompiler_PIC16.ConstantFolding;
//{Do a fold constant optimization and evaluate constant expresions. }
//  procedure ConstantFoldBody(body: TEleBody);
//  {Do constant folding simplification in all expression nodes. Note the similarity with
//  TGenCodeBas.GenCodeSentences(), for scanning the AST.
//  Constant fold are done only if constant are in the same Node. It is:
//
//    <constant> + <constant> + <variable>
//
//  Cases like:
//
//    <constant> + <variable> + <constant>
//
//  Won't be folded because constant will be located in the AST in different nodes.
//  }
//  var
//    expFun: TEleExpress;
//    sen: TEleSentence;
//    eleSen: TAstElement;
//    ele: TAstElement;
//    i: Integer;
//  begin
//    //Process body
//    TreeElems.OpenElement(body);
//    for eleSen in TreeElems.curNode.elements do begin
//      if eleSen.idClass <> eleSenten then continue;
//      sen := TEleSentence(eleSen);
//      if sen.sntType = sntAssign then begin    //assignment
//        {After preparation, assignment sentences could be splitted in several assignment.}
//        for ele in sen.elements do begin
//          expFun := TEleExpress(ele);
//          ConstantFoldExpr(expFun);  //Try to evaluate constant.
//          if HayError then exit;
//        end;
//      end else if sen.sntType = sntProcCal then begin
//        {After preparation, sntProcCal sentences could be include several
//        assignment before the Call.}
//        for i:=0 to sen.elements.Count-2 do begin  //Excluding the call
//          expFun := TEleExpress(sen.elements[i]);
//          ConstantFoldExpr(expFun);  //Try to evaluate constant.
//          if HayError then exit;
//        end;
//      end;
//    end;
//    TreeElems.CloseElement;              //Close the Body.
//  end;
//var
//  fun : TEleFunDec;
//  bod: TEleBody;
//begin
//  compMod := cmConsEval;    //Mode Constant evaluation.
//  pic.disableCodegen := true;  //Disable the code generation
//  pic.iRam := 0;  //Clear RAM position
//  //Code subroutines
//  for fun in usedFuncs do begin
//    if fun.codSysInline <> nil then continue;  //No INLINE
//    ConstantFoldBody(fun.bodyImplem);
//    if HayError then exit;   //Puede haber error
//  end;
//  //Code body
//  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
//  ConstantFoldBody(bod);
//end;
//procedure TCompiler_PIC16.ConstanPropagation;
//{Do a constant propagation optimization. }
//  function IsForm_var_assign_const(assigExp: TEleExpress;
//        out varDec: TEleVarDec;
//        out consVal: TConsValue
//        ): boolean;
//  {Indicates if the assigment expression is of the form:
//     <variable> := <constant>
//  }
//  var
//    leftOp, rightOp: TEleExpress;
//  begin
//    if assigExp.opType <> otFunct then exit(false);  //Validation
//    //Must be an assignment expression
//    leftOp := TEleExpress(assigExp.elements[0]);
//    rightOp := TEleExpress(assigExp.elements[1]);
//    if (rightOp.Sto = stConst) and rightOp.evaluated then begin
//       //It's the form: <variable> := <constant>
//      varDec := leftOp.vardec;  //Takes var declaration
//      consVal := rightOp.value;
//      exit(true);
//    end;
//  end;
//  function ChangeToConstant(Op: TEleExpress; varDec: TEleVarDec; const consVal: TConsValue): boolean;
//  {Test if the operand is a variable refering to "varDec". If so, change it to a constant
//  type, set to "consVal" and returns TRUE. }
//  begin
//    if Op.opType <> otVariab then exit(false);
//    if varDec.IsParameter then exit(false);
//    if (Op.vardec = varDec) then begin
//      Op.opType := otConst;
//      Op.Sto := stConst;
//      Op.evaluated := true;
//      Op.value := consVal;
//      exit(True);
//    end else begin
//      exit(False);
//    end;
//  end;
//  function ReplaceVarByConst(assigExp: TEleExpress; varDec: TEleVarDec;
//        const consVal: TConsValue): boolean;
//  {Replace the variable by a constant in the right part of an assignment expression.
//  It replacing is done, returns TRUE.
//  }
//  var
//    rightOp, Op: TEleExpress;
//    ele: TAstElement;
//  begin
//    //Search at the right expression
//    Result := false;
//    rightOp := TEleExpress(assigExp.elements[1]);
//    case rightOp.opType of
//    otConst: begin
//      //Nothing to replace
//    end;
//    otVariab: begin
//      //Could be
//      if ChangeToConstant(rightOp, varDec, consVal) then begin
//        Result := True;
//      end;
//    end;
//    otFunct: begin
//      {Check for all of the operands of the function. Normal function at this level,
//      won't have child nodes.}
//      for ele in rightOp.elements do begin
//        Op := TEleExpress(ele);
//        if ChangeToConstant(Op, varDec, consVal) then begin
//          Result := True;
//        end;
//      end;
//    end;
//    end;
//  end;
//  procedure ConstantPropagBody(body: TEleBody);
//  {Do constant propagation in all sentences of the body. }
//  var
//    assigExp, assigToDelete: TEleExpress;
//    sen: TEleSentence;
//    eleSen, par: TAstElement;
//    n, i: Integer;
//    varDec, varDecToDelete: TEleVarDec;
//    consVal: TConsValue;
//    replaceMode, replaced: Boolean;
//  begin
//    //Process body
//    TreeElems.OpenElement(body);
//    for eleSen in TreeElems.curNode.elements do begin
//      if eleSen.idClass <> eleSenten then continue;
//      sen := TEleSentence(eleSen);
//      if sen.sntType = sntAssign then begin    //Assignment
//        //Explore previous posssible assigments
//        n := sen.elements.Count;
//        replaceMode := false;
//        replaced := false;
//        for i:=0 to n - 1 do begin
//          //Search the type: <variable> := <constant>
//          assigExp := TEleExpress(sen.elements[i]);  //Shoudn't fail
//          if replaceMode then begin
//            if ReplaceVarByConst(assigExp, varDec, consVal) then begin
//              //Replaced here.
//              replaced := true;
//              debugln('Constant propagation applied to: ' + varDecToDelete.name);
//            end;
//          end else begin
//            //Find mode. Finding the form: <variable> := <constant>
//            if i = n-1 then continue;  //The last assigment is unuseful for replacing.
//            if IsForm_var_assign_const(assigExp, varDec, consVal) then begin
//              replaceMode := true;  //Enter to mode replace for following sentences
//              assigToDelete := assigExp;  //This assigment will be deleted after replaced.
//              varDecToDelete := varDec;
//            end;
//          end;
//        end;
//        //Delete assigment
//        {Formally we can delete an assigment of the form: <variable> := <constant>
//        even if it hasn't been replaced. In that case it will mean that <variable> is
//        not used (if it cannot be replaced is other problem but we assume it can be
//        replaced in all cases.).}
//        if replaced then begin
//          assigToDelete.elements.Clear;
//          par := assigToDelete.Parent;
//          par.elements.Remove(assigToDelete);
//          //Delete references, because we've deleted the only one remained existent.
//          varDecToDelete.lstCallers.Clear;
//          //We can even delete the variable declaration if it's no used from other sentences.
//        end;
//      end;
//    end;
//    TreeElems.CloseElement;              //Close the Body.
//  end;
//var
//  fun : TEleFunDec;
//  bod: TEleBody;
//begin
//  compMod := cmConsEval;    //Generates code.
//  pic.disableCodegen := true;  //Disable the code generation
//  pic.iRam := 0;  //Clear RAM position
//  //Code subroutines
//  for fun in usedFuncs do begin
//    if fun.codSysInline <> nil then continue;  //No INLINE
//    ConstantPropagBody(fun.bodyImplem);
//    if HayError then exit;   //Puede haber error
//  end;
//  //Code body
//  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
//  ConstantPropagBody(bod);
//end;
//procedure TCompiler_PIC16.PrepareBody(cntBody, sntBlock: TEleCodeCont);
//{Do a separation for assigmente sentences in order to have the "three-address code" form
//like used in several compilers.
//Parameters:
//  cntBody  -> The main Body of a procedure or the main program. This will be used
//               as reference to locate the new variable declarations.
//  sntBlock  -> Block of code where are the sentences to need be prepared. It's the
//               same of "container" except when "block" is nested like in a condiitonal.
//}
//  function MoveParamToAssign(curContainer: TAstElement; Op: TEleExpress;
//                             parvar: TEleVarDec): TEleExpress;
//  {Mueve el nodo especificado "Op", que representa a un parámetro de la función, a una
//  nueva instruccion de asignación (que es creada al inicio del bloque "curContainer") y
//  reemplaza el nodo faltante por una variable temporal que es la que se crea en la
//  instrucción de asignación.
//  Es similar a MoveNodeToAssign() pero no reemplaza el nodo movido y no crea una variable
//  auxiliar, sino que usa "parvar".
//  Retorna la instrucción de asignación creada.
//  }
//  var
//    _setaux: TEleExpress;
//    Op1aux: TEleExpress;
//    funSet: TEleFunDec;
//  begin
//    //Create the new _set() expression.
//    _setaux := CreateExpression('_set', typNull, otFunct, Op.srcDec);
//    funSet := MethodFromBinOperator(Op.Typ, ':=', Op.Typ);
//    if funSet = nil then begin   //Operator not found
//      GenError('Undefined operation: %s %s %s', [Op.Typ.name, ':=', Op.Typ.name], Op.srcDec);
//      _setaux.Destroy;    //We destroy because it hasn't been included in the AST.
//      exit(nil);
//    end;
//    _setaux.fundec := funSet;
//
//    //Add the new assigment before the main
//    TreeElems.openElement(curContainer);
//    TreeElems.AddElement(_setaux, 0);    //Add a new assigmente before
//    TreeElems.openElement(_setaux);
//
//    //Add first operand (variable) of the assignment.
//    Op1aux := CreateExpression(parvar.name, parvar.typ, otVariab, Op.srcDec);
//    SetVariabCA(Op1aux, parvar);
//    TreeElems.addElement(Op1aux);
//    AddCallerToFromCurr(parvar); //Add reference to auxiliar variable.
//
//    //Move the second operand to the previous _set created
//    TreeElems.ChangeParentTo(_setaux, Op);
//
//    exit(_setaux);
//  end;
//  function SplitProcCall(curContainer: TAstElement; expMethod: TEleExpress): boolean; forward;
//  function SplitSet(curContainer: TAstElement; setMethod: TAstElement): boolean;
//  {Verify if a set expression has more than three operands. If so then
//  it's splitted adding one or more aditional set sentences, at the beggining of
//  "curContainer".
//  If at least one new set sentence is added, returns TRUE.}
//  var
//    Op2, parExp, new_set, Op1, idx: TEleExpress;
//    par: TAstElement;
//  begin
//    Result := false;
//    if TEleExpress(setMethod).fundec.getset <> gsSetInSimple then exit;
//    //Split expressions in second operand of assignment.
//    Op2 := TEleExpress(setMethod.elements[1]);  //Takes assignment source.
//    if (Op2.opType = otFunct) then begin
//      //Op2 is a function.
//      if Op2.fundec.callType in [ctSysNormal, ctUsrNormal] then begin  //Normal function
//        {IT's the form:
//             x := func(x,y);
//                  \_______/
//                     Op2
//        }
//        //Generates an asignment for each parameter.
//        SplitProcCall(curContainer, Op2);
//      end else if Op2.fundec.callType = ctSysInline then begin       //INLINE function
//        {IT's the form:
//             x := A + B
//                  \___/
//                   Op2
//        or:
//             x := A++        }
//        {We expect parameters A, B should be simple operands (Constant or variables)
//        otherwise we will move them to a separate assignment}
//        for par in Op2.elements do begin
//          parExp := TEleExpress(par);
//          if parExp.opType = otFunct then begin
//            new_set := MoveNodeToAssign(cntBody, curContainer, parExp);
//            if HayError then exit;
//            SplitSet(curContainer, new_set);  //Check if it's needed split the new _set() created.
//            Result := true;
//          end;
//        end;
//      end;
//    end;
//  end;
//  function SplitExpress(curContainer: TAstElement; expMethod: TEleExpress): boolean;
//  {Verify if an expression has more than three operands. If so then
//  it's splitted adding one or more set sentences.
//  If at least one new set sentence is added, returns TRUE.}
//  var
//    parExp, new_set: TEleExpress;
//    par: TAstElement;
//  begin
//    Result := false;
//    if (expMethod.opType = otFunct) then begin  //Neither variables nor constants.
//      {We expect parameters should be simple operands (Constant or variables)
//      otherwise we will move them to a separate assignment}
//      if expMethod.fundec.callType in [ctSysNormal, ctUsrNormal] then begin  //Normal function
//
//        //Generates an asignment for each parameter.
//        SplitProcCall(curContainer, expMethod);
//      end else if expMethod.fundec.callType = ctSysInline then begin  //Like =, >, and, ...
//        for par in expMethod.elements do begin
//          parExp := TEleExpress(par);
//          if parExp.opType = otFunct then begin
//            new_set := MoveNodeToAssign(cntBody, curContainer, parExp);
//            if HayError then exit;
//            SplitSet(curContainer, new_set);  //Check if it's needed split the new _set() created.
//            Result := true;
//          end;
//        end;
//      end;
//    end;
//  end;
//  function SplitProcCall(curContainer: TAstElement; expMethod: TEleExpress): boolean;
//  {Split a procedure (not INLINE) call instruction, inserting an assignment instruction
//  for each parameter.}
//  var
//    parExp, new_set: TEleExpress;
//    funcBase: TEleFunDec;
//    ipar: Integer;
//    par: TAstParam;
//  begin
//    Result := false;
//    if expMethod.opType <> otFunct then exit;   //Not a fucntion call
//    funcBase := expMethod.fundec;    //Base function reference
//    if funcBase.codSysInline=nil then begin   //Not INLINE
//      {Move all parameters (children nodes) to a separate assignment}
//      ipar := 0;  //Parameter index
//      while expMethod.elements.Count>0 do begin  //While remain parameters.
//        parExp := TEleExpress(expMethod.elements[0]);  //Take parameter element
//        par := funcBase.pars[ipar];
//        new_set := MoveParamToAssign(curContainer, parExp, par.vardec);
//        if HayError then exit;
//        SplitSet(curContainer, new_set);  //Check if it's needed split the new _set() created.
//        Result := true;
//        inc(ipar);
//      end;
//    end;
//  end;
//var
//  sen: TEleSentence;
//  eleSen, _set, ele, _proc: TAstElement;
//  _exp, Op1, Op2, val1, val2: TEleExpress;
//  _blk, _blk0: TEleCodeCont;
//begin
//  //Prepare assignments for arrays.
//  for eleSen in sntBlock.elements do begin
//    if eleSen.idClass <> eleSenten then continue;
//    //We have a sentence here.
//    sen := TEleSentence(eleSen);
//    if sen.sntType = sntAssign then begin  //Assignment
//      _set := sen.elements[0];  //Takes the _set method.
//      Op1 := TEleExpress(_set.elements[0]);  //Takes assigment target.
//      Op2 := TEleExpress(_set.elements[1]);  //Takes assigment target.
//      if (Op1.opType = otFunct) and (Op1.fundec.getset = gsGetInItem) then begin
//        //It's a _set() for a _getitem() INLINE assignment for array.
//        if Op1.fundec.funset = nil then begin
//          GenError('Cannot locate the setter for this type.');
//          exit;
//        end;
//        //Convert getter() to setter().
//        Op1.fundec := Op1.fundec.funset;     //Must be gsSetInItem
//        Op1.name := Op1.fundec.name;
//        Op1.Typ  := Op1.fundec.retType;
//        //Move third parameter to _setitem() and locate it at the Top
//        TreeElems.ChangeParentTo(Op1, Op2);
//        TreeElems.ChangeParentTo(Op1.Parent.Parent, Op1);
//        _set.Parent.elements.Remove(_set);
//      end else if (Op1.opType = otFunct) and (Op1.fundec.getset = gsGetInPtr) then begin
//        //It's a _set() for a _getptr() INLINE assignment for POINTER.
//        if Op1.fundec.funset = nil then begin
//          GenError('Cannot locate the setter for this type.');
//          exit;
//        end;
//        //Convert getter() to setter().
//        Op1.fundec := Op1.fundec.funset;     //Must be gsSetInPtr;
//        Op1.name := Op1.fundec.name;
//        Op1.Typ  := Op1.fundec.retType;
//        //Move third parameter to _setptr() and locate it at the Top
//        TreeElems.ChangeParentTo(Op1, Op2);
//        TreeElems.ChangeParentTo(Op1.Parent.Parent, Op1);
//        _set.Parent.elements.Remove(_set);
//      end;
//    end;
//  end;
//  //Prepare sentences
//  for eleSen in sntBlock.elements do begin
//    if eleSen.idClass <> eleSenten then continue;
//    //We have a sentence here.
//    sen := TEleSentence(eleSen);
//    if sen.sntType = sntAssign then begin  //Assignment
//      _set := sen.elements[0];  //Takes the one _set method.
//      SplitSet(sen, _set)  //Might generate additional assignments sentences
//    end else if sen.sntType = sntProcCal then begin  //Procedure call
//      _proc := sen.elements[0];  //Takes the proc.
//      SplitProcCall(sen, TEleExpress(_proc))
//    end else if sen.sntType in [sntIF, sntREPEAT, sntWHILE] then begin
//      //There are expressions and blocks inside conditions and loops.
//      for ele in sen.elements do begin
//        if ele.idClass = eleCondit then begin  //It's a condition
//          _exp := TEleExpress(ele.elements[0]);  //The first item is a TEleExpress
//          SplitExpress(ele, _exp)
//        end else if ele.idClass = eleBlock then begin   //body of IF
//          _blk := TEleCodeCont(ele);  //The first item is a TEleExpress
//          PrepareBody(cntBody, _blk);
//        end;
//      end;
//    end else if sen.sntType = sntFOR then begin
//      //FOR sentences could need some aditional changes.
//      _blk0 := nil;
//      for ele in sen.elements do begin
//        if ele.idClass = eleCondit then begin  //It's a condition
//          _exp := TEleExpress(ele.elements[0]);  //The first item is a TEleExpress
//          SplitExpress(ele, _exp)
//        end else if ele.idClass = eleBlock then begin   //Initialization or body
//          _blk := TEleCodeCont(ele);  //The first item is a TEleExpress
//          PrepareBody(cntBody, _blk);
//          if _blk0 = nil then _blk0 := _blk;  //Get intialization block
//        end;
//      end;
//      //Get first and last value of index.
//      val1 := TEleExpress(_blk0.elements[0].elements[1]);
//      val2 := TEleExpress(_exp.elements[1]);
//      //Special cases
//      if (val1.opType = otConst) and (val2.opType = otConst) then begin
//        if val1.val > val2.val then begin
//          //Unreachable code
////          TreeElems.DeleteTypeNode();
//        end;
//      end;
//    end else if sen.sntType = sntExit then begin
//      if sen.elements.Count>0 then begin   //If there is argument
//        _exp := TEleExpress(sen.elements[0]);  //The first item is a TEleExpress
//        SplitExpress(sen, _exp)
//      end;
//    end;
//  end;
//end;
//procedure TCompiler_PIC16.PrepareSentences;
//var
//  fun : TEleFunDec;
//  bod: TEleBody;
//begin
//  //Split subroutines
//  for fun in usedFuncs do begin
//    if fun.callType = ctUsrNormal then begin
//      PrepareBody(fun.bodyImplem, fun.bodyImplem);
//      if HayError then exit;   //Puede haber error
//    end;
//  end;
//  //Split body
//  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
//  PrepareBody(bod, bod);
//end;
procedure TCompiler_PIC16.CreateVarsAndPars;
{Create in RAM, local variables and parameters for functions.}
var
  elem   : TAstElement;
  xvar   : TEleVarDec;
  fun    : TEleFunDec;
  mirFun: TMirFunDec;
  i: Integer;
  mirEle : TMirElement;
  vardec: TMirVarDec;
begin
//  //Explora primero a las funciones terminales
//  for fun in usedFuncs do begin
//    if not fun.IsTerminal2 then continue;
//    //DebugLn('función terminal: %s con %d var.loc.', [fun.name, fun.nLocalVars]);
//    //Los parámetros y variables locales aparecen como elementos de la función
//    for elem in fun.elements do if elem.idClass = eleVarDec then begin  //Para todas sus variables.
//      xvar := TEleVarDec(elem);
//      if xvar.IsParameter or  //If function is used, we assume the all the parameters too.
//         (xvar.nCalled>0) then begin
//        //Asign RAM space to this variable in shared mode.
//        CreateVarInRAM(xvar, true);
//        if HayError then exit;
//      end;
//    end;
//    if OptReuProVar then SetSharedUnused;   //limpia las posiciones usadas
//  end;
//  if OptReuProVar then SetSharedUsed;  //Ahora marca como usados, porque ya se creó la zona de bytes compartidos
//  //Explora solo a las funciones que no son terminales
//  for fun in usedFuncs do begin
//    if fun.IsTerminal2 then continue;
//    //Los parámetros y variables locales aparecen como elementos de la función
//    for elem in fun.elements do if elem.idClass = eleVarDec then begin  //Para todas sus variables.
//      xvar := TEleVarDec(elem);
//      if xvar.IsParameter or  //If function is used, we assume the all the parameters too.
//         xvar.required or      //Variable is used as a register and it's used.
//         (xvar.nCalled>0) then begin
//        //Asign RAM space to this variable in Normal mode.
//        CreateVarInRAM(xvar, false);
//        if HayError then exit;
//      end;
//    end;
//  end;
//  //Reserva espacio para las variables (Que no son de funciones).
//  for xvar in TreeElems.AllVars do begin
//    if xvar.Parent.idClass = eleFuncImp then continue;  //Las variables de funciones ya se crearon
//    //if xvar.Parent.idClass = eleUnit then continue;
////debugln('Verificando: ' + xvar.name);
//    if (xvar.nCalled>0) or xvar.required then begin
//      //Asigna una dirección válida para esta variable
////debugln('  ubicando: ' + xvar.name);
//      CreateVarInRAM(xvar, false);
//      if HayError then exit;
//    end else begin
//      //Variable no usada
//      xvar.ResetAddress;
//      if xvar.Parent = TreeElems.main then begin
//        //Genera mensaje solo para variables del programa principal.
//        GenWarn(WA_UNUSED_VAR_, [xVar.name], xvar.srcDec);
//      end;
//    end;
//  end;
{*** Por completar
  /////////////////////////////////////////////////////////
  //Explora primero a las funciones terminales
  for i:=1 to mirRep.root.items.Count-2 do begin
    mirFun := TMirFunDec(mirRep.root.items[i]);
    if not mirFun.IsTerminal2 then continue;
    //DebugLn('función terminal: %s con %d var.loc.', [mirFun.name, mirFun.nLocalVars]);
    //Los parámetros y variables locales aparecen como elementos de la función
    for mirEle in mirFun.declars do if mirEle.mirType = mtyVarDec then begin  //Para todas sus variables.
      vardec := TMirVarDec(mirEle);
      if vardec.IsParameter then begin  //If function is used, we assume the all the parameters too.
        //Asign RAM space to this variable in shared mode.
        CreateVarInRAM(vardec, true);
        if HayError then exit;
      end;
    end;
    if OptReuProVar then SetSharedUnused;   //limpia las posiciones usadas
  end;
  if OptReuProVar then SetSharedUsed;  //Ahora marca como usados, porque ya se creó la zona de bytes compartidos
  //Explora solo a las funciones que no son terminales
  for i:=1 to mirRep.root.items.Count-2 do begin
    mirFun := TMirFunDec(mirRep.root.items[i]);
    if mirFun.IsTerminal2 then continue;
    //Los parámetros y variables locales aparecen como elementos de la función
    for mirEle in mirFun.declars do if mirEle.mirType = mtyVarDec then begin  //Para todas sus variables.
      vardec := TMirVarDec(mirEle);
      if vardec.IsParameter {or  //If function is used, we assume the all the parameters too.
         vardec.required} then begin  //Variable is used as a register and it's used.
        //Asign RAM space to this variable in Normal mode.
        CreateVarInRAM(vardec, false);
        if HayError then exit;
      end;
    end;
  end;
  //Reserva espacio para las variables (Que no son de funciones).
  for mirEle in mirRep.root.declars do if mirEle.mirType = mtyVarDec then begin
    vardec := TMirVarDec(mirEle);
//    if vardec.required then begin
      //Asigna una dirección válida para esta variable
//debugln('  ubicando: ' + vardec.name);
      CreateVarInRAM(vardec, false);
      if HayError then exit;
//    end else begin
//      //Variable no usada
//      vardec.ResetAddress;
//      if vardec.Parent = TreeElems.main then begin
//        //Genera mensaje solo para variables del programa principal.
//        GenWarn(WA_UNUSED_VAR_, [vardec.name], vardec.srcDec);
//      end;
//    end;
  end;
}
end;


procedure TCompiler_PIC16.GenerateMIR;
var
  astFunDec : TEleFunDec;
  astVardec: TEleVarDec;
  bod : TEleBody;
  elem : TAstElement;
  mirFunDec: TMirFunDec;
  mirVarDec: TMirVarDec;
  astConDec: TEleConsDec;
  mirConDec: TMirConDec;
begin
  //Agrega variables globales
//  for astVardec in TreeElems.AllVars do begin
//    if astVardec.Parent.idClass = eleFuncImp then continue;  //Las variables de funciones ya se crearon
////debugln('Verificando: ' + astVardec.name);
//    if (astVardec.nCalled>0) or astVardec.required then begin
//      mirVarDec := AddMirVarDec(mirRep.root, astVardec); //Agrega declaración en el MIR
//      astVardec.mirVarDec := mirVarDec;  //Guarda referencia al MIR.;
//    end;
//  end;
  for elem in TreeElems.main.elements do begin
    if (elem.idClass = eleConsDec) and (elem.nCalled>0) then begin
      astConDec := TEleConsDec(elem);
      mirConDec := AddMirConDec(mirRep.root, astConDec);
    end else if (elem.idClass = eleVarDec) and (elem.nCalled>0) then begin
      astVardec := TEleVarDec(elem);
      mirVarDec := AddMirVarDec(mirRep.root, astVardec); //Agrega declaración en el MIR
      astVardec.mirVarDec := mirVarDec;  //Guarda referencia al MIR.;
    end;
  end;
  for astFunDec in usedFuncs do begin
    if astFunDec.callType = ctUsrNormal then begin
      //Agrega al MIR y guarda referencia.
      mirFunDec := AddMirFunDecUNF(mirRep.root, astFunDec);
      astFunDec.mirFunDec := mirFunDec;  //Guarda referencia al MIR.
      //Explora sus elementos internos.
      for elem In astFunDec.elemImplem do begin
          if elem.idClass = eleVarDec then begin
            astVarDec := TEleVarDec(elem);  //Guarda referencia
            //Agrega al MIR y guarda referencia.
            mirVarDec := AddMirVarDec(mirFunDec, astVarDec);
            astVarDec.mirVarDec := mirVarDec;  //Guarda referencia al MIR.
          end else if elem.idClass = eleBody then begin
            mirRep.ConvertBody(mirFunDec, TEleBody(elem));
            //if HayError then exit;   //Puede haber error
          end;
      end;
      //Actualizmos los parámetros MIR después de explorar los elementos
      //internas AST (que incluye a los parámetros).
      mirFunDec.ReadParamsFromAST(astFunDec);
    end else if astFunDec.callType = ctSysNormal then begin
      mirFunDec := AddMirFunDecSNF(mirRep.root, astFunDec);
      //System function doesn't have body.
    end;
  end;
  //Split body
  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
  mirRep.ConvertBody(mirRep.root, bod);
end;
procedure TCompiler_PIC16.DoOptimize;
{Usa la información del árbol de sintaxis, para optimizar su estructura con
miras a la síntesis.
Se debe llamar después de llamar a DoAnalyzeProgram().}
begin
  if IsUnit then exit;
  ExprLevel := 0;

  ClearError;
  //Detecting unused elements
  RefreshAllElementLists; //Actualiza lista de elementos
  RemoveUnusedFunc;       //Se debe empezar con las funciones. 1ms aprox.
  RemoveUnusedVars;       //Luego las variables. 1ms aprox.
  RemoveUnusedCons;       //1ms aprox.
  RemoveUnusedTypes;      //1ms aprox.
  //Updating callers and calleds.
  UpdateFunLstCalled;     //Actualiza lista "lstCalled" de las funciones usadas.
  if HayError then exit;
  SeparateUsedFunctions;  //Updates "usedFuncs".
//  GenerateMIR;            //Genera la representación MIR
  //Evaluate declared constants
//  EvaluateConstantDeclare;
//  if HayError then exit;
//  //Simplify expressions
////  PrepareSentences;
//  {Do a first folding in nodes. Some constants (like those that depend on addresses)
//  might not be evaluated. So it should be needed to do other Code folding again.}
//  ConstantFolding;
//  if HayError then exit;
//  ConstanPropagation;
end;
procedure TCompiler_PIC16.DoCompile;
{Compila el contenido del archivo "mainFile" con las opciones que se hayan definido en
esta instancia del compilador.
No debería usarse directamente, sino a través del método Exec(), para asegurarse de que
se inicializan correctamente las configuraiones.}
var
  p: SizeInt;
begin
  if comp_level = clNull then exit;
  debugln('');
  StartCountElapsed;  //Start timer
  DefCompiler;   //Debe hacerse solo una vez al inicio
  hexfile  := ChangeFileExt(mainFile, '.prg');     //Obtiene nombre
  hexfile  := hexFilePath;   //Expande nombre si es necesario
  //se pone en un "try" para capturar errores y para tener un punto salida de salida
  //único
  if ejecProg then begin
    GenError(ER_COMPIL_PROC);
    exit;  //sale directamente
  end;
  try
    ejecProg := true;  //marca bandera
    ClearError;
    //Genera instrucciones de inicio
    ClearContexts;       //elimina todos los Contextos de entrada
    //Compila el texto indicado
    if not OpenContextFrom(mainFile) then begin
      //No lo encuentra
      GenError(ER_FIL_NOFOUND, [mainFile]);
      exit;
    end;
    {-------------------------------------------------}
    TreeElems.Clear;
    mirRep.Clear;
    //Asigna nombre y archivo a elemento
    TreeElems.main.name := ExtractFileName(mainFile);
    p := pos('.',TreeElems.main.name);
    if p <> 0 then TreeElems.main.name := copy(TreeElems.main.name, 1, p-1);
    TreeElems.main.srcDec := GetSrcPos;
    //Continúa con preparación
//    EndCountElapsed('** Setup in: ');
//    StartCountElapsed;  //Start timer
    CreateSystemUnitInAST;  //Crea los elementos del sistema. 3ms aprox.
    ClearMacros;           //Limpia las macros
    //Initiate CPU
    ExprLevel := 0;
    pic.dataAddr1 := -1;  //Reset flag
    pic.MsjError := '';
    //Compila el archivo actual como programa o como unidad
    pic.InitMemRAM;  //Init RAM and clear.
    pic.iRam := 0;  //Ubica puntero al inicio.
    IsUnit := GetUnitDeclaration();
    DoAnalyze;
    if HayError then exit;
    UpdateCallersToUnits;
    //Actualiza esta opción al generador de código porque puede haberse cambiado con las directivas.
    SIF_P65pas.str_nullterm := str_nullterm;

    EndCountElapsed('-- Analyzed in: ');
    if comp_level >= clAnalOptim then begin  //Hay optimización
      if not IsUnit then begin
        {Compila solo los procedimientos usados, leyendo la información del árbol de sintaxis,
        que debe haber sido actualizado en la primera pasada.}
        StartCountElapsed;
        DoOptimize;
        if HayError then exit;
        EndCountElapsed('-- Optimized in: ');
      end;
    end;
    if comp_level >= clComplete then begin  //Hay síntesis
      if not IsUnit then begin
        StartCountElapsed;
        DoGenerateCode;
        //EndCountElapsed('-- Synthetized in: ');
        //StartCountElapsed;
        //Genera archivo hexa, en la misma ruta del programa
        DoGenerateHexFile(hexfile);
        EndCountElapsed('-- Output generated in: ');
      end;
    end;
    {-------------------------------------------------}
    //ClearAll;//es necesario por dejar limpio
  finally
    StartCountElapsed;
    ejecProg := false;
    //Tareas de finalización
    if OnAfterCompile<>nil then OnAfterCompile;
    EndCountElapsed('-- OnAfterCompile in: ');
  end;
end;
function AdrStr(absAdr: word): string;
{formatea una dirección en cadena.}
begin
  Result := '$' + IntToHex(AbsAdr, 4);
end;
procedure TCompiler_PIC16.DumpCode(lins: TSTrings);
{Genera el código ensamblador en el StringList "lins", con las configuraciones
actuales del compilador.
Se debe llamar despues de llamar a pic.GenHex(), para que se actualicen las variables
minUsed y maxUsed.}
const
  SPACEPAD = '          ';
  ASMPAD = '  ';
  LSPC = length(SPACEPAD);

  procedure VariablesLocation(lins: TStrings; ExcUnused: boolean);
  {Return a string with information about all variables location.}
  var
    v: TEleVarDec;
    subUsed: string;
  begin
    for v in TreeElems.AllVars do begin   //Se supone que "AllVars" ya se actualizó.
        //debugln('AllVars['+IntToStr(i)+']='+v.name+','+v.Parent.name);
        if ExcUnused and (v.nCalled = 0) then continue;
        if v.storage in [stRegister, stRegistX, stRegistY] then continue;
        if v.nCalled = 0 then subUsed := '; <Unused>' else subUsed := '';
        if v.typ.IsByteSize then begin
          lins.Add(PadRight(v.name, LSPC) + ' EQU ' + AdrStr(v.addr)+ subUsed +
                   '       ;' + v.typ.name);
        end else if v.typ.IsWordSize then begin
          lins.Add(PadRight(v.name, LSPC) + ' EQU ' +  AdrStr(v.addrL)+ subUsed +
                   '       ;' + v.typ.name);
          lins.Add(SPACEPAD + ' EQU ' +  AdrStr(v.addrH)+ subUsed);
        end else if v.typ.catType = tctArray then begin   //It's an array
          lins.Add(PadRight(v.name, LSPC) + ' EQU ' +  AdrStr(v.addrL) + '~' +
                   AdrStr(v.addr + v.typ.size-1) + subUsed + ' ;' + v.typ.name);
        end else begin
          lins.Add(PadRight(v.name, LSPC) + ' EQU ' +  AdrStr(v.addr) + subUsed +
                   '       ;' + v.typ.name);
        end;
    end;
  end;

var
  i: word;
  minUsed: integer;
  lblLin, comLin, lin: String;
  nBytes: byte;

  procedure DumpVar(const InitStr: string);
    var n: integer;
        s: string;

  begin
    s := InitStr;
    n := 7;
    inc(i);
    while pic.ram[i].name = ''  do begin
      s := s + ' ' + IntToHEx(pic.ram[i].value,2);
      dec(n);
      if n = 0 then begin
        lins.Add(s);
        n := 8;
        //s := SPACEPAD;
        s := PadRight('', LSPC) + '$'+IntToHex(i,4) + ' DB';
      end;
      inc(i);
    end;
    if n < 8 then lins.Add(s);
  end;

begin
  if asmOutType=0 then begin  //Normal Assembler output
    //Include header
    lins.Add('      ;Code generated by P65Pas compiler');
    lins.Add('      processor ' + PICName);
    //Variables location section
    if IncVarDec then begin
       lins.Add('__all_variables:');
       VariablesLocation(lins, ExcUnused);
    end;
    //Se supone que minUsed y maxUsed, ya deben haber sido actualizados.
    if IncAddress then begin  //ORG title
      lins.Add(SPACEPAD + '      ORG $' + IntToHex(pic.minUsed, 4));
    end else begin
      lins.Add(SPACEPAD + 'ORG $' + IntToHex(pic.minUsed, 4));
    end;
    //Write the RAM content.
    i := pic.minUsed;
    while i <= pic.maxUsed do begin
      if (i=addBootldr) and (addBootldr<>addVariab) then lins.Add('__bootloader:');
      if (i=addVariab) and (addVariab<>addFuncts) then lins.Add('__var_section:');
      //Read label and comments.
      lblLin := pic.ram[i].name;
      comLin := pic.ram[i].topComment;
      //Check RAM position.
      if pic.ram[i].used in [ruData, ruAbsData] then begin
        //Must be a variable.
        if IncAddress then begin
          if comLin<>'' then lins.add(comLin);
          DumpVar( PadRight(lblLin, LSPC) + '$'+IntToHex(i,4) + ' DB ' +
                    IntToHEx(pic.ram[i].value,2) );
        end else begin
          lins.Add( PadRight(lblLin, LSPC) + 'DB ' + IntToHEx(pic.ram[i].value,2) );
        end;
        //i := i + 1;
      end else begin
        //Debe ser código o memoria sin usar.
        if lblLin<>'' then lins.Add(lblLin+':');  //Etiqueta al inicio de línea
        //Escribe comentario al inicio de línea
        if asmIncComm and (comLin<>'') then  begin
          lins.Add(comLin);
        end;
        lin := pic.GetASMlineAt(i, IncAddress, IncAddress, asmIncComm, incVarName, nBytes);
        lins.Add(ASMPAD + lin);
        i := i + nBytes;   //Incrementa a siguiente instrucción
      end;
    end;
    lins.Add(';--------------------');
    lins.Add('      END');
  end else begin  //Generate POKE's BASIC code.
    minUsed := pic.CPUMAXRAM;
    i := pic.minUsed;
    while i <= pic.maxUsed do begin
      if pic.ram[i].used <> ruUnused then begin
        if i<minUsed then minUsed := i;  //Calcula mínimo
        lins.Add('poke ' +  IntToStr(i) + ',' + IntToStr(pic.ram[i].value));
      end;
      inc(i);
    end;
    lins.Add('sys ' +  IntToStr(minUsed) );
  end;
end;
function TCompiler_PIC16.RAMusedStr: string;
var
  usedRAM, totRAM: integer;
begin
  totRAM := pic.TotalMemRAM;
  if totRAM=0 then exit;  //protección
  usedRAM := pic.UsedMemRAM;
  Result := MSG_RAM_USED + IntToStr(usedRAM) +'/'+ IntToStr(totRAM) + 'B (' +
        FloatToStrF(100*usedRAM/totRAM, ffGeneral, 1, 3) + '%)';
end;
procedure TCompiler_PIC16.GetResourcesUsed(out ramUse, romUse, stkUse: single);
var
  usedRAM, totRAM: integer;
begin
  //Calcula RAM
  ramUse := 0;  //valor por defecto
  totRAM := pic.TotalMemRAM;
  if totRAM = 0 then exit;  //protección
  usedRAM := pic.UsedMemRAM;
  ramUse := usedRAM/totRAM;
  //Calcula STACK
//  nes := TreeElems.main.UpdateCalledAll;   //Debe haberse llenado TreeElems.main.lstCalled
  //No considera el anidamiento por interrupciones
  stkUse := TreeElems.main.maxNesting/STACK_SIZE;
end;
procedure TCompiler_PIC16.GenerateListReport(lins: TStrings);
{Genera un reporte detallado de la compilación}
var
  curInst, opc: TP6502Inst;
  i: word;
  OpCodeCoun: array[low(TP6502Inst)..high(TP6502Inst)] of integer;
  tmpList: TStringList;
  txt, OpCode, Times, state: String;

  fun: TEleFunDec;
  caller : TAstEleCaller;
  called : TAstElement;
  //exitCall: TExitCall;
begin
  ////////////////////////////////////////////////////////////
  //////////// Reporte de uso de memeoria  ///////////
  ////////////////////////////////////////////////////////////
  lins.Add(RAMusedStr);
  ////////////////////////////////////////////////////////////
  //////////// Reporte de cuenta de instrucciones  ///////////
  ////////////////////////////////////////////////////////////
  //Limpia contadores
  for opc := low(TP6502Inst) to high(TP6502Inst) do begin
    OpCodeCoun[opc] := 0;
  end;
  //Cuenta apariciones
  for i:=0 to high(pic.ram) do begin
    if pic.ram[i].used <> ruUnused then begin
       pic.PC.W := i;
       curInst := pic.CurInstruction;
       Inc(OpCodeCoun[curInst]);  //Acumula
    end;
  end;
  //Carga en lista para ordenar
  tmpList:= TStringList.Create;
  for opc := low(TP6502Inst) to high(TP6502Inst) do begin
    tmpList.Add(Format('%.4d', [OpCodeCoun[Opc]]) + '-' + PIC16InstName[opc].name);
  end;
  tmpList.Sort;  //Ordena
  //Muestra lista ordenada
  lins.Add(';INSTRUCTION COUNTER');
  lins.Add(';===================');
  for i:=tmpList.Count-1 downto 0 do begin
    txt := tmpList[i];
    OpCode := copy(txt , 6, 10);
    Times  := copy(txt , 1, 4);
    if Times = '0000' then continue;
    lins.Add(copy(OpCode + '    ',1,7) + '->'+ Times);
  end;
  tmpList.Destroy;

  ////////////////////////////////////////////////////////////
  ////////////////// Reporte de Funciones   ///////////
  ////////////////////////////////////////////////////////////
  lins.Add('');
  lins.Add(';PROCEDURE LIST');
  lins.Add(';===================');

  lins.Add(';NAME                    USED   POSITION IN SOURCE');
  lins.Add(';----------------------- ------ -------------------------------');
  for fun in TreeElems.AllFuncs do begin
    if fun.nCalled > 0 then begin
      if fun.nCalled = 0 then
        state := 'Unused'
      else
        state := RightStr('     '+IntToStr(fun.nCalled)+ '', 6);

      lins.Add( copy(fun.name + space(24) , 1, 24) + ' ' +
                state + ' ' +
                fun.srcDec.RowColString + ':' + ctxFile(fun.srcDec.idCtx)
      );
    end;
  end;

  ////////////////////////////////////////////////////////////
  ////////////////// Detalle de Funciones   ///////////
  ////////////////////////////////////////////////////////////
  lins.Add('');
  lins.Add(';PROCEDURE DETAIL');
  lins.Add(';===================');
  for fun in TreeElems.AllFuncs do begin
    if fun.nCalled > 0 then begin
      lins.Add('------------------------------------');
      lins.Add('----- PROCEDURE ' + fun.name);
      lins.Add('------------------------------------');
      lins.Add('  Caller Procedures:');
      if fun.lstCallers .Count = 0 then begin
        lins.Add('    <none>');
      end else begin
        for caller in fun.lstCallers do begin
          lins.Add('    - ' + caller.caller.Parent.name);
        end;
      end;
      lins.Add('');

      lins.Add('  Called Procedures:');
      if fun.lstCalled.Count = 0 then begin
        lins.Add('    <none>');
      end else begin
        for called in fun.lstCalled do begin
          lins.Add('    - ' + called.name);
        end;
      end;
      lins.Add('');

      lins.Add('  All Called Procedures:');
      if fun.lstCalledAll.Count = 0 then begin
        lins.Add('    <none>');
      end else begin
        for called in fun.lstCalledAll do begin
          lins.Add('    - ' + called.name);
        end;
      end;
      lins.Add('');

      lins.Add('  Exit Instruction in obligatory code:');
      if fun.firstObligExit = nil then begin
        lins.Add('    <none>');
      end else begin
        //for exitCall in fun.lstExitCalls do begin
        //  lins.Add('    - Exit() in ' +exitCall.srcPos.RowColString);
        //end;
        lins.Add('    - Oblig. exit() in ' + fun.firstObligExit.srcDec.RowColString);
      end;
      lins.Add('');

    end;
  end;
  //Detalles del programa principal

  lins.Add('------------------------------------');
  lins.Add('----- Main Program');
  lins.Add('------------------------------------');
  lins.Add('  Called Procedures:');
  if TreeElems.main.lstCalled.Count = 0 then begin
    lins.Add('    <none>');
  end else begin
    for called in TreeElems.main.lstCalled do begin
      lins.Add('    - ' + called.name);
    end;
  end;
  lins.Add('');
  //Muestra el máximo nivel de anidamiento.
  lins.Add('Max. Nesting = ' + IntToSTr(TreeElems.main.maxNesting));

end;
//Interfaz for IDE
procedure TCompiler_PIC16.Exec(srcFile, outFile: string; pars: string);
{Execute the compiler. Commonly it will compile "srcFile" getting the parameters fron the
string "pars". Pars must contain a parameter each line.
This must be the main entry point to the compiler.}
var
  parsList: TStringList;
  txt, tmp: string;
begin
  //Default settings for Command line Options
  mainFile := srcFile;
  comp_level  := clComplete;
  ForToRepeat := true;
  enabDirMsgs := true;
  OptReuProVar:= false;   //Optimiza reutilizando variables locales de procedimientos.
  OptRetProc  := false;   //Optimiza el último exit de los procedimientos.
  RemUnOpcod  := false;

  asmOutType  := 0;  //Normal Assembler
  asmIncComm  := false;
  IncVarDec   := false;
  IncVarName  := false;
  IncAddress  := false;
  //Default settings for Directive settings.
  syntaxMode  := modPicPas;   //Por defecto en sintaxis nueva
  cpuMode     := cpu6502;
  bootloader  := bldJMP;
  str_nullterm:= false;
  //Load parameters in a list
  parsList := TStringList.Create;
  parsList.Text := trim(pars);
//debugln('--Executing:('+ StringReplace(pars, LineEnding,' ',[rfReplaceAll])+')');
  //Extract and set parameters
  unitPaths.Clear;
  for txt in parsList do begin
    if length(txt)<2 then continue;
    if          copy(txt,1,2) = '-C' then begin  //---Compiling options
      case txt of
      //Compiler level
      '-Cn' : comp_level := clNull;
      '-Ca' : comp_level := clAnalys;
      '-Cao': comp_level := clAnalOptim;
      '-C'  : comp_level := clComplete;
      //Compiler settings
      '-Cf' : ForToRepeat := false;
      end;
    end else if copy(txt,1,2) = '-O' then begin  //---Optimization options
      case txt of
      '-Ov' : OptReuProVar := true;
      '-Or' : OptRetProc   := true;
      '-Ou' : RemUnOpcod   := true;
      end;
    end else if copy(txt,1,2) = '-A' then begin  //---Assembler options
      case txt of
      '-A0': asmOutType := 0;    //Output in normal Assembler.
      '-A1': asmOutType := 1;    //Output in BASIC POKE's loader.
      '-Ac': asmIncComm := true; //Include commnents in ASM output.
      '-Av': IncVarDec  := true; //Include variables information section.
      '-Au': ExcUnused  := true; //Exclude unused variables in variable section.
      '-An': incVarName := true; //Include nombres de variables en las instrucciones.
      '-Aa': IncAddress := true; //Include memory address in instructions.
      end;
    end else if copy(txt,1,2) = '-F' then begin  //File names and paths
      if copy(txt,1,3) = '-Fu' then begin  //Add unit path
        tmp := copy(txt,4,length(txt));
        if tmp='' then continue;
        if tmp[1]='"' then delete(tmp,1,1);
        if tmp[length(tmp)]='"' then delete(tmp,length(tmp),1);
        unitPaths.Add(tmp);
      end else if copy(txt,1,3) = '-Fo' then begin  //Set output file
        tmp := copy(txt,4,length(txt));
        if tmp='' then continue;
        if tmp[1]='"' then delete(tmp,1,1);
        if tmp[length(tmp)]='"' then delete(tmp,length(tmp),1);
        setHexFile(tmp);
      end;
    end else if txt = '-Dn' then begin  //Disable directive messages
      enabDirMsgs := false;
    end else begin         //Other.

    end;
  end;
  //Compile
  DoCompile;
  //Destroy list
  parsList.Destroy;
end;
//Inicialización
procedure SetCodSysInline(fun: TEleFunDec; codSysInline: TCodSysInline);
{Procedimiento que, por seguridad, debería ser el único acceso a TEleFunDec.codSysInline.
Así se garantiza que el "casting" se haga apropiadamente.}
begin
  fun.callType := ctSysInline;
  fun.codSysInline := TMethod(codSysInline);
end;
//////////////// Tipo Boolean /////////////
function TCompiler_PIC16.PICName: string;
begin
  Result := pic.Model;
end;
//////////////// Tipo Byte /////////////
procedure TCompiler_PIC16.byte_LoadToWR(fun: TMirOperand);
{Load operand to WR. It's, convert storage to stRegister }
begin
  case fun.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    _LDAi(fun.value.valInt);
  end;
  stRamFix: begin
    _LDA(fun.vardec.addr);
  end;
  stRamVarOf: begin
    if fun.vardec.typ.IsByteSize then begin
      //Indexado por Byte
      _LDX(fun.vardec.addr);  //Load address
      _LDAx(fun.offs);
    end else if fun.vardec.typ.IsWordSize then begin
      if fun.offs<256 then begin
        AddCallerToFromCurr(IX);  //We declare using IX
        //if not IX.allocated then begin
        //  GenError(ER_NOT_IMPLEM_, [fun.StoAsStr]);
        //  exit;
        //end;
        //Escribe dirección en puntero
        _LDA(fun.vardec.addr);
        _STA(IX.addr);
        _LDA(fun.vardec.addr+1);
        _STA(IX.addr+1);
        //Carga desplazamiento
        _LDYi(fun.offs);  //Load address
        //Carga indexado
        pic.codAsm(i_LDA, aIndirecY, IX.addr);
      end else begin
        GenError(ER_NOT_IMPLEM_, [fun.StoAsStr]);
      end;
    end else begin
      GenError(ER_NOT_IMPLEM_, [fun.StoAsStr]);
    end;
  end;
  stRegister, stRegistA: begin
    //Already in WR
  end;
  else
    //Almacenamiento no implementado
    GenError(ER_NOT_IMPLEM_, [fun.StoAsStr]);
  end;
end;
procedure TCompiler_PIC16.byte_DefineRegisters;
begin
  //No es encesario, definir registros adicionales a A
end;
procedure TCompiler_PIC16.byte_SaveToStk;
begin
  _PHA;
end;
//////////////// Tipo Word /////////////
procedure TCompiler_PIC16.word_RequireWR;
{Generate de callings to Work Register used when loading a Word in Work registers.}
begin
  AddCallerToFromCurr(H);
end;
procedure TCompiler_PIC16.word_LoadToWR(fun: TMirOperand);
{Carga el valor de una expresión a los registros de trabajo.}
var
  idx: TEleVarDec;
  addrNextOp1, addrNextOp2: Integer;
begin
  case fun.Sto of  //el parámetro debe estar en "Op^"
  stConst : begin
    //byte alto
    _LDAi(fun.value.HByte);
    _STA(H.addr);
    //byte bajo
    _LDAi(fun.value.LByte);
  end;
  stRamFix: begin
    _LDA(fun.vardec.addr+1);
    _STA(H.addr);
    _LDA(fun.vardec.addr);
  end;
  stRegister: begin  //Already in (H,A)
  end;
//  stVarRef, stExpRef: begin
//    if Op^.Sto = stExpRef then begin
//      idx := IX;  //Index variable
//    end else begin
//      idx := Op^.vardec;  //Index variable
//    end;
//    if idx.typ.IsByteSize then begin
//      //Indexed in zero page is simple
//      _LDX(idx.addr);
//      _INX;  //Fail in cross-page
//      pic.codAsm(i_LDA, aZeroPagX, 0);  //MSB
//      _STA(H.addr);
//      _DEX;
//      pic.codAsm(i_LDA, aZeroPagX, 0);  //LSB
//    end else if idx.typ.IsWordSize then begin
//      if idx.addr<256 then begin
//        //Index in zero page. It's simple
//        _LDYi(1);
//        pic.codAsm(i_LDA, aIndirecY, idx.addr);  //MSB
//        _STA(H.addr);
//        _DEY;
//        pic.codAsm(i_LDA, aIndirecY, idx.addr);  //LSB
//      end else begin
//        //Index is word and not in zero page
//        //WARNING this is "Self-modifiying" code.
//        //---------- MSB ------------
//        _CLC;   //Prepare adding 1
//        _LDA(idx.addr);  //Load LSB index
//        _ADCi(1);
//addrNextOp1 := pic.iRam + 1;  //Address next instruction
//        pic.codAsm(i_STA, aAbsolute, 0); //Store forward
//        _LDA(idx.addr+1);  //Load virtual MSB index
//        _ADCi(0);   //Just to add the carry
//addrNextOp2 := pic.iRam + 1;  //Address next instruction
//        PIC.codAsm(i_STA, aAbsolute, 0);  //Store forward
//        //Modified LDA instruction
//        pic.codAsm(i_LDA, aAbsolute, 0); //Store forward
//        //Complete address
//        pic.ram[addrNextOp1].value := (pic.iRam - 2) and $FF;
//        pic.ram[addrNextOp1+1].value := (pic.iRam - 2)>>8;
//        pic.ram[addrNextOp2].value := (pic.iRam - 1) and $FF;
//        pic.ram[addrNextOp2+1].value := (pic.iRam - 1)>>8;
//        _STA(H.addr);  //Store MSB in H
//        //---------- LSB ------------
//        _LDA(idx.addr);  //Load LSB index
//addrNextOp1 := pic.iRam + 1;  //Address next instruction
//        pic.codAsm(i_STA, aAbsolute, 0); //Store forward
//        _LDA(idx.addr+1);  //Load virtual MSB index
//addrNextOp2 := pic.iRam + 1;  //Address next instruction
//        PIC.codAsm(i_STA, aAbsolute, 0);  //Store forward
//        //Modified LDA instruction
//        pic.codAsm(i_LDA, aAbsolute, 0); //LSB
//        //Complete address
//        pic.ram[addrNextOp1].value := (pic.iRam - 2) and $FF;
//        pic.ram[addrNextOp1+1].value := (pic.iRam - 2)>>8;
//        pic.ram[addrNextOp2].value := (pic.iRam - 1) and $FF;
//        pic.ram[addrNextOp2+1].value := (pic.iRam - 1)>>8;
//      end;
//    end else begin
//      //refVar can only be byte or word size.
//      GenError('Not supported this index.');
//    end;
//  end;
  else
    //Almacenamiento no implementado
    GenError(MSG_NOT_IMPLEM);
  end;
end;
procedure TCompiler_PIC16.word_DefineRegisters;
begin
  //Changed from versión 0.7.1
  AddCallerToFromCurr(H);
end;
procedure TCompiler_PIC16.word_SaveToStk;
begin
  //guarda A
  _PHA;
  //guarda H
  _LDA(H.addr);
  _PHA;
end;
procedure TCompiler_PIC16.word_Low(var fun: TMirOperand);
{Acceso al byte de menor peso de un word.}
var
  par: TMirOperand;
begin
  par := fun.elements[0];  //Only one parameter
  requireA;
  case par.Sto of
  stRamFix: begin
    if par.allocated then begin
      SetFunVariab(fun, par.addL);
    end else begin
      //We cannot set a variable yet
      SetFunExpres(fun);
    end;
  end;
  stConst: begin
    if par.evaluated then begin
      //We can take the low part
      SetFunConst_byte(fun, par.value.ValInt and $ff);
    end else begin
      //We cannot set a variable yet
      SetFunExpres(fun);
    end;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TCompiler_PIC16.word_High(var fun: TMirOperand);
{Acceso al byte de mayor peso de un word.}
var
  par: TMirOperand;
begin
  par := fun.elements[0];  //Only one parameter
  requireA;
  case par.Sto of
  stRamFix: begin
    if par.allocated then begin
      SetFunVariab(fun, par.addH);
    end else begin
      //We cannot set a variable yet
      SetFunExpres(fun);
    end;
  end;
  stConst: begin
    if par.evaluated then begin
      //We can take the high part
      SetFunConst_byte(fun, par.value.ValInt and $ff00 >>8);
    end else begin
      //We cannot set a variable yet
      SetFunExpres(fun);
    end;
  end;
  else
    GenError('Syntax error.');
  end;
end;

function TCompiler_PIC16.AddSIFtoUnit(name: string; retType: TEleTypeDec; const srcPos: TSrcPos;
               const pars: TAstParamArray; codSys: TCodSysInline): TEleFunDec;
{Create a new system function in the current element of the Syntax Tree.
 Returns the reference to the function created.
   pars   -> Array of parameters for the function to be created.
   codSys -> SIF Routine or the the routine to generate de code.
}
var
   funimp: TEleFunImp;
   tmpLoc: TElemLocation;
begin
  tmpLoc := curLocation;     //Save current location. We are going to change it.
  //Add declaration
  curLocation := locInterface;
  Result := AddFunctionDEC(name, retType, srcPos, pars, false);
  Result.callType := ctSysInline; //INLINE function
  //Implementation
  {Note that implementation is added always after declarartion. It's not the usual
  in common units, where all declarations are first}
  curLocation := locImplement;
  funimp := AddFunctionIMP(name, retType, srcPos, Result, true);
  //Here variables can be added
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  TreeElems.AddBodyAndOpen(SrcPos);  //Create body
  SetCodSysInline(Result, codSys);  //Set routine to generate code o SIF routine.
  TreeElems.CloseElement;  //Close body
  TreeElems.CloseElement;  //Close function implementation
  curLocation := tmpLoc;   //Restore current location
end;
function TCompiler_PIC16.AddSNFtoUnit(name: string; retType: TEleTypeDec; const srcPos: TSrcPos;
               var pars: TAstParamArray; codSys: TCodSysNormal): TEleFunDec;
{Create a new system function in the current element of the Syntax Tree.
 Returns the reference to the function created.
   pars   -> Array of parameters for the function to be created.
   codSys -> SIF Routine or the the routine to generate de code.
}
var
  local_vars: TAstParamArray;   //Array for local variables

  procedure extract_local_vars;
  //Extract the local variables from "pars" to "local_vars"
  var
    i, n: Integer;
  begin
    n := 0;
    setlength(local_vars, n);
    i := 0;
    while i<= high(pars) do begin
      if pars[i].isLocVar then begin
        inc(n);
        setlength(local_vars, n);
        local_vars[n-1] := pars[i];
        delete(pars, i, 1);  //Delete element
      end else begin
        inc(i);
      end;
    end;
  end;
var
   fundec: TEleFunDec;
   funimp: TEleFunImp;
   tmpLoc: TElemLocation;
   locvar: TEleVarDec;
   i: Integer;
begin
  extract_local_vars();
  tmpLoc := curLocation;     //Save current location. We are going to change it.
  //Add declaration
  curLocation := locInterface;
  fundec := AddFunctionDEC(name, retType, srcPos, pars, false);
  fundec.callType := ctSysNormal;
  //Implementation
  {Note that implementation is added always after declarartion. It's not the usual
  in common units, where all declarations are first}
  curLocation := locImplement;
  funimp := AddFunctionIMP(name, retType, srcPos, fundec, true);
  //Create local variables
  for i:=0 to high(local_vars) do begin
    locvar := TreeElems.AddVarDecAndOpen(srcPos, local_vars[i].name, local_vars[i].typ);
    locvar.storage := stRamFix;
    locvar.adicPar := local_vars[i].adicVar;
    TreeElems.CloseElement;  //Close variable
    local_vars[i].vardec := locvar;  //Keep reference
  end;
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  TreeElems.AddBodyAndOpen(SrcPos);  //Create body
  //Set function created
  fundec.callType     := ctSysNormal;
  fundec.codSysNormal := codSys;  //Set routine to generate code SIF.
  TreeElems.CloseElement;  //Close body
  TreeElems.CloseElement;  //Close function implementation
  curLocation := tmpLoc;   //Restore current location
  //Add callers to local variables created. Must be done after creating the body.
  for i:=0 to high(local_vars) do begin
    locvar := local_vars[i].vardec;
    AddCallerToFrom(locvar, funimp.BodyNode);
  end;
  exit(fundec);
end;
procedure TCompiler_PIC16.AddParam(var pars: TAstParamArray; parName: string; const srcPos: TSrcPos;
                   typ0: TEleTypeDec; adicDec: TAdicDeclar);
//Create a new parameter to the function.
var
  n: Integer;
begin
  //Add record to the array
  n := high(pars)+1;
  setlength(pars, n+1);
  pars[n].name := parName;  //Name is not important
  pars[n].srcPos := srcPos;
  pars[n].typ  := typ0;  //Agrega referencia
  pars[n].adicVar.hasAdic := adicDec;
  pars[n].adicVar.hasInit := false;
  pars[n].isLocVar := false;
end;
function TCompiler_PIC16.CreateInUOMethod(
                      clsType: TEleTypeDec;   //Base type where the method bellow.
                      opr     : string;      //Opertaor associated to the method
                      name    : string;      //Name of the method
                      retType : TEleTypeDec;  //Type returned by the method.
                      pCompile: TCodSysInline;
                      operTyp: TOperatorType = opkUnaryPre): TEleFunDec;
{Create a new system function (associated to a unary operator) in the current element of
 the AST.
 Returns the reference to the function created.}
var
  pars: TAstParamArray;     //Array of parameters
begin
  setlength(pars, 0);        //Reset parameters
  AddParam(pars, 'b', srcPosNull, clsType, decNone);  //Base object
  //Add declaration
  Result      := AddFunctionUNI(name, retType, srcPosNull, pars, false, true);
  //Here variables can be added
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  SetCodSysInline(Result, pCompile); //Set routine to generate code
  Result.oper := UpCase(opr); //Set operator as UpperCase to speed searching.
  if opr = '' then Result.operTyp := opkNone
  else Result.operTyp := operTyp; //Must be pre or post
  TreeElems.CloseElement;    //Close function implementation
end;
function TCompiler_PIC16.CreateInBOMethod(
                      clsType: TEleTypeDec;   //Base type where the method bellow.
                      opr     : string;      //Opertaor associated to the method
                      name    : string;      //Name of the method
                      parType : TEleTypeDec;  //Parameter type
                      retType : TEleTypeDec;  //Type returned by the method.
                      pCompile: TCodSysInline): TEleFunDec;
{Create a new system function (associated to a binary operator) in the current element of
 the AST. If "opr" is null, just create a method without operator.
 Returns the reference to the function created.}
var
  pars: TAstParamArray;     //Array of parameters
begin
  setlength(pars, 0);        //Reset parameters
  AddParam(pars, 'b', srcPosNull, clsType, decNone);  //Base object
  AddParam(pars, 'n', srcPosNull, parType, decNone);  //Parameter
  //Add declaration
  Result      := AddFunctionUNI(name, retType, srcPosNull, pars, false,
                      false);  //Don't include variables to don't ask for RAM.
  TreeElems.AddBodyAndOpen(srcPosNull);  //Create body
  //Here variables can be added
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  SetCodSysInline(Result, pCompile); //Set routine to generate code
  Result.oper := UpCase(opr); //Set operator as UpperCase to speed search.
  if opr = '' then Result.operTyp := opkNone
  else Result.operTyp := opkBinary;
  TreeElems.CloseElement;  //Close body
  TreeElems.CloseElement;  //Close function implementation
end;
function TCompiler_PIC16.CreateInTerMethod(clsType: TEleTypeDec;
  name: string; parType1, parType2: TEleTypeDec; retType: TEleTypeDec;
  pCompile: TCodSysInline): TEleFunDec;
{Create a new system ternary INLINE function in the current element of
 the AST.
 Returns the reference to the function created.}
var
  pars: TAstParamArray;     //Array of parameters
begin
  setlength(pars, 0);        //Reset parameters
  AddParam(pars, 'b', srcPosNull, clsType, decNone);  //Base object
  AddParam(pars, 'i', srcPosNull, parType1, decNone);  //Parameter
  AddParam(pars, 'n', srcPosNull, parType2, decNone);  //Parameter
  //Add declaration
  Result      := AddFunctionUNI(name, retType, srcPosNull, pars, false,
                      false);  //Don't include variables to don't ask for RAM.
  TreeElems.AddBodyAndOpen(srcPosNull);  //Create body
  //Here variables can be added
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  SetCodSysInline(Result, pCompile); //Set routine to generate code
  Result.operTyp := opkNone;   //Could be a ternary operator
  TreeElems.CloseElement;  //Close body
  TreeElems.CloseElement;  //Close function implementation
end;
procedure TCompiler_PIC16.DefineArray(etyp: TEleTypeDec);
var
  consDec: TEleConsDec;
  expr: TEleExpress;
  f, f1, f2: TEleFunDec;
begin
  //Create assigement method
  f := CreateInBOMethod(etyp, ':=', '_set', etyp, typNull, @SIF_arr_asig_arr);
  f.asgMode := asgSimple;
  //Create attribute "low" as constant.
  AddConstDeclarByte('low', 0);
  //Create methods
//  CreateUOMethod(etyp, '', 'length', typByte, @arrayLength);
  CreateInUOMethod(etyp, '', 'high'  , typByte, @arrayHigh);
  CreateInUOMethod(etyp, '', 'clear' , typNull, @SIF_ArrayClear);
//  CreateInBOMethod(etyp, '', 'fill' , typByte, typNull, @SIF_ArrayFill);
end;
procedure TCompiler_PIC16.DefinePointer(etyp: TEleTypeDec);
{Set operations that defines pointers aritmethic.}
var
  f, f1: TEleFunDec;
begin
  //Asignación desde word y Puntero
  f := CreateInBOMethod(etyp, ':=', '_set', typWord, typNull, @SIF_word_asig_word);
  f.asgMode := asgSimple;
  f := CreateInBOMethod(etyp, ':=', '_set', etyp, typNull, @SIF_word_asig_word);
  f.asgMode := asgSimple;
  //Getter and setter
  f1 := CreateInUOMethod(etyp, '', '_getptr', etyp.ptrType, @SIF_GetPointer);
  f1.getset := gsGetInPtr;
  f := CreateInBOMethod(etyp, '', '_setptr', etyp.ptrType, typNull, @SIF_SetPointer);
  f.asgMode := asgSimple;
  f.getset := gsSetInPtr;
  f1.funset := f;

  CreateInBOMethod(etyp, '=',  '_equ',  typWord, typBool, @SIF_word_equal_word);
  CreateInBOMethod(etyp, '=',  '_equ',  etyp   , typBool, @SIF_word_equal_word);

  CreateInBOMethod(etyp, '+',  '_add',  typWord, etyp   , @SIF_pointer_add_word);
  CreateInBOMethod(etyp, '+',  '_add',  typByte, etyp   , @SIF_pointer_add_byte);

  CreateInBOMethod(etyp, '-',  '_sub',  typWord, etyp   , @SIF_pointer_sub_word);
  CreateInBOMethod(etyp, '-',  '_sub',  typByte, etyp   , @SIF_pointer_sub_byte);
  CreateInBOMethod(etyp, '>' , '_gre',  etyp   , typBool, @SIF_word_great_word);
  CreateInBOMethod(etyp, '<' , '_les',  etyp   , typBool, @SIF_word_less_word);
  CreateInBOMethod(etyp, '>=', '_gequ', etyp   , typBool, @SIF_word_gequ_word);
  CreateInBOMethod(etyp, '<=', '_lequ', etyp   , typBool, @SIF_word_lequ_word);

  f := CreateInBOMethod(etyp, '+=', '_aadd', typWord, etyp, @SIF_word_aadd_word);
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
  f := CreateInBOMethod(etyp, '+=', '_aadd', typByte, etyp, @SIF_word_aadd_byte);
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
//  etyp.CreateUnaryPostOperator('^',6, 'deref', @SIF_derefPointer);  //dereferencia
end;
procedure TCompiler_PIC16.DefineObject(etyp: TEleTypeDec);
var
  consDec: TEleConsDec;
  expr: TEleExpress;
  f, f1, f2: TEleFunDec;
begin
  //Create assigement method
  f := CreateInBOMethod(etyp, ':=', '_set', etyp, typNull, @SIF_obj_asig_obj);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
end;
procedure TCompiler_PIC16.CreateSystemTypesAndVars;
begin
  /////////////// System types ////////////////////
  typBool := CreateEleTypeDec('boolean', srcPosNull, 1, tctAtomic, t_boolean);
  SetOnLoadToWR(typBool, @byte_LoadToWR);
  typBool.location := locInterface;   //Location for type (Interface/Implementation/...)
  TreeElems.AddElementAndOpen(typBool);  //Open to create "elements" list.
  TreeElems.CloseElement;   //Close Type
  typByte := CreateEleTypeDec('byte', srcPosNull, 1, tctAtomic, t_uinteger);
  SetOnLoadToWR(typByte, @byte_LoadToWR);
  typByte.location := locInterface;
  TreeElems.AddElementAndOpen(typByte);  //Open to create "elements" list.
  TreeElems.CloseElement;

  typChar := CreateEleTypeDec('char', srcPosNull, 1, tctAtomic, t_string);
  SetOnLoadToWR(typChar, @byte_LoadToWR);
  typChar.location := locInterface;
  TreeElems.AddElementAndOpen(typChar);
  TreeElems.CloseElement;

  typWord := CreateEleTypeDec('word', srcPosNull, 2, tctAtomic, t_uinteger);
  SetOnLoadToWR(typWord, @word_LoadToWR);
  typWord.OnRequireWR := @word_RequireWR;
  typWord.location := locInterface;
  TreeElems.AddElementAndOpen(typWord);
  TreeElems.CloseElement;

  typWord := CreateEleTypeDec('word', srcPosNull, 2, tctAtomic, t_uinteger);
  SetOnLoadToWR(typWord, @word_LoadToWR);
  typWord.OnRequireWR := @word_RequireWR;
  typWord.location := locInterface;
  TreeElems.AddElementAndOpen(typWord);
  TreeElems.CloseElement;

  typDWord := CreateEleTypeDec('dword', srcPosNull, 4, tctAtomic, t_uinteger);
  //typDWord.OnLoadToWR := @word_LoadToWR;
  //typDWord.OnRequireWR := @word_RequireWR;
  typDWord.location := locInterface;
  TreeElems.AddElementAndOpen(typDWord);
  TreeElems.CloseElement;

  typTriplet := CreateEleTypeDec('triplet', srcPosNull, 3, tctAtomic, t_uinteger);
  //typTriplet.OnLoadToWR := @trip_LoadToWR;
  //typTriplet.OnRequireWR := @trip_RequireWR;
  //typTriplet.location := locInterface;
  TreeElems.AddElementAndOpen(typTriplet);
  TreeElems.CloseElement;

  {Create variables for aditional Working register. Note that this variables are
  accesible (and usable) from the code, because the name assigned is a common variable.}
  //Create register H as variable
  H := AddVarDecAndOpen('__H', typByte, srcPosNull);
  TreeElems.CloseElement;  { TODO : ¿No sería mejor evitar abrir el elemento para no tener que cerrarlo? }
  H.adicPar.hasAdic := decNone;
  H.adicPar.hasInit := false;
  H.location := locInterface;  //make visible
  //Create register E as variable
  E := AddVarDecAndOpen('__E', typByte, srcPosNull);
  TreeElems.CloseElement;
  E.adicPar.hasAdic := decNone;
  E.adicPar.hasInit := false;
  E.location := locInterface;  //make visible
  //Create register U as variable
  U := AddVarDecAndOpen('__U', typByte, srcPosNull);
  TreeElems.CloseElement;
  U.adicPar.hasAdic := decNone;
  U.adicPar.hasInit := false;
  U.location := locInterface;  //make visible
  //Create register IX as variable
  IX := AddVarDecAndOpen('__IX', typWord, srcPosNull);
  TreeElems.CloseElement;
  IX.adicPar.hasAdic := decNone;
  IX.adicPar.hasInit := false;
  IX.location := locInterface;  //make visible
end;
procedure TCompiler_PIC16.CreateBooleanOperations;
var
  f: TEleFunDec;
begin
  /////////////// Boolean type ////////////////////
  //Methods-Operators
  TreeElems.OpenElement(typBool);
  f:=CreateInBOMethod(typBool, ':=',  '_set', typBool, typNull, @SIF_bool_asig_bool);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
  f:=CreateInUOMethod(typBool  , 'NOT', '_not', typBool, @SIF_not_bool, opkUnaryPre);
  f:=CreateInBOMethod(typBool, 'AND', '_and', typBool, typBool, @SIF_bool_and_bool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typBool, 'OR' , '_or' , typBool, typBool, @SIF_bool_or_bool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typBool, 'XOR', '_xor', typBool, typBool, @SIF_bool_xor_bool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typBool, '=' ,  '_equ', typBool, typBool, @SIF_bool_equal_bool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typBool, '<>',  '_dif', typBool, typBool, @SIF_bool_xor_bool);
  f.fConmutat := true;
  TreeElems.CloseElement;   //Close Type
end;
procedure TCompiler_PIC16.CreateByteOperations;
var
  f, f1, f2: TEleFunDec;
begin
  //Methods-Operators
  TreeElems.OpenElement(typByte);
  //Simple Assignment
  f:=CreateInBOMethod(typByte, ':=', '_set', typByte, typNull, @SIF_byte_asig_byte);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
  //Array-pinter Assignment
  f1 := CreateInBOMethod(typByte, '', '_getitem', typByte, typByte, @SIF_GetItemIdxByte);
  f1.getset := gsGetInItem;
  f2 := CreateInBOMethod(typByte, '', '_getitem', typWord, typByte, @SIF_GetItemIdxWord);
  f2.getset := gsGetInItem;
  //AddCallerToFrom(IX, f.bodyNode);  //Dependency
  f := CreateInTerMethod(typByte, '_setitem', typByte, typByte, typNull, @SIF_SetItemIndexByte);
  f.asgMode := asgSimple;
  f.getset := gsSetInItem;
  f1.funset := f;         //Connect to getter
  f := CreateInTerMethod(typByte, '_setitem', typWord, typByte, typNull, @SIF_SetItemIndexWord);
  f.asgMode := asgSimple;
  f.getset := gsSetInItem;
  f2.funset := f;         //Connect to getter

  //Assignment with operations
  f:=CreateInBOMethod(typByte, '+=', '_aadd',typByte, typNull, @SIF_byte_aadd_byte);
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typByte, '-=', '_asub',typByte, typNull, @SIF_byte_asub_byte);
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typByte, '+' , '_add', typByte, typByte, @SIF_byte_add_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, '+' , '_add', typWord, typWord, @SIF_byte_add_word);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, '-' , '_sub', typByte, typByte, @SIF_byte_sub_byte);
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  f:=CreateInBOMethod(typByte, '*' , '_mul', typByte, typWord, @SIF_byte_mul_byte);
  f.fConmutat := true;
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  sifByteMulByte := f;
  f:=CreateInBOMethod(typByte, 'DIV' , '_div', typByte, typByte, @SIF_byte_div_byte);
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  sifByteDivByte := f;
  f:=CreateInBOMethod(typByte, 'MOD' , '_mod', typByte, typByte, @SIF_byte_mod_byte);
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  sifByteModByte := f;

  f:=CreateInBOMethod(typByte, 'AND','_and', typByte, typByte, @SIF_byte_and_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, 'OR' ,'_or' , typByte, typByte, @SIF_byte_or_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, 'XOR','_xor', typByte, typByte, @SIF_byte_xor_byte);
  f.fConmutat := true;
  f:=CreateInUOMethod(typByte, 'NOT','_not', typByte, @SIF_not_byte, opkUnaryPre);
  f:=CreateInBOMethod(typByte, '=' , '_equ', typByte, typBool, @SIF_byte_equal_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, '<>', '_dif', typByte, typBool, @SIF_byte_difer_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, '>' , '_gre', typByte, typBool, @SIF_byte_great_byte);
  f:=CreateInBOMethod(typByte, '<' , '_les', typByte, typBool, @SIF_byte_less_byte);
  f:=CreateInBOMethod(typByte, '>=', '_gequ',typByte, typBool, @SIF_byte_gequ_byte);
  f:=CreateInBOMethod(typByte, '<=', '_lequ',typByte, typBool, @SIF_byte_lequ_byte);
  f:=CreateInBOMethod(typByte, '>>', '_shr', typByte, typByte, @SIF_byte_shr_byte);  { TODO : Definir bien la precedencia }
  f:=CreateInBOMethod(typByte, '<<', '_shl', typByte, typByte, @SIF_byte_shl_byte);
  TreeElems.CloseElement;   //Close Type
end;
procedure TCompiler_PIC16.CreateCharOperations;
var
  f: TEleFunDec;
begin
  /////////////// Char type ////////////////////
  TreeElems.OpenElement(typChar);
  f:=CreateInBOMethod(typChar, ':=', '_set', typChar, typNull, @SIF_char_asig_char);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
  //opr.CreateOperation(typString, @SIF_char_asig_string);
  f:=CreateInBOMethod(typChar, '=' , '_equ', typChar, typBool, @SIF_char_equal_char);
  f.fConmutat := true;
  f:=CreateInBOMethod(typChar, '<>', '_dif', typChar, typBool, @SIF_char_difer_char);
  f.fConmutat := true;
  TreeElems.CloseElement;   //Close Type
end;
procedure TCompiler_PIC16.CreateWordOperations;
var
  f: TEleFunDec;
begin
  /////////////// Word type ////////////////////
  TreeElems.OpenElement(typWord);
  f:=CreateInBOMethod(typWord, ':=' ,'_set' , typWord, typNull, @SIF_word_asig_word);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  f:=CreateInBOMethod(typWord, ':=' ,'_set' , typByte, typNull, @SIF_word_asig_byte);
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typWord, '+=' ,'_aadd', typByte, typNull, @SIF_word_aadd_byte);
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typWord, '+=' ,'_aadd', typWord, typNull, @SIF_word_aadd_word);
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typWord, '-=' ,'_asub', typByte, typNull, @SIF_word_asub_byte);
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typWord, '-=' ,'_asub', typWord, typNull, @SIF_word_asub_word);
  AddCallerToFrom(E, f.bodyNode);  // Require _E
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typWord, '+'  , '_add', typByte, typWord, @SIF_word_add_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, '+'  , '_add', typWord, typWord, @SIF_word_add_word);
  f.fConmutat := true;
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  f:=CreateInBOMethod(typWord, '-'  , '_sub', typByte, typWord, @SIF_word_sub_byte);
  f:=CreateInBOMethod(typWord, '-'  , '_sub', typWord, typWord, @SIF_word_sub_word);
  f:=CreateInBOMethod(typWord, '*' , '_mul', typByte, typWord, @SIF_word_mul_byte);
  f.fConmutat := true;

  f:=CreateInBOMethod(typWord, 'DIV' , '_div', typWord, typWord, @SIF_word_div_word);
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  sifWordDivWord := f;
  f:=CreateInBOMethod(typWord, 'MOD' , '_mod', typWord, typWord, @SIF_word_mod_word);
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  sifWordModWord := f;

  f:=CreateInBOMethod(typWord, 'AND', '_and', typByte, typByte, @SIF_word_and_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, 'AND', '_and', typWord, typWord, @SIF_word_and_word);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, 'OR' , '_or' , typWord, typWord, @SIF_word_or_word);
  f.fConmutat := true;
  f:=CreateInUOMethod(typWord, 'NOT', '_not', typWord, @SIF_not_word, opkUnaryPre);
  f:=CreateInBOMethod(typWord, '>>' , '_shr', typByte, typWord, @SIF_word_shr_byte); { TODO : Definir bien la precedencia }
  f:=CreateInBOMethod(typWord, '<<' , '_shl', typByte, typWord, @SIF_word_shl_byte);
  sifWordShlByte := f;         //Guarda referencia

  f:=CreateInBOMethod(typWord, '=' , '_equ' , typWord, typBool, @SIF_word_equal_word);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, '=' , '_equ' , typByte, typBool, @SIF_word_equal_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, '<>', '_dif' , typWord, typBool, @SIF_word_difer_word);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, '>=', '_gequ', typWord, typBool, @SIF_word_gequ_word);
  AddCallerToFrom(E, f.bodyNode);  //Dependency
  f:=CreateInBOMethod(typWord, '<' , '_les' , typWord, typBool, @SIF_word_less_word);
  f:=CreateInBOMethod(typWord, '>' , '_gre' , typWord, typBool, @SIF_word_great_word);
  f:=CreateInBOMethod(typWord, '<=', '_lequ', typWord, typBool, @SIF_word_lequ_word);
  //Methods
  f:=CreateInUOMethod(typWord, '', 'low' , typByte, @word_Low);
  f:=CreateInUOMethod(typWord, '', 'high', typByte, @word_High);

  TreeElems.CloseElement;   //Close Type
end;
procedure TCompiler_PIC16.CreateSystemUnitInAST;
{Initialize the system elements. Must be executed just one time when compiling.}
var
  uni: TEleUnit;
  pars: TAstParamArray;  //Array of parameters
  f, sifDelayMs, sifWord: TEleFunDec;
begin
  //////// Funciones del sistema ////////////
  //Implement calls to Code Generator
  callDefineArray  := @DefineArray;
  callDefineObject := @DefineObject;
  callDefinePointer:= @DefinePointer;
  callValidRAMaddr := @ValidRAMaddr;
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
  TreeElems.AddElementAndOpen(uni);  //Open Unit
  CreateSystemTypesAndVars;
  curLocation := locInterface;   {Maybe not needed because element here are created directly.}
  //Creates operations
  CreateBooleanOperations;
  CreateByteOperations;
  CreateCharOperations;
  CreateWordOperations;

  /////////////// DWord type ////////////////////
  TreeElems.OpenElement(typDWord);
  f:=CreateInBOMethod(typDWord, ':=' ,'_set' , typDWord, typNull, @SIF_dword_asig_dword);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typDWord, ':=' ,'_set' , typByte, typNull, @SIF_dword_asig_byte);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typDWord, ':=' ,'_set' , typWord, typNull, @SIF_dword_asig_word);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
  AddCallerToFrom(H, f.bodyNode);  //Dependency
//  f:=CreateInBOMethod(typDWord, '+=' ,'_aadd', typByte, typNull, @SIF_word_aadd_byte);
//  f.getset := gsSetOther;
//  f:=CreateInBOMethod(typDWord, '+=' ,'_aadd', typDWord, typNull, @SIF_word_aadd_word);
//  f.getset := gsSetOther;
//  f:=CreateInBOMethod(typDWord, '-=' ,'_asub', typByte, typNull, @SIF_word_asub_byte);
//  f.getset := gsSetOther;
//  f:=CreateInBOMethod(typDWord, '-=' ,'_asub', typDWord, typNull, @SIF_word_asub_word);
//  AddCallerToFrom(E, f.bodyNode);  // Require _E
//  f.getset := gsSetOther;
//  f:=CreateInBOMethod(typDWord, '+'  , '_add', typByte, typDWord, @SIF_word_add_byte);
//  f.fConmutat := true;
  f:=CreateInBOMethod(typDWord, '+'  , '_add', typDWord, typDWord, @SIF_dword_add_dword);
  f.fConmutat := true;
  f:=CreateInBOMethod(typDWord, '+'  , '_add', typByte, typDWord, @SIF_dword_add_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typDWord, '+'  , '_add', typWord, typDWord, @SIF_dword_add_word);
  f.fConmutat := true;
//  AddCallerToFrom(H, f.bodyNode);  //Dependency
//  f:=CreateInBOMethod(typDWord, '-'  , '_sub', typByte, typDWord, @SIF_word_sub_byte);
//  f:=CreateInBOMethod(typDWord, '-'  , '_sub', typDWord, typDWord, @SIF_word_sub_word);
//  f:=CreateInBOMethod(typDWord, '*' , '_mul', typByte, typDWord, @SIF_word_mul_byte);
//  f.fConmutat := true;
//
//  f:=CreateInBOMethod(typDWord, 'DIV' , '_div', typDWord, typDWord, @SIF_word_div_word);
//  AddCallerToFrom(H, f.bodyNode);  //Dependency
//  sifWordDivWord := f;
//  f:=CreateInBOMethod(typDWord, 'MOD' , '_mod', typDWord, typDWord, @SIF_word_mod_word);
//  AddCallerToFrom(H, f.bodyNode);  //Dependency
//  sifWordModWord := f;
//
//  f:=CreateInBOMethod(typDWord, 'AND', '_and', typByte, typByte, @SIF_word_and_byte);
//  f.fConmutat := true;
//  f:=CreateInBOMethod(typDWord, 'AND', '_and', typDWord, typDWord, @SIF_word_and_word);
//  f.fConmutat := true;
//  f:=CreateInBOMethod(typDWord, 'OR' , '_or' , typDWord, typDWord, @SIF_word_or_word);
//  f.fConmutat := true;
//  f:=CreateInUOMethod(typDWord, 'NOT', '_not', typDWord, @SIF_not_word, opkUnaryPre);
//  f:=CreateInBOMethod(typDWord, '>>' , '_shr', typByte, typDWord, @SIF_word_shr_byte); { TODO : Definir bien la precedencia }
//  f:=CreateInBOMethod(typDWord, '<<' , '_shl', typByte, typDWord, @SIF_word_shl_byte);
//
//  f:=CreateInBOMethod(typDWord, '=' , '_equ' , typDWord, typBool, @SIF_word_equal_word);
//  f.fConmutat := true;
//  f:=CreateInBOMethod(typDWord, '=' , '_equ' , typByte, typBool, @SIF_word_equal_byte);
//  f.fConmutat := true;
//  f:=CreateInBOMethod(typDWord, '<>', '_dif' , typDWord, typBool, @SIF_word_difer_word);
//  f.fConmutat := true;
//  f:=CreateInBOMethod(typDWord, '>=', '_gequ', typDWord, typBool, @SIF_word_gequ_word);
//  AddCallerToFrom(E, f.bodyNode);  //Dependency
//  f:=CreateInBOMethod(typDWord, '<' , '_les' , typDWord, typBool, @SIF_word_less_word);
//  f:=CreateInBOMethod(typDWord, '>' , '_gre' , typDWord, typBool, @SIF_word_great_word);
//  f:=CreateInBOMethod(typDWord, '<=', '_lequ', typDWord, typBool, @SIF_word_lequ_word);
//  //Methods
//  f:=CreateInUOMethod(typDWord, '', 'low' , typByte, @word_Low);
//  f:=CreateInUOMethod(typDWord, '', 'high', typByte, @word_High);

  TreeElems.CloseElement;   //Close Type

  ///////////////// System INLINE functions (SIF) ///////////////
  //Create system function "delay_ms". Too complex as SIF. We better implement as SNF.
//  setlength(pars, 0);  //Reset parameters
//  AddParam(pars, 'ms', srcPosNull, typWord, decRegis);  //Add parameter
//  sifDelayMs :=
//  AddSIFtoUnit('delay_ms', typNull, srcPosNull, pars, @SIF_delay_ms);

  //Create system function "inc"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  sifFunInc :=
  AddSIFtoUnit('inc', typNull, srcPosNull, pars, @SIF_Inc);

  //Create system function "dec"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  AddSIFtoUnit('dec', typNull, srcPosNull, pars, @SIF_Dec);

  //Create system function "ord"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);
  AddSIFtoUnit('ord', typByte, srcPosNull, pars, @SIF_Ord);

  //Create system function "chr"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);
  AddSIFtoUnit('chr', typChar, srcPosNull, pars, @SIF_Chr);

  //Create system function "byte"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  AddSIFtoUnit('byte', typByte, srcPosNull, pars, @SIF_Byte);

  //Create system function "word"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  sifWord :=
  AddSIFtoUnit('word', typWord, srcPosNull, pars, @SIF_Word);
  AddCallerToFrom(H, sifWord.BodyNode);  //Require H

  //Create system function "word"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  //sifWord :=
  AddSIFtoUnit('dword', typDWord, srcPosNull, pars, @SIF_DWord);
  //AddCallerToFrom(H, sifWord.BodyNode);  //Require H

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

  //Close Unit
  TreeElems.CloseElement;
end;

constructor TCompiler_PIC16.Create;
begin
  inherited Create;
  //OnNewLine:=@cInNewLine;
  syntaxMode := modPicPas;   //Por defecto en sintaxis nueva

  OnError := procedure (msg: string);
  begin
    GenError(msg);
  end;
  OnWarning := procedure (msg: string);
  begin
    GenWarning(msg);
  end;
  //Conecta el parser de directivas al Generador de Cödigo
  OnSetGeneralORG := procedure(value: integer);
  begin
     GeneralORG := value
  end;
  OnSetCpuMode := procedure(value: string);
  begin
    if value = '6502' then begin
      cpuMode := cpu6502;
      picCore.Model := '6502';
    end;
    if value = '65C02' then begin
      cpuMode := cpu65C02;
      picCore.Model := '65C02';
    end;
  end;
  OnSetFrequency := procedure(f: Longint; value: string);
  begin
    case UpperCase(value) of
    'KHZ': f := f * 1000;
    'MHZ': f := f * 1000000;
    else
      GenErrorDir(ER_ERROR_DIREC);
      exit;
    end;
    if f>picCore.MaxFreq then begin
      GenErrorDir(ER_TOOHIGHFRE);
      exit;
    end;
    picCore.frequen:=f; //asigna frecuencia
  end;
  OnSetStatRAMCom := procedure(value: string);
  begin
    picCore.SetStatRAMCom(txtMsg);
    if picCore.MsjError<>'' then GenErrorDir(picCore.MsjError);
  end;
  OnSetDataAddr:= procedure(value: string);
  begin
    picCore.SetDataAddr(txtMsg);
    if picCore.MsjError<>'' then GenErrorDir(picCore.MsjError);
  end;
  OnClearStateRam := procedure(value: string);
  begin
    picCore.DisableAllRAM;
  end;
  OnReadPicModel:= function(): string;
  begin
    Result := picCore.Model;
  end;
  OnSetPicModel := procedure(value: string);
  begin
    picCore.Model := value;
  end;
  OnReadFrequen := function(): Single;
  begin
    Result := picCore.frequen;
  end;
  OnSetFrequen := procedure(value: single);
  begin
    picCore.frequen := round(Value);
  end;
  OnReadMaxFreq := function(): Single;
  begin
    Result := PICCore.MaxFreq;
  end;
  OnSetMaxFreq := procedure(value: single);
  begin
    PICCore.MaxFreq := round(Value);
  end;
  OnReadORG:= function(): Single;
  begin
    Result := picCore.iRam;
  end;
  OnSetORG := procedure(value: single);
  begin
    picCore.iRam:= round(Value);
  end;

end;
destructor TCompiler_PIC16.Destroy;
begin
  inherited Destroy;
end;

end.
//1457
