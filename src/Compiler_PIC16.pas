{Unit that defines the compiler building it from its parts.

}
unit Compiler_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, LazLogger, P65C02utils, CPUCore, CompBase,
  ParserDirec, CompGlobals, AstElemP65, AstTree, ParserASM_6502, MirList,
  LexPas, SIF_P65pas, StrUtils;
type
  { TCompiler_PIC16 }
  TCompiler_PIC16 = class(TParserDirecBase)
  private  //Funciones básicas
    addBootldr: integer;  //Address where start Bootloader.
    addVariab : integer;  //Address where start Variables section.
    addFuncts : integer;  //Address where start function section.
//    procedure ConstantFoldExpr(eleExp: TAstExpress);
    procedure AddParam(var pars: TAstParamArray; parName: string;
      const srcPos: TSrcPos; typ0: TAstTypeDec; adicDec: TAdicDeclar);
    function AddSIFtoUnit(name: string; sfi: TSysFunID; retType: TAstTypeDec;
      const srcPos: TSrcPos; const pars: TAstParamArray): TAstFunDec;
    function AddSNFtoUnit(name: string; retType: TAstTypeDec;
      const srcPos: TSrcPos; var pars: TAstParamArray; codSys: TCodSysNormal
      ): TAstFunDec;
    procedure cbClearStateRam;
    function cbReadFrequen: Single;
    function cbReadMaxFreq: Single;
    function cbReadORG: Integer;
    function cbReadPicModel: string;
    procedure cbSetCpuMode(value: string);
    procedure cbSetDataAddr(value: string);
    procedure cbSetFrequen(value: single);
    procedure cbSetFrequency(f: Longint; value: string);
    procedure cbSetGeneralORG(value: integer);
    procedure cbSetMaxFreq(value: single);
    procedure cbSetORG(value: integer);
    procedure cbSetPicModel(value: string);
    procedure cbSetStatRAMCom(value: string);
    procedure Cod_EndProgram;
    procedure Cod_StartProgram;
    procedure ConvertBody(mcont: TMirContainer; sntBlock: TAstCodeCont);
    procedure CreateBooleanOperations;
    procedure CreateByteOperations;
    procedure CreateCharOperations;
    procedure CreateDWordOperations;
    function CreateInBOMethod(clsType: TAstTypeDec; opr: string; name: string;
      parType: TAstTypeDec; retType: TAstTypeDec): TAstFunDec;
    function CreateInTerMethod(clsType: TAstTypeDec; name: string; parType1,
      parType2: TAstTypeDec; retType: TAstTypeDec): TAstFunDec;
    function CreateInUOMethod(clsType: TAstTypeDec; opr: string; name: string;
      retType: TAstTypeDec; operTyp: TOperatorType = opkUnaryPre): TAstFunDec;
    procedure CreateWordOperations;
    procedure DefineArray(etyp: TAstTypeDec);
    procedure CreateSystemTypesAndVars;
    procedure DefineObject(etyp: TAstTypeDec);
    procedure DefinePointer(etyp: TAstTypeDec);
    procedure GenerateMIR;
    function PICName: string;
//    procedure PrepareBody(cntBody, sntBlock: TAstCodeCont);
//    procedure PrepareSentences;
    procedure CreateVarsAndPars;

  private  //Compilers steps
//    procedure EvaluateConstantDeclare;
//    procedure ConstantFolding;
//    procedure ConstanPropagation;
    procedure DoCompile;
    procedure DoOptimize;
//    procedure DoGenerateCode;
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
  sifByteMulByte, sifByteDivByte, sifByteModByte: TAstFunDec;
  sifWordDivWord, sifWordModWord, sifWordShlByte: TAstFunDec;
var
  //System variables used as registers
  H      : TAstVarDec;  //To load the high byte of words.
  E      : TAstVarDec;  //To load the high word of dwords.
  U      : TAstVarDec;  //To load the high word of dwords.
  IX     : TAstVarDec;  //To index operands

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
  ParserDirec.SetLanguage;
//  ParserASM_6502.SetLanguage;
  SIF_P65pas.SetLanguage;
  {$I _language\tra_Compiler.pas}
end;
//procedure TCompiler_PIC16.ConstantFoldExpr(eleExp: TAstExpress);
//{Performs:
//- Constant evaluation, for constant nodes that can be evaluated.
//- Constant folding, for expression nodes, that returns constants.
//Note the similarity of this method with GenCodeExpr().}
//var
//  funcBase: TAstFunDec;
//  ele: TAstElement;
//  parExpr: TAstExpress;
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
//          parExpr := TAstExpress(ele);
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
//        parExpr := TAstExpress(ele);
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
//procedure TCompiler_PIC16.ConstantFolding;
//{Do a fold constant optimization and evaluate constant expresions. }
//  procedure ConstantFoldBody(body: TAstBody);
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
//    expFun: TAstExpress;
//    sen: TAstSentence;
//    eleSen: TAstElement;
//    ele: TAstElement;
//    i: Integer;
//  begin
//    //Process body
//    TreeElems.OpenElement(body);
//    for eleSen in TreeElems.curNode.elements do begin
//      if eleSen.idClass <> eleSenten then continue;
//      sen := TAstSentence(eleSen);
//      if sen.sntType = sntAssign then begin    //assignment
//        {After preparation, assignment sentences could be splitted in several assignment.}
//        for ele in sen.elements do begin
//          expFun := TAstExpress(ele);
//          ConstantFoldExpr(expFun);  //Try to evaluate constant.
//          if HayError then exit;
//        end;
//      end else if sen.sntType = sntProcCal then begin
//        {After preparation, sntProcCal sentences could be include several
//        assignment before the Call.}
//        for i:=0 to sen.elements.Count-2 do begin  //Excluding the call
//          expFun := TAstExpress(sen.elements[i]);
//          ConstantFoldExpr(expFun);  //Try to evaluate constant.
//          if HayError then exit;
//        end;
//      end;
//    end;
//    TreeElems.CloseElement;              //Close the Body.
//  end;
//var
//  fun : TAstFunDec;
//  bod: TAstBody;
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
//  function IsForm_var_assign_const(assigExp: TAstExpress;
//        out varDec: TAstVarDec;
//        out consVal: TConsValue
//        ): boolean;
//  {Indicates if the assigment expression is of the form:
//     <variable> := <constant>
//  }
//  var
//    leftOp, rightOp: TAstExpress;
//  begin
//    if assigExp.opType <> otFunct then exit(false);  //Validation
//    //Must be an assignment expression
//    leftOp := TAstExpress(assigExp.elements[0]);
//    rightOp := TAstExpress(assigExp.elements[1]);
//    if (rightOp.Sto = stConst) and rightOp.evaluated then begin
//       //It's the form: <variable> := <constant>
//      varDec := leftOp.vardec;  //Takes var declaration
//      consVal := rightOp.value;
//      exit(true);
//    end;
//  end;
//  function ChangeToConstant(Op: TAstExpress; varDec: TAstVarDec; const consVal: TConsValue): boolean;
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
//  function ReplaceVarByConst(assigExp: TAstExpress; varDec: TAstVarDec;
//        const consVal: TConsValue): boolean;
//  {Replace the variable by a constant in the right part of an assignment expression.
//  It replacing is done, returns TRUE.
//  }
//  var
//    rightOp, Op: TAstExpress;
//    ele: TAstElement;
//  begin
//    //Search at the right expression
//    Result := false;
//    rightOp := TAstExpress(assigExp.elements[1]);
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
//        Op := TAstExpress(ele);
//        if ChangeToConstant(Op, varDec, consVal) then begin
//          Result := True;
//        end;
//      end;
//    end;
//    end;
//  end;
//  procedure ConstantPropagBody(body: TAstBody);
//  {Do constant propagation in all sentences of the body. }
//  var
//    assigExp, assigToDelete: TAstExpress;
//    sen: TAstSentence;
//    eleSen, par: TAstElement;
//    n, i: Integer;
//    varDec, varDecToDelete: TAstVarDec;
//    consVal: TConsValue;
//    replaceMode, replaced: Boolean;
//  begin
//    //Process body
//    TreeElems.OpenElement(body);
//    for eleSen in TreeElems.curNode.elements do begin
//      if eleSen.idClass <> eleSenten then continue;
//      sen := TAstSentence(eleSen);
//      if sen.sntType = sntAssign then begin    //Assignment
//        //Explore previous posssible assigments
//        n := sen.elements.Count;
//        replaceMode := false;
//        replaced := false;
//        for i:=0 to n - 1 do begin
//          //Search the type: <variable> := <constant>
//          assigExp := TAstExpress(sen.elements[i]);  //Shoudn't fail
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
//  fun : TAstFunDec;
//  bod: TAstBody;
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
//procedure TCompiler_PIC16.PrepareBody(cntBody, sntBlock: TAstCodeCont);
//{Do a separation for assigmente sentences in order to have the "three-address code" form
//like used in several compilers.
//Parameters:
//  cntBody  -> The main Body of a procedure or the main program. This will be used
//               as reference to locate the new variable declarations.
//  sntBlock  -> Block of code where are the sentences to need be prepared. It's the
//               same of "container" except when "block" is nested like in a condiitonal.
//}
//  function MoveParamToAssign(curContainer: TAstElement; Op: TAstExpress;
//                             parvar: TAstVarDec): TAstExpress;
//  {Mueve el nodo especificado "Op", que representa a un parámetro de la función, a una
//  nueva instruccion de asignación (que es creada al inicio del bloque "curContainer") y
//  reemplaza el nodo faltante por una variable temporal que es la que se crea en la
//  instrucción de asignación.
//  Es similar a MoveNodeToAssign() pero no reemplaza el nodo movido y no crea una variable
//  auxiliar, sino que usa "parvar".
//  Retorna la instrucción de asignación creada.
//  }
//  var
//    _setaux: TAstExpress;
//    Op1aux: TAstExpress;
//    funSet: TAstFunDec;
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
//  function SplitProcCall(curContainer: TAstElement; expMethod: TAstExpress): boolean; forward;
//  function SplitSet(curContainer: TAstElement; setMethod: TAstElement): boolean;
//  {Verify if a set expression has more than three operands. If so then
//  it's splitted adding one or more aditional set sentences, at the beggining of
//  "curContainer".
//  If at least one new set sentence is added, returns TRUE.}
//  var
//    Op2, parExp, new_set, Op1, idx: TAstExpress;
//    par: TAstElement;
//  begin
//    Result := false;
//    if TAstExpress(setMethod).fundec.getset <> gsSetInSimple then exit;
//    //Split expressions in second operand of assignment.
//    Op2 := TAstExpress(setMethod.elements[1]);  //Takes assignment source.
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
//          parExp := TAstExpress(par);
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
//  function SplitExpress(curContainer: TAstElement; expMethod: TAstExpress): boolean;
//  {Verify if an expression has more than three operands. If so then
//  it's splitted adding one or more set sentences.
//  If at least one new set sentence is added, returns TRUE.}
//  var
//    parExp, new_set: TAstExpress;
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
//          parExp := TAstExpress(par);
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
//  function SplitProcCall(curContainer: TAstElement; expMethod: TAstExpress): boolean;
//  {Split a procedure (not INLINE) call instruction, inserting an assignment instruction
//  for each parameter.}
//  var
//    parExp, new_set: TAstExpress;
//    funcBase: TAstFunDec;
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
//        parExp := TAstExpress(expMethod.elements[0]);  //Take parameter element
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
//  sen: TAstSentence;
//  eleSen, _set, ele, _proc: TAstElement;
//  _exp, Op1, Op2, val1, val2: TAstExpress;
//  _blk, _blk0: TAstCodeCont;
//begin
//  //Prepare assignments for arrays.
//  for eleSen in sntBlock.elements do begin
//    if eleSen.idClass <> eleSenten then continue;
//    //We have a sentence here.
//    sen := TAstSentence(eleSen);
//    if sen.sntType = sntAssign then begin  //Assignment
//      _set := sen.elements[0];  //Takes the _set method.
//      Op1 := TAstExpress(_set.elements[0]);  //Takes assigment target.
//      Op2 := TAstExpress(_set.elements[1]);  //Takes assigment target.
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
//    sen := TAstSentence(eleSen);
//    if sen.sntType = sntAssign then begin  //Assignment
//      _set := sen.elements[0];  //Takes the one _set method.
//      SplitSet(sen, _set)  //Might generate additional assignments sentences
//    end else if sen.sntType = sntProcCal then begin  //Procedure call
//      _proc := sen.elements[0];  //Takes the proc.
//      SplitProcCall(sen, TAstExpress(_proc))
//    end else if sen.sntType in [sntIF, sntREPEAT, sntWHILE] then begin
//      //There are expressions and blocks inside conditions and loops.
//      for ele in sen.elements do begin
//        if ele.idClass = eleCondit then begin  //It's a condition
//          _exp := TAstExpress(ele.elements[0]);  //The first item is a TAstExpress
//          SplitExpress(ele, _exp)
//        end else if ele.idClass = eleBlock then begin   //body of IF
//          _blk := TAstCodeCont(ele);  //The first item is a TAstExpress
//          PrepareBody(cntBody, _blk);
//        end;
//      end;
//    end else if sen.sntType = sntFOR then begin
//      //FOR sentences could need some aditional changes.
//      _blk0 := nil;
//      for ele in sen.elements do begin
//        if ele.idClass = eleCondit then begin  //It's a condition
//          _exp := TAstExpress(ele.elements[0]);  //The first item is a TAstExpress
//          SplitExpress(ele, _exp)
//        end else if ele.idClass = eleBlock then begin   //Initialization or body
//          _blk := TAstCodeCont(ele);  //The first item is a TAstExpress
//          PrepareBody(cntBody, _blk);
//          if _blk0 = nil then _blk0 := _blk;  //Get intialization block
//        end;
//      end;
//      //Get first and last value of index.
//      val1 := TAstExpress(_blk0.elements[0].elements[1]);
//      val2 := TAstExpress(_exp.elements[1]);
//      //Special cases
//      if (val1.opType = otConst) and (val2.opType = otConst) then begin
//        if val1.val > val2.val then begin
//          //Unreachable code
////          TreeElems.DeleteTypeNode();
//        end;
//      end;
//    end else if sen.sntType = sntExit then begin
//      if sen.elements.Count>0 then begin   //If there is argument
//        _exp := TAstExpress(sen.elements[0]);  //The first item is a TAstExpress
//        SplitExpress(sen, _exp)
//      end;
//    end;
//  end;
//end;
//procedure TCompiler_PIC16.PrepareSentences;
//var
//  fun : TAstFunDec;
//  bod: TAstBody;
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
  xvar   : TAstVarDec;
  fun    : TAstFunDec;
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
//      xvar := TAstVarDec(elem);
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
//      xvar := TAstVarDec(elem);
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

//Reading from AST
procedure TCompiler_PIC16.ConvertBody(mcont: TMirContainer; sntBlock: TAstCodeCont);
{Convert a ASTBody to instructions in MIR representation.
Parameters:
  cntFunct  -> The function where MIR will be created, or the main program. This will
               be used as reference to locate the new variable declarations.
  sntBlock  -> Block of code where are the sentences to need be prepared. It's the
               same of "cntFunct" except when "block" is nested like in a condiitonal.
}
  function SplitSet(setMethod: TAstExpress): boolean;
  {Process a set sentence. If a set expression has more than three operands
  it's splitted adding one or more aditional set sentences, at the beggining of
  "curContainer".
  If at least one new set sentence is added, returns TRUE.}
  var
    Op2, Op1: TAstExpress;
    mirvardec: TMirVarDec;
    astvardec: TAstVarDec;
  begin
    Result := false;
    Op1 := TAstExpress(setMethod.elements[0]);  //Takes target.
    if Op1.opType <> otVariab then exit;
    //Split expressions in second operand of assignment.
    Op2 := TAstExpress(setMethod.elements[1]);  //Takes assignment source.
    astvardec := Op1.vardec;
    if astvardec = nil then begin
      //No variable declaration. Maybe a variable like: pointer^ := ...
      mirvardec := nil;
    end else begin
      mirvardec := TMirVarDec(astvardec.mirVarDec);
    end;
    AddAssign(mcont, mirvardec, Op2);
  end;
  function SplitExpress(expMethod: TAstExpress): boolean;
  {Verify if an expression has more than three operands. If so then
  it's splitted adding one or more set sentences.
  If at least one new set sentence is added, returns TRUE.}
  var
    parExp, new_set: TAstExpress;
    par: TAstElement;
  begin
    Result := false;
    if (expMethod.opType = otFunct) then begin  //Neither variables nor constants.
      {We expect parameters should be simple operands (Constant or variables)
      otherwise we will move them to a separate assignment}
      mirRep.AddFunCall(mcont, expMethod);
    end;
  end;
//  procedure GotoToEnd;
//  {Add a goto to the End of code is there is some aditional code after teh point we
//  are inserting instructions.}
//  var
//    gotToEnd: TMirGoto;
//    insPoint0: Integer;
//  begin
//    if InsertModeActive then begin
//      //There are instruction after. We need a GOTO to the end.
//      gotToEnd := AddGoto(cntFunct);
//      insPoint0 := insPoint;  //Save position.
//      ClearInsertMode;
//      EndGoto(cntFunct, gotToEnd);
//      SetInsertMode(insPoint0);
//    end;
//  end;
  function ReadValidConditionBlock(sen: TAstSentence; var i: Integer;
           out condit: TAstExpress; out block: TAstCodeCont): boolean;
  var
    expBool: TAstExpress;
  begin
    while i<sen.elements.Count do begin
      expBool := TAstExpress(sen.elements[i]);   //Even element is condition
      block   := TAstCodeCont(sen.elements[i+1]); //Odd element is block
      condit := TAstExpress(expBool.elements[0]);
      if (condit.opType = otConst) and condit.evaluated and
         (condit.value.ValBool=false) then begin
         //FALSE conditions are not considered.
         inc(i, 2);      //Try the next.
         Continue;
      end;
      if block.elements.Count = 0 then begin
         //FALSE conditions are not considered.
        inc(i, 2);      //Try the next.
        Continue;
      end;
      //Is valid
      inc(i, 2);  //Point to next position
      Exit(true);
    end;
    //Not found
    Exit(false);
  end;
  procedure ConvertIF(sen: TAstSentence);
  {COnvert an IF AST structure to the MIR representation. Only a BASIC simplification is
  applied. Optimization needs to be done later.}
  var
    i, lblEndIf: Integer;
    ifgot: TMirIfGoto;
    condit: TAstExpress;
    _blk: TAstCodeCont;
    gotToEnd: TMirGoto;
  begin
    //There are expressions and blocks inside conditions and loops.
    gotToEnd := Nil;
    lblEndIf := -1;
    i := 0;
    while ReadValidConditionBlock(sen, i, condit, _blk) do begin
      //if (condit.opType = otConst) and (condit.value.ValBool=true) then begin
      //  //True conditions (or ELSE) are the last to be executed.
      //  ConvertBody(mcont, _blk);
      //  GotoToEnd;
      //  break;  //No more is executed.
      //end else begin      //It's function
      //  if nextCondit<>nil then begin  //There are more conditions.
          //We add IF negated because normal form is: IF NOT ... GOTO ...
          ifgot := mirRep.AddIfGoto(mcont, condit, true);
          ConvertBody(mcont, _blk);
          //Add goto to the end of IF structure (including ELSEIF ...).
          if gotToEnd=Nil then begin  //First Goto
            gotToEnd := mirRep.AddGoto(mcont);
            lblEndIf := gotToEnd.ilabel;
          end else begin
            gotToEnd := mirRep.AddGoto(mcont, lblEndIf);
          end;
          //Add label
          mirRep.EndIfGoto(mcont, ifgot);
    end;
    if lblEndIf<>-1 then begin
      //There is al least one GOTO to the end of IF.
      mirRep.EndGoto(mcont, lblEndIf);
    end;
  end;
  procedure ConvertWHILE(sen: TAstSentence);
  {Convert an WHILE AST structure to the MIR representation.}
  var
    ifgot: TMirIfGoto;
    condit, expBool: TAstExpress;
    _blk: TAstCodeCont;
    lblBegin: TMirLabel;
  begin
    //There are expressions and blocks inside conditions and loops.
    expBool := TAstExpress(sen.elements[0]);   //Even element is condition
    _blk   := TAstCodeCont(sen.elements[1]); //Odd element is block
    if _blk.elements.Count=0 then exit;   //Empty block
    condit := TAstExpress(expBool.elements[0]);
    //Label to the beginning of the WHILE to test condition.
    lblBegin := mirRep.AddLabel(mcont);
    ifgot := mirRep.AddIfGoto(mcont, condit, true);
    ConvertBody(mcont, _blk);
    //Add goto to the begin of IF structure (including ELSEIF ...).
    mirRep.AddGoto(mcont, lblBegin);
    //Add label
    mirRep.EndIfGoto(mcont, ifgot);
  end;
var
  sen: TAstSentence;
  eleSen, _set, ele, _proc: TAstElement;
  Op1, Op2, val1, val2: TAstExpress;
  _blk, _blk0: TAstCodeCont;
begin
  //Prepare sentences
  for eleSen in sntBlock.elements do begin
    if eleSen.idClass = eleExpress then begin
      Op1 := TAstExpress(eleSen);
      if Op1.opType <> otFunct then begin
        GenError('Expected function or procedure: %s', [eleSen.name], eleSen.srcDec);
        exit;
      end;
      if Op1.fundec.getset = gsSetInSimple then begin
        //Es una asignación.
        {Una asignación es también, en la práctica, una llamada a una función, pero
        se trata de forma diferente porque:
         - Tiene una forma especial (siempre una variable por modificar como 1er parámetro)
         - Los algoritmos de optimización del MIR necesitan a las asignaciones en su forma canónica.}
        SplitSet(Op1)  //Might generate additional assignments sentences
      end else begin
        //Es una función cualquiera (del sistema o de usuario)
        {Notar que todas las funciones (INLINE o normal) se representan igual en el
        AST y el MIR}
        mirRep.AddFunCall(mcont, Op1);
      end;
    end else if eleSen.idClass = eleSenten then begin

    end else begin
      //Other AST element.
      GenError('Invalid Syntax element: %s', [eleSen.name], eleSen.srcDec);
      exit;
    end;
//    //We have a sentence here.
//    sen := TAstSentence(eleSen);
//    if if sen.sntType = sntIF then begin
//      ConvertIF(sen);
//    end else if sen.sntType = sntWHILE then begin
//      ConvertWHILE(sen);
//    end else if sen.sntType = sntFOR then begin
//      //FOR sentences could need some aditional changes.
//      _blk0 := nil;
//      for ele in sen.elements do begin
//        if ele.idClass = eleCondit then begin  //It's a condition
//          expBool := TAstExpress(ele.elements[0]);  //The first item is a TAstExpress
//          SplitExpress(ele, expBool)
//        end else if ele.idClass = eleBlock then begin   //Initialization or body
//          _blk := TAstCodeCont(ele);  //The first item is a TAstExpress
//          PrepareBody(mcont, _blk);
//          if _blk0 = nil then _blk0 := _blk;  //Get intialization block
//        end;
//      end;
//      //Get first and last value of index.
//      val1 := TAstExpress(_blk0.elements[0].elements[1]);
//      val2 := TAstExpress(expBool.elements[1]);
//      //Special cases
//      if (val1.opType = otConst) and (val2.opType = otConst) then begin
//        if val1.val > val2.val then begin
//          //Unreachable code
////          TreeElems.DeleteTypeNode();
//        end;
//      end;
//    end else if sen.sntType = sntExit then begin
//      if sen.elements.Count>0 then begin   //If there is argument
//        expBool := TAstExpress(sen.elements[0]);  //The first item is a TAstExpress
//        SplitExpress(sen, expBool)
//      end;
//    end;
  end;
end;
procedure TCompiler_PIC16.GenerateMIR;
var
  bod : TAstBody;
  elem : TAstElement;
  astFunDec: TAstFunDec;
  astVardec: TAstVarDec;
  astTypdec: TAstTypeDec;
  mirFunDec: TMirFunDec;
  mirVarDec: TMirVarDec;
  astConDec: TAstConsDec;
  mirConDec: TMirConDec;
  mirTypDec: TMirTypDec;
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
    if elem.nCalled=0 then Continue;  //No usado.
    if elem.idClass = eleConsDec then begin
      astConDec := TAstConsDec(elem);
      mirConDec := AddMirConDec(mirRep.root.declars, astConDec);
    end else if elem.idClass = eleVarDec then begin
      astVardec := TAstVarDec(elem);
      mirVarDec := AddMirVarDec(mirRep.root.declars, astVardec); //Agrega declaración en el MIR
      astVardec.mirVarDec := mirVarDec;  //Guarda referencia al MIR.;
    end else if elem.idClass = eleTypeDec then begin
      astTypdec := TAstTypeDec(elem);
      mirTypDec := AddMirTypDec(mirRep.root.declars, astTypdec); //Agrega declaración en el MIR
      astTypdec.mirTypDec := mirTypDec;  //Guarda referencia al MIR.
    end;
  end;
  for astFunDec in usedFuncs do begin
    if astFunDec.callType = ctUsrNormal then begin
      //Agrega al MIR y guarda referencia.
      mirFunDec := AddMirFunDecUNF(mirRep.root.declars, astFunDec);
      astFunDec.mirFunDec := mirFunDec;  //Guarda referencia al MIR.
      //Explora sus elementos internos.
      for elem In astFunDec.elemImplem do begin
          if elem.idClass = eleVarDec then begin
            astVarDec := TAstVarDec(elem);  //Guarda referencia
            //Agrega al MIR y guarda referencia.
            mirVarDec := AddMirVarDec(mirFunDec.declars, astVarDec);
            astVarDec.mirVarDec := mirVarDec;  //Guarda referencia al MIR.
          end else if elem.idClass = eleBody then begin
            ConvertBody(mirFunDec.instrucs, TAstBody(elem));
            //if HayError then exit;   //Puede haber error
          end;
      end;
      //Actualizmos los parámetros MIR después de explorar los elementos
      //internas AST (que incluye a los parámetros).
      mirFunDec.ReadParamsFromAST(astFunDec);
    end else if astFunDec.callType = ctSysNormal then begin
      mirFunDec := AddMirFunDecSNF(mirRep.root.declars, astFunDec);
      //System function doesn't have body.
    end;
  end;
  //Split body
  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
  ConvertBody(mirRep.root.instrucs, bod);
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
  TreeElems.RefreshAllUnits; //Actualiza lista de unidades
  RemoveUnusedFunc;       //Se debe empezar con las funciones. 1ms aprox.
  RemoveUnusedVars;       //Luego las variables. 1ms aprox.
  RemoveUnusedCons;       //1ms aprox.
  RemoveUnusedTypes;      //1ms aprox.
  UpdateFunLstCalled;     //Actualiza lista "lstCalled" de las funciones usadas.
  if HayError then exit;
  SeparateUsedFunctions;  //Updates "usedFuncs".
  //Genera la representación MIR
  GenerateMIR;
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
    v: TAstVarDec;
    subUsed: string;
  begin
    { *** Completar luego
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
    }
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
  stkUse := TreeElems.maxNesting/STACK_SIZE;
end;
procedure TCompiler_PIC16.GenerateListReport(lins: TStrings);
{Genera un reporte detallado de la compilación}
var
  curInst, opc: TP6502Inst;
  i: word;
  OpCodeCoun: array[low(TP6502Inst)..high(TP6502Inst)] of integer;
  tmpList: TStringList;
  txt, OpCode, Times, state: String;

  fun: TAstFunDec;
  caller : TAstEleCaller;
  called : TAstElement;
  curNesting, maxNesting: Integer;
  lstCalledAll: TAstListCalled;
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
      lstCalledAll := ReadCalledAll(fun, curNesting, maxNesting);
      if lstCalledAll.Count = 0 then begin
        lins.Add('    <none>');
      end else begin
        for called in lstCalledAll do begin
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
  lins.Add('Max. Nesting = ' + IntToSTr(TreeElems.maxNesting));

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
procedure SetCodSysInline(fun: TAstFunDec);
{Procedimiento que, por seguridad, debería ser el único acceso a TAstFunDec.codSysInline.
Así se garantiza que el "casting" se haga apropiadamente.}
begin
  fun.callType := ctSysInline;
end;
//////////////// Tipo Boolean /////////////
function TCompiler_PIC16.PICName: string;
begin
  Result := pic.Model;
end;

function TCompiler_PIC16.AddSIFtoUnit(name: string; sfi: TSysFunID; retType: TAstTypeDec;
         const srcPos: TSrcPos; const pars: TAstParamArray): TAstFunDec;
{Create a new system function in the current element of the Syntax Tree.
 Returns the reference to the function created.
   pars   -> Array of parameters for the function to be created.
   codSys -> SIF Routine or the the routine to generate de code.
}
var
   funimp: TAstFunImp;
   tmpLoc: TElemLocation;
begin
  tmpLoc := curLocation;     //Save current location. We are going to change it.
  {Note that we add declaration e implementation at the interface section. This is not
  the normal but the compiler works OK}
  curLocation := locInterface;
  Result := AddFunctionUNI(name, retType, srcPos, pars, false, true);
  Result.sfi := sfi;
  Result.callType := ctSysInline; //INLINE function
  //Here variables can be added
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  TreeElems.AddBodyAndOpen(SrcPos);  //Create body
  SetCodSysInline(Result);  //Set routine to generate code o SIF routine.
  TreeElems.CloseElement;  //Close body
  TreeElems.CloseElement;  //Close function implementation
  curLocation := tmpLoc;   //Restore current location
end;
function TCompiler_PIC16.AddSNFtoUnit(name: string; retType: TAstTypeDec; const srcPos: TSrcPos;
               var pars: TAstParamArray; codSys: TCodSysNormal): TAstFunDec;
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
   fundec: TAstFunDec;
   funimp: TAstFunImp;
   tmpLoc: TElemLocation;
   locvar: TAstVarDec;
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
                   typ0: TAstTypeDec; adicDec: TAdicDeclar);
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
  pars[n].adicVar.hasInit := nil;
  pars[n].isLocVar := false;
end;
function TCompiler_PIC16.CreateInUOMethod(
                      clsType: TAstTypeDec;   //Base type where the method bellow.
                      opr     : string;      //Opertaor associated to the method
                      name    : string;      //Name of the method
                      retType : TAstTypeDec;  //Type returned by the method.
                      operTyp: TOperatorType = opkUnaryPre): TAstFunDec;
{Create a new system function (associated to a unary operator) in the current element of
 the AST.
 Returns the reference to the function created.}
var
  pars: TAstParamArray;     //Array of parameters
begin
  setlength(pars, 0);        //Reset parameters
//  AddParam(pars, 'b', srcPosNull, clsType, decNone);  //Base object
  //Add declaration
  Result      := AddFunctionUNI(name, retType, srcPosNull, pars, false, true);
  //Here variables can be added
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  SetCodSysInline(Result); //Set routine to generate code
  Result.oper := UpCase(opr); //Set operator as UpperCase to speed searching.
  if opr = '' then Result.operTyp := opkNone
  else Result.operTyp := operTyp; //Must be pre or post
  TreeElems.CloseElement;    //Close function implementation
end;
function TCompiler_PIC16.CreateInBOMethod(
                      clsType: TAstTypeDec;   //Base type where the method bellow.
                      opr     : string;      //Opertaor associated to the method
                      name    : string;      //Name of the method
                      parType : TAstTypeDec;  //Parameter type
                      retType : TAstTypeDec  //Type returned by the method.
                      ): TAstFunDec;
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
  SetCodSysInline(Result); //Set routine to generate code
  Result.oper := UpCase(opr); //Set operator as UpperCase to speed search.
  if opr = '' then Result.operTyp := opkNone
  else Result.operTyp := opkBinary;
  TreeElems.CloseElement;  //Close body
  TreeElems.CloseElement;  //Close function implementation
end;
function TCompiler_PIC16.CreateInTerMethod(clsType: TAstTypeDec;
  name: string; parType1, parType2: TAstTypeDec; retType: TAstTypeDec
  ): TAstFunDec;
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
  SetCodSysInline(Result); //Set routine to generate code
  Result.operTyp := opkNone;   //Could be a ternary operator
  TreeElems.CloseElement;  //Close body
  TreeElems.CloseElement;  //Close function implementation
end;
procedure TCompiler_PIC16.DefineArray(etyp: TAstTypeDec);
var
  consDec: TAstConsDec;
  expr: TAstExpress;
  f, f1, f2: TAstFunDec;
begin
  //Create assigement method
  f := CreateInBOMethod(etyp, ':=', '_set', etyp, AstTree.typNull);
  f.asgMode := asgSimple;
  //Create attribute "low" as constant.
  AddConstDeclarByte('low', 0);
  //Create methods
//  CreateUOMethod(etyp, '', 'length', typByte, @arrayLength);
  CreateInUOMethod(etyp, '', 'high'  , typByte, opkNone);
  CreateInUOMethod(etyp, '', 'clear' , AstTree.typNull, opkNone);
//  CreateInBOMethod(etyp, '', 'fill' , typByte, typNull, @SIF_ArrayFill);
end;
procedure TCompiler_PIC16.DefinePointer(etyp: TAstTypeDec);
{Set operations that defines pointers aritmethic.}
var
  f, f1: TAstFunDec;
begin
  //Asignación desde word y Puntero
  f := CreateInBOMethod(etyp, ':=', '_set', typWord, AstTree.typNull);
  f.asgMode := asgSimple;
  f := CreateInBOMethod(etyp, ':=', '_set', etyp, AstTree.typNull);
  f.asgMode := asgSimple;

  CreateInBOMethod(etyp, '=',  '_equ',  typWord, typBool);
  CreateInBOMethod(etyp, '=',  '_equ',  etyp   , typBool);

  CreateInBOMethod(etyp, '+',  '_add',  typWord, etyp   );
  CreateInBOMethod(etyp, '+',  '_add',  typByte, etyp   );

  CreateInBOMethod(etyp, '-',  '_sub',  typWord, etyp   );
  CreateInBOMethod(etyp, '-',  '_sub',  typByte, etyp   );
  CreateInBOMethod(etyp, '>' , '_gre',  etyp   , typBool);
  CreateInBOMethod(etyp, '<' , '_les',  etyp   , typBool);
  CreateInBOMethod(etyp, '>=', '_gequ', etyp   , typBool);
  CreateInBOMethod(etyp, '<=', '_lequ', etyp   , typBool);

  f := CreateInBOMethod(etyp, '+=', '_aadd', typWord, AstTree.typNull);
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
  f := CreateInBOMethod(etyp, '+=', '_aadd', typByte, AstTree.typNull);
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
//  etyp.CreateUnaryPostOperator('^',6, 'deref', @SIF_derefPointer);  //dereferencia
end;
procedure TCompiler_PIC16.DefineObject(etyp: TAstTypeDec);
var
  consDec: TAstConsDec;
  expr: TAstExpress;
  f, f1, f2: TAstFunDec;
begin
//  //Create assigement method
//  f := CreateInBOMethod(etyp, ':=', '_set', etyp, typNull, @SIF_obj_asig_obj);
//  f.asgMode := asgSimple;
//  f.getset := gsSetInSimple;
end;
//Creació de la unidad System
procedure TCompiler_PIC16.CreateSystemTypesAndVars;
begin
  /////////////// System types ////////////////////
  typBool := CreateEleTypeDec('boolean', srcPosNull, 1, tctAtomic, t_boolean);
  typBool.location := locInterface;   //Location for type (Interface/Implementation/...)
  TreeElems.AddElementAndOpen(typBool);  //Open to create "elements" list.
  TreeElems.CloseElement;   //Close Type
  typByte := CreateEleTypeDec('byte', srcPosNull, 1, tctAtomic, t_uinteger);
  typByte.location := locInterface;
  TreeElems.AddElementAndOpen(typByte);  //Open to create "elements" list.
  TreeElems.CloseElement;

  typChar := CreateEleTypeDec('char', srcPosNull, 1, tctAtomic, t_string);
  typChar.location := locInterface;
  TreeElems.AddElementAndOpen(typChar);
  TreeElems.CloseElement;

  typWord := CreateEleTypeDec('word', srcPosNull, 2, tctAtomic, t_uinteger);
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

  // ****  This section is hardware dependent ****
  {Create variables for aditional Working register. These variables are accesible
  (and usable) from the code, because the name assigned is a common variable.}
  //Create register H as variable
  H := AddVarDecAndOpen('__H', typByte, srcPosNull);
  TreeElems.CloseElement;  { TODO : ¿No sería mejor evitar abrir el elemento para no tener que cerrarlo? }
  H.adicPar.hasAdic := decNone;
  H.adicPar.hasInit := nil;
  H.location := locInterface;  //make visible
  //Create register E as variable
  E := AddVarDecAndOpen('__E', typByte, srcPosNull);
  TreeElems.CloseElement;
  E.adicPar.hasAdic := decNone;
  E.adicPar.hasInit := nil;
  E.location := locInterface;  //make visible
  //Create register U as variable
  U := AddVarDecAndOpen('__U', typByte, srcPosNull);
  TreeElems.CloseElement;
  U.adicPar.hasAdic := decNone;
  U.adicPar.hasInit := nil;
  U.location := locInterface;  //make visible
  //Create register IX as variable
  IX := AddVarDecAndOpen('__IX', typWord, srcPosNull);
  TreeElems.CloseElement;
  IX.adicPar.hasAdic := decNone;
  IX.adicPar.hasInit := nil;
  IX.location := locInterface;  //make visible
end;
procedure TCompiler_PIC16.CreateBooleanOperations;
var
  f: TAstFunDec;
begin
  /////////////// Boolean type ////////////////////
  //Methods-Operators
  TreeElems.OpenElement(typBool);
  f:=CreateInBOMethod(typBool, ':=',  '_set', typBool, AstTree.typNull);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
  f:=CreateInUOMethod(typBool, 'NOT', '_not', typBool, opkUnaryPre);
  f:=CreateInBOMethod(typBool, 'AND', '_and', typBool, typBool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typBool, 'OR' , '_or' , typBool, typBool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typBool, 'XOR', '_xor', typBool, typBool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typBool, '=' ,  '_equ', typBool, typBool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typBool, '<>',  '_dif', typBool, typBool);
  f.fConmutat := true;
  TreeElems.CloseElement;   //Close Type
end;
procedure TCompiler_PIC16.CreateByteOperations;
var
  f, f1, f2: TAstFunDec;
begin
  //Methods-Operators
  TreeElems.OpenElement(typByte);
  //Simple Assignment
  f:=CreateInBOMethod(typByte, ':=', '_set', typByte, AstTree.typNull);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
  //Array-pinter Assignment
//  f1 := CreateInBOMethod(typByte, '', '_getitem', typByte, typByte);
//  f1.getset := gsGetInItem;
//  f2 := CreateInBOMethod(typByte, '', '_getitem', typWord, typByte);
//  f2.getset := gsGetInItem;
//  //AddCallerToFrom(IX, f.bodyNode);  //Dependency
//  f := CreateInTerMethod(typByte, '_setitem', typByte, typByte, AstTree.typNull);
//  f.asgMode := asgSimple;
//  f.getset := gsSetInItem;
//  f1.funset := f;         //Connect to getter
//  f := CreateInTerMethod(typByte, '_setitem', typWord, typByte, AstTree.typNull);
//  f.asgMode := asgSimple;
//  f.getset := gsSetInItem;
//  f2.funset := f;         //Connect to getter

  //Assignment with operations
  f:=CreateInBOMethod(typByte, '+=', '_aadd',typByte, AstTree.typNull);
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typByte, '-=', '_asub',typByte, AstTree.typNull);
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typByte, '+' , '_add', typByte, typByte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, '+' , '_add', typWord, typWord);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, '-' , '_sub', typByte, typByte);
  f:=CreateInBOMethod(typByte, '*' , '_mul', typByte, typWord);
  f.fConmutat := true;
  sifByteMulByte := f;
  f:=CreateInBOMethod(typByte, 'DIV' , '_div', typByte, typByte);
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  sifByteDivByte := f;
  f:=CreateInBOMethod(typByte, 'MOD' , '_mod', typByte, typByte);
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  sifByteModByte := f;

  f:=CreateInBOMethod(typByte, 'AND','_and', typByte, typByte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, 'OR' ,'_or' , typByte, typByte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, 'XOR','_xor', typByte, typByte);
  f.fConmutat := true;
  f:=CreateInUOMethod(typByte, 'NOT','_not', typByte, opkUnaryPre);
  f:=CreateInBOMethod(typByte, '=' , '_equ', typByte, typBool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, '<>', '_dif', typByte, typBool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, '>' , '_gre', typByte, typBool);
  f:=CreateInBOMethod(typByte, '<' , '_les', typByte, typBool);
  f:=CreateInBOMethod(typByte, '>=', '_gequ',typByte, typBool);
  f:=CreateInBOMethod(typByte, '<=', '_lequ',typByte, typBool);
  f:=CreateInBOMethod(typByte, '>>', '_shr', typByte, typByte);  { TODO : Definir bien la precedencia }
  f:=CreateInBOMethod(typByte, '<<', '_shl', typByte, typByte);
  TreeElems.CloseElement;   //Close Type
end;
procedure TCompiler_PIC16.CreateCharOperations;
var
  f: TAstFunDec;
begin
  /////////////// Char type ////////////////////
  TreeElems.OpenElement(typChar);
  f:=CreateInBOMethod(typChar, ':=', '_set', typChar, AstTree.typNull);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
  //opr.CreateOperation(typString, @SIF_char_asig_string);
  f:=CreateInBOMethod(typChar, '=' , '_equ', typChar, typBool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typChar, '<>', '_dif', typChar, typBool);
  f.fConmutat := true;
  TreeElems.CloseElement;   //Close Type
end;
procedure TCompiler_PIC16.CreateWordOperations;
var
  f: TAstFunDec;
begin
  /////////////// Word type ////////////////////
  TreeElems.OpenElement(typWord);
  f:=CreateInBOMethod(typWord, ':=' ,'_set' , typWord, AstTree.typNull);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  f:=CreateInBOMethod(typWord, ':=' ,'_set' , typByte, AstTree.typNull);
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typWord, '+=' ,'_aadd', typByte, AstTree.typNull);
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typWord, '+=' ,'_aadd', typWord, AstTree.typNull);
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typWord, '-=' ,'_asub', typByte, AstTree.typNull);
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typWord, '-=' ,'_asub', typWord, AstTree.typNull);
  AddCallerToFrom(E, f.bodyNode);  // Require _E
  f.asgMode := asgOperat;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typWord, '+'  , '_add', typByte, typWord);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, '+'  , '_add', typWord, typWord);
  f.fConmutat := true;
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  f:=CreateInBOMethod(typWord, '-'  , '_sub', typByte, typWord);
  f:=CreateInBOMethod(typWord, '-'  , '_sub', typWord, typWord);
  f:=CreateInBOMethod(typWord, '*' , '_mul', typByte, typWord);
  f.fConmutat := true;

  f:=CreateInBOMethod(typWord, 'DIV' , '_div', typWord, typWord);
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  sifWordDivWord := f;
  f:=CreateInBOMethod(typWord, 'MOD' , '_mod', typWord, typWord);
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  sifWordModWord := f;

  f:=CreateInBOMethod(typWord, 'AND', '_and', typByte, typByte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, 'AND', '_and', typWord, typWord);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, 'OR' , '_or' , typWord, typWord);
  f.fConmutat := true;
  f:=CreateInUOMethod(typWord, 'NOT', '_not', typWord, opkUnaryPre);
  f:=CreateInBOMethod(typWord, '>>' , '_shr', typByte, typWord); { TODO : Definir bien la precedencia }
  f:=CreateInBOMethod(typWord, '<<' , '_shl', typByte, typWord);
  sifWordShlByte := f;         //Guarda referencia

  f:=CreateInBOMethod(typWord, '=' , '_equ' , typWord, typBool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, '=' , '_equ' , typByte, typBool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, '<>', '_dif' , typWord, typBool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, '>=', '_gequ', typWord, typBool);
  AddCallerToFrom(E, f.bodyNode);  //Dependency
  f:=CreateInBOMethod(typWord, '<' , '_les' , typWord, typBool);
  f:=CreateInBOMethod(typWord, '>' , '_gre' , typWord, typBool);
  f:=CreateInBOMethod(typWord, '<=', '_lequ', typWord, typBool);
  //Methods
  f:=CreateInUOMethod(typWord, '', 'low' , typByte);
  f:=CreateInUOMethod(typWord, '', 'high', typByte);

  TreeElems.CloseElement;   //Close Type
end;
procedure TCompiler_PIC16.CreateDWordOperations;
var
  f: TAstFunDec;
begin
  /////////////// DWord type ////////////////////
  TreeElems.OpenElement(typDWord);
  f:=CreateInBOMethod(typDWord, ':=' ,'_set' , typDWord, AstTree.typNull);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typDWord, ':=' ,'_set' , typByte, AstTree.typNull);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typDWord, ':=' ,'_set' , typWord, AstTree.typNull);
  f.asgMode := asgSimple;
  f.getset := gsSetInSimple;
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  f:=CreateInBOMethod(typDWord, '+=' ,'_aadd', typByte, AstTree.typNull);
//  f.getset := gsSetOther;
  f:=CreateInBOMethod(typDWord, '+=' ,'_aadd', typDWord, AstTree.typNull);
//  f.getset := gsSetOther;
//  f:=CreateInBOMethod(typDWord, '-=' ,'_asub', typByte, typNull, @SIF_word_asub_byte);
//  f.getset := gsSetOther;
//  f:=CreateInBOMethod(typDWord, '-=' ,'_asub', typDWord, typNull, @SIF_word_asub_word);
//  AddCallerToFrom(E, f.bodyNode);  // Require _E
//  f.getset := gsSetOther;
//  f:=CreateInBOMethod(typDWord, '+'  , '_add', typByte, typDWord, @SIF_word_add_byte);
//  f.fConmutat := true;
  f:=CreateInBOMethod(typDWord, '+'  , '_add', typDWord, typDWord);
  f.fConmutat := true;
  f:=CreateInBOMethod(typDWord, '+'  , '_add', typByte, typDWord);
  f.fConmutat := true;
  f:=CreateInBOMethod(typDWord, '+'  , '_add', typWord, typDWord);
  f.fConmutat := true;
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  f:=CreateInBOMethod(typDWord, '-'  , '_sub', typByte, typDWord);
  f:=CreateInBOMethod(typDWord, '-'  , '_sub', typDWord, typDWord);
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
  f:=CreateInBOMethod(typDWord, '=' , '_equ' , typDWord, typBool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typDWord, '=' , '_equ' , typWord, typBool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typDWord, '=' , '_equ' , typByte, typBool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typDWord, '<>', '_dif' , typDWord, typBool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typDWord, '>=', '_gequ', typDWord, typBool);
//  AddCallerToFrom(E, f.bodyNode);  //Dependency
  f:=CreateInBOMethod(typDWord, '<' , '_les' , typDWord, typBool);
  f:=CreateInBOMethod(typDWord, '>' , '_gre' , typDWord, typBool);
  f:=CreateInBOMethod(typDWord, '<=', '_lequ', typDWord, typBool);
//  //Methods
//  f:=CreateInUOMethod(typDWord, '', 'low' , typByte, @word_Low);
//  f:=CreateInUOMethod(typDWord, '', 'high', typByte, @word_High);

  TreeElems.CloseElement;   //Close Type
end;
procedure TCompiler_PIC16.CreateSystemUnitInAST;
{Initialize the system elements. Must be executed just one time when compiling.}
var
  uni: TAstUnit;
  pars: TAstParamArray;  //Array of parameters
  pars1null: TAstParamArray;  //Array of parameters with one Null parameter
  f, sifDelayMs, sifWord: TAstFunDec;
begin
  //////// Funciones del sistema ////////////
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
  TreeElems.AddElementAndOpen(uni);  //Open Unit
  CreateSystemTypesAndVars;
  curLocation := locInterface;   {Maybe not needed because element here are created directly.}
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
  AddSIFtoUnit('exit', SFI_EXIT0, AstTree.typNull, srcPosNull, pars);  //Versión sin parámetros
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
  AddCallerToFrom(H, sifWord.BodyNode);  //Require H
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
  TreeElems.CloseElement;
end;

procedure TCompiler_PIC16.cbSetGeneralORG(value: integer);
begin
   GeneralORG := value
end;
procedure TCompiler_PIC16.cbSetCpuMode(value: string);
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
procedure TCompiler_PIC16.cbSetFrequency(f: Longint; value: string);
begin
  case UpperCase(value) of
  'KHZ': f := f * 1000;
  'MHZ': f := f * 1000000;
  else
    GenErrorDir('Error in directive.');
    exit;
  end;
  if f>picCore.MaxFreq then begin
    GenErrorDir('Frequency too high for this device.');
    exit;
  end;
  picCore.frequen:=f; //asigna frecuencia
end;
procedure TCompiler_PIC16.cbSetStatRAMCom(value: string);
begin
  picCore.SetStatRAMCom(value);
  if picCore.MsjError<>'' then GenErrorDir(picCore.MsjError);
end;
procedure TCompiler_PIC16.cbSetDataAddr(value: string);
begin
  picCore.SetDataAddr(value);
  if picCore.MsjError<>'' then GenErrorDir(picCore.MsjError);
end;
procedure TCompiler_PIC16.cbClearStateRam;
begin
  picCore.DisableAllRAM;
end;
function TCompiler_PIC16.cbReadPicModel(): string;
begin
  Result := picCore.Model;
end;
procedure TCompiler_PIC16.cbSetPicModel(value: string);
begin
  picCore.Model := value;
end;
function TCompiler_PIC16.cbReadFrequen(): Single;
begin
  Result := picCore.frequen;
end;
procedure TCompiler_PIC16.cbSetFrequen(value: single);
begin
  picCore.frequen := round(Value);
end;
function TCompiler_PIC16.cbReadMaxFreq(): Single;
begin
  Result := PICCore.MaxFreq;
end;
procedure TCompiler_PIC16.cbSetMaxFreq(value: single);
begin
  PICCore.MaxFreq := round(Value);
end;
function TCompiler_PIC16.cbReadORG(): Integer;
begin
  Result := picCore.iRam;
end;
procedure TCompiler_PIC16.cbSetORG(value: integer);
begin
  picCore.iRam := value;
end;
procedure TCompiler_PIC16.Cod_StartProgram;
//Codifica la parte inicial del programa
begin
  //Code('.CODE');   //inicia la sección de código
end;
procedure TCompiler_PIC16.Cod_EndProgram;
//Codifica la parte final del programa
begin
  //Code('END');   //inicia la sección de código
end;

constructor TCompiler_PIC16.Create;
begin
  inherited Create;
  //OnNewLine:=@cInNewLine;
  syntaxMode := modPicPas;   //Por defecto en sintaxis nueva

  ID := 16;  //Identifica al compilador PIC16
  //Conecta el parser de directivas al Generador de Cödigo
  OnSetGeneralORG:= @cbSetGeneralORG;
  OnSetCpuMode   := @cbSetCpuMode;
  OnSetFrequency := @cbSetFrequency;
  OnSetStatRAMCom:= @cbSetStatRAMCom;
  OnSetDataAddr  := @cbSetDataAddr;
  OnClearStateRam:= @cbClearStateRam;
  OnReadPicModel := @cbReadPicModel;
  OnSetPicModel  := @cbSetPicModel;
  OnReadFrequen  := @cbReadFrequen;
  OnSetFrequen   := @cbSetFrequen;
  OnReadMaxFreq  := @cbReadMaxFreq;
  OnSetMaxFreq   := @cbSetMaxFreq;
  OnReadORG      := @cbReadORG;
  OnSetORG       := @cbSetORG;
end;
destructor TCompiler_PIC16.Destroy;
begin
  inherited Destroy;
end;

end.
//1457
