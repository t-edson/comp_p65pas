{
XpresElemP65
============
By Tito Hinostroza.

Basic definitions for syntax elements used in the Abstract Syntax Tree.
}
unit AstElemP65;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, TypInfo, LexPas, CompGlobals, LazLogger, StrUtils;
const
  ADRR_ERROR = $FFFF;
const //Prefixes used to name anonym type declarations
  //Short string are used to don't affect the searching speed
  PREFIX_ARR = '_ar';
  PREFIX_PTR = '_pt';
  PREFIX_OBJ = '_ob';
  //Name for elements
  TIT_BODY_ELE = 'Body';
type  //Previous definitions for elements
  TVarOffs = word;

  TAstElement = class;
  TAstElements = specialize TFPGObjectList<TAstElement>;

  { TAstEleCaller }
  //Information about the call to one element from other element.
  TAstEleCaller = class
    curPos: TSrcPos;    //Position from where it is called this element.
    caller: TAstElement; //Element that calls this element (Function body or variable).
    function CallerUnit: TAstElement;  //Unit/Program from where it is called this element.
  end;
  TAstListCallers = specialize TFPGObjectList<TAstEleCaller>;

  //Datos de las llamadas que se hacen a otro elemento
//  TAstEleCalled = class
//    curPos: TSrcPos;    //Posición desde donde es llamado
//    curBnk: byte;       //banco RAM, desde donde se llama
//    called: TAstElement; //función que llama a esta función
//  end;
  TAstListCalled = specialize TFPGObjectList<TAstElement>;

type  //TAstElement class
  //Element types for the language.
  TAstIDClass = (//Declaraction
                eleNone,      //No type
                eleVarDec,    //Variable declaration
                eleConsDec,   //Constant declaration
                eleTypeDec,   //Type declaration
                eleFuncDec,   //Function declaration
                eleFuncImp,   //Function implementation
                //Structural
                eleBody,      //Body procedure/program
                eleBlock,     //Block of code
                eleProgFrame, //Code container
                eleProg,      //Main program
                eleUnit,      //Unit
                eleFinal,     //FINALIZATION section
                //Expressions
                eleExpress,   //Expression
                //Instructions relative
                eleSenten,    //Sentence/Instruction
                eleAsmInstr,  //ASM instruction
                eleAsmBlock   //ASM block
                );
  TAstCodeCont = class;
  { TAstElement }
  //Base class for all syntactic elements
  TAstElement = class
  private
    Fname    : string;   //Element name
    Funame   : string;   //Upper case name. Used to acelerate searchings.
    procedure Setname(AValue: string);
  public  //Callers management
    //List of functions that calls to this function.
    lstCallers: TAstListCallers;
    function nCalled: integer; virtual; //número de llamadas
    function IsCalledBy(callElem: TAstElement): boolean; //Identifica a un llamador
    function IsCalledByChildOf(callElem: TAstElement): boolean; //Identifica a un llamador
    function IsCalledAt(callPos: TSrcPos): boolean;
    function IsDeclaredAt(decPos: TSrcPos): boolean;
    function FindCalling(callElem: TAstElement): TAstEleCaller; //Identifica a un llamada
    function RemoveCallsFrom(callElem: TAstElement): integer; //Elimina llamadas
    procedure RemoveLastCaller; //Elimina la última llamada
    procedure ClearCallers;  //limpia lista de llamantes
//    function ExistsIn(list: TAstElements): boolean;
  public  //Gestión de los elementos llamados
    curNesting: Integer;   //Nested level for calls
    maxNesting: Integer;   //Max nested level for calls
    //Lista de funciones que son llamadas directamente (Se llena en el enlazado)
    lstCalled : TAstListCalled;
    //Lista de funciones que son llamadas dirceta o indirectamente (Se llena en el enlazado)
    lstCalledAll: TAstListCalled;
    //Métodos para el llemado
    procedure AddCalled(elem: TAstElement);
    function UpdateCalledAll: integer;
  public
    Parent  : TAstElement;  //Reference to parent element
    idClass : TAstIDClass;  //To avoid use RTTI
    elements: TAstElements; //Container list for other elements
    location: TElemLocation;  //Element location
    codCont : TAstCodeCont;  //Temporal field for Code container.
    property name: string read Fname write Setname;
    property uname: string read Funame;
    function Path: string;
    function FindElemName(const eName: string; out ele: TAstElement): boolean;
    function FindIdxElemName(const eName: string; var idx0: integer): boolean;
    function LastNode: TAstElement; inline;
    function Index: integer;
    function AddElement(elem: TAstElement; position: integer = - 1): TAstElement;
  public  //Location in the source code.
    //Where the element is declared.
    srcDec: TSrcPos;
    {Ending location of the element. Useful in elements TAstBody, to limit the
    block of code.}
    srcEnd: TSrcPos;
    function posXYin(const posXY: TPoint): boolean;
  public  //Inicialización
    procedure Clear; virtual;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  {Base class to derivate: Body, Blocks, Or a Declaration  }
  TAstCodeCont = class(TAstElement)
  end;


const
  CONS_ITEM_BLOCK = 20;

type
  //Groups of data types.
  TTypeGroup=(
    t_integer,  //Signed integer numbers
    t_uinteger, //Unsigned integer numbers
    t_float,    //Float numbers
    t_boolean,  //Booleans
    t_string,   //String of chars
    t_enum  ,   //Enumerated. { TODO : Check if needed }
    t_object    //Object (contain fields)
  );
  { TConsValue }
  {Structure to store all the possible values for a constant.
  Must have fields for all basic types defined in "TTypeGroup" and for composed
  values}
  TConsValue = object
  public
    ValInt  : Int64;    //For values t_integer y t_uinteger
    ValFloat: extended; //For values t_float
    ValBool : boolean;  //For values t_boolean
    ValStr  : string;   //For values t_string
  public //Arrays   *** Deberíamn eliminarse estas propied. porque en el AST, se deben crear como ramas.
    items   : array of TConsValue;  //Ítems list
    nItems  : integer;  //Number of items
    curSize : integer;
    procedure InitItems;
    procedure AddConsItem(const c: TConsValue);
    procedure CloseItems;
  public  //Access to ValInt
    function LByte: byte; inline;  //Returns low byte of integer value.
    function HByte: byte; inline;  //Returns high byte of integer value.
    function EByte: byte; inline;
    function UByte: byte; inline;
    function valuesAsString: string;
  end;

  TProcDefineVar = procedure(const varName, varInitVal: string) of object;

  TAstExpress = class;

  TAstVarDec = class;

  TAstConsDec = class;

  {Description for aditional information in variables declaration: ABSOLUTE ,
  REGISTER,  or initialization. }
  TAdicVarDec = record
    //Absolute or register information.
    hasAdic  : TAdicDeclar;   //Flag. Indicates when variable is register or absolute.
//    absVar   : TAstVarDec;    //Reference to variable, when is ABSOLUTE <variable>
    absAddr  : TAstExpress;   {Reference to the AST expression that returns the absolute
                              address where the variable should be located. Only valid
                              when: hasAdic = decAbsol.}
    //Initialization information.
    hasInit  : TAstExpress;   {Reference and Flag. When is not NIL, refers to the
                              expression in the AST where is the initial value.
                              Initial expression must be a child node.}
    //*** También puede dejarse "hasInit" como boolean y crear otro campo "initVal".
    {Although the "absolute address" and the "initial value" can be obtained from the
    children nodes of the variable declaration, the quantity of nodes (1 or 2) and the
    value of the first node (first can be "absolute address" or "initial value"), are not
    fixed. That's why we have references to these nodes (absAddr and hasInit).}
  end;
type  //Declaration elements
  TAstFunImp = class;


  TAstTypeDec= class;
  TAstTypeDecs= specialize TFPGObjectList<TAstTypeDec>; //lista de variables

  { TAstTypeDec }
  {Clase para modelar a los tipos definidos por el usuario y a los tipos del sistema.
  Es una clase relativamente extensa, debido a la flxibilidad que ofrecen los tipos en
  Pascal.}
  TAstTypeDec = class(TAstCodeCont)
  private
    fSize: word;
    internalTypes: TAstTypeDecs;  //Container for types recursively defined.
    function getSize: word;
    procedure setSize(AValue: word);
  public   //Events
    {Estos eventos NO se generan automáticamente en TCompilerBase, sino que es la
    implementación del tipo, la que deberá llamarlos. Son como una ayuda para facilitar
    la implementación.}
    {*** ¿Puede un tipo estar asociado a una rutina SIF? Ceo que estos callbacks no deben ir aquí.}
    OnLoadToWR  : TMethod;  {//Used when required to load an operand in Work Register.
                            Formalmente debería ser TProcLoadOperand, pero se pone como
                            TMethod porque la declaración de TProcLoadOperand implica al
                            tipo "TMirOperand" que será definido después.
                            En su uso se deberá manejar como TProcLoadOperand (haciendo
                            "casting"). Para evitar problemas en el "casting", se
                            recomienda acceder a este campo en un solo procedimiento que
                            haga el casting.}
  public   //Identification
    copyOf  : TAstTypeDec;  //Indicates this type is copy of other
    group   : TTypeGroup;   //Type group (numéric, string, etc)
    catType : TCatType;   //Categoría del tipo
    property size: word read getSize write setSize;   //Tamaño en bytes del tipo
    function groupStr: string;
    function catTypeStr: string;
  public   //Fields when type is Array or pointer
    consNitm: TAstConsDec;  //Reference to constant defining the number of items.
    itmType : TAstTypeDec;  {Reference to the item type when it's array.
                                TArr = array[255] of byte;  //itemType = byte
                            }
    isDynam : boolean;      //Indicates the size is dynamic. No current supported except when initialized.
    ptrType : TAstTypeDec;  {Reference to the type pointed, when it's pointer.
                                TPtr = ^integer;       //ptrType = integer
                           }
    function nItems: integer;  //Number of items, when is tctArray (-1 if it's dynamic.)
  public   //Fields when type is Object
    objSize : integer;
  public   //Information
    tmpNode: TAstElement;  //Temporal node informatios. Used by OpenTypeDec().
    function IsByteSize: boolean;
    function IsWordSize: boolean;
    function IsDWordSize: boolean;
    function IsArrayOf(itTyp: TAstTypeDec; numIt: integer): boolean;
    function IsPointerTo(ptTyp: TAstTypeDec): boolean;
    function IsEquivalent(typ: TAstTypeDec): boolean;
  public
    //Este campo debería usarse para acceder al elementeo en el MIR
    mirTypDec: TObject;  //Formalmente debe ser TMirTypDec, pero se pone TObject para no generar referencias circulares.
  public  //Initialization
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TAstCodeCont }
  //Class to modelate constants declaration.
  TAstConsDec = class(TAstCodeCont)
    //Element type
    typ      : TAstTypeDec;
    {Flag to indicate if the constant value, stored in "value" field, is valid.
    If evaluated = true  -> The constant value can be read in "value".
    If evaluated = false -> The constant is not yet evaluated. It has been defined as an
                            expression, not yet evaluated.}
    evaluated: boolean;
    //Constant value
    value    : ^TConsValue;
    //Este campo debería usarse para acceder al elementeo en el MIR
    mirConDec: TObject;  //Formalmente debe ser TMirConDec, pero se pone TObject para no generar referencias circulares.
    constructor Create; override;
  end;
  TAstConsDecs = specialize TFPGObjectList<TAstConsDec>; //lista de constantes

  //Operand value storage. Hardware dependent.
  TStorage = (
    stNone     = %0000,  //Without storage.
    stConst    = %0001,  //Constant value.
    stRamFix   = %0010,  //In RAM. Address constant and defined.
    //stRamVar = %0100,  //In RAM. Address obtained reading a variable.
    stRamVarOf = %0011,  //In RAM. Address obtained reading a variable and offset.
    stRamReg   = %0100,  //In RAM. Address allocated in Work Register.

    stRegister = %1000,  //In default registers (Work register).
    //Only valid for byte-size types
    stRegistA  = %1001,  //In register A. Only for byte-size variables.
    stRegistX  = %1010,  //In register X. Only for byte-size variables.
    stRegistY  = %1011   //In register Y. Only for byte-size variables.
  );
  {Mixed storage used to implement INLINE binary operands code. This is created to
  represent two TStorage values in a simple byte constant (joining bits), in order to
  facilitate the use in a CASE ... OF structure.}
  TStoOperandsBSIF =(
    stConst_Const    = %00010001,
    stConst_RamFix   = %00010010,
    stConst_Regist   = %00011000,

    stRamFix_Const   = %00100001,
    stRamFix_RamFix  = %00100010,
    stRamFix_Regist  = %00101000,

    stRegist_Const   = %10000001,
    stRegist_RamFix  = %10000010,
    stRegist_Regist  = %10001000
  );

  //Operand type
  TopType = (
    otVariab,  {Operand is variable. Allows read/write.
               Support storages:
                - stRamFix
                - stRamReg
                - stRamVar
                - stRegister
                - stRegistA
                - stRegistX
                - stRegistY
               }
    otConst,   {Operand is constant. Only for read.
               Support storages:
                - stConst
                - stRamFix   //Special case
               }
    otFunct    {Operand is a function/method or expression result. Only for read.
               Support storages:
                - stRegister
                - stRegistA
                - stRegistX
                - stRegistY
               }
  );

  { TAstVarDec }
  //Class to modelate variable declarations.
  TAstVarDec = class(TAstCodeCont)
  private
    ftyp: TAstTypeDec;
    function Gettyp: TAstTypeDec;
    procedure Settyp(AValue: TAstTypeDec);
  public   //Manejo de parámetros adicionales
    adicPar: TAdicVarDec;  //Parámetros adicionales en la declaración de la variable.
  public
    //Flag to validate if varriable is used as parameter.
    IsParameter: boolean;
    {Indica si la variable es temporal, es decir que se ha creado solo para acceder a
    una parte de otra variable, que si tiene almacenamiento físico.}
    IsTmp      : boolean;
    //IsReg      : boolean;  //Flag to indicate the variable is used as register.
    required   : boolean;    {Indicates the variable is required to be allocated. Work
                              for variables used as registers. }
    //Reference to Type element
    property typ: TAstTypeDec read Gettyp write Settyp;
  public  //Campos para guardar las direcciones físicas asignadas en RAM.
    allocated: boolean;   //Activated when variable is allocated (RAM or register).
    storage  : TStorage;  //Depend on adicPar.hasAdic.
    addr     : word;      //Base address.
  public
    //Este campo debería usarse para acceder al elementeo en el MIR
    mirVarDec: TObject;  //Formalmente debe ser TMirVarDec, pero se pone TObject para no generar referencias circulares.
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
  TAstVarDecs = specialize TFPGObjectList<TAstVarDec>;

type  //Expression elements
  TAstFunBase = class;
  TAstFunDec = class;

  //Constant types
  TConsType = (
    ctLiteral,  //Literal like $1234 or 123
    ctConsRef,  //Reference to a constant declared like "CONST1"
    ctVarAddr,  //Constant expression like addr(<variable>)
    ctFunAddr   //Constant expression like addr(<function>)
  );
  { TAstExpress }
  {Represents an expression/operand. }
  TAstExpress = class(TAstElement)
  public
    opType  : TopType;      //Operand type: otVariab, otConst, otFunct.
    Sto     : TStorage;     //Storage of the value (memory, register, value)
    Typ     : TAstTypeDec;  //Data type for the operand.
    function opTypeAsStr: string; //"opType" as string
    function StoAsStr: string;  //Storage as string
    procedure StringToArrayOfChar(str: string);
    function ValueIsZero: boolean;
  public  //Fields used when opType is otFunct.
    fundec  : TAstFunDec;  //Reference to function declaration
    {When element is "otFunct", this flag indicates the function/method has been
    called using an operator instead of call the function by its name.}
    fcallOp : boolean;
    function IsConstantPlusVariable: boolean;
    function IsVariablePlusConstant: boolean;
    procedure exchange2Children;
  public  //Fields used when "Sto" is stConst
    evaluated: boolean;     //Activated when constant is evaluated.
    consType : TConsType;   //Constant type
    //Fields used according to "consType" value.
    value    : TConsValue;  //Constant value, when consType=ctLiteral
    consRef  : TAstConsDec;  //Ref. to TAstConsDec when consType=ctConsRef
    addrVar  : TAstVarDec;   //Ref. to TAstVarDec  when consType=ctVarAddr
    addrFun  : TAstFunDec;   //Ref. to TAstFun when consType=ctFunAddr
    public
    //Functions to read values.
    function val: dword;
    function valL: word;
    function valH: word;
    function valU: word;
    function valE: word;
    function valWlo: word;
    function valWhi: word;
    procedure SetLiteralBoolConst(valBool: Boolean);
    procedure SetLiteraltIntConst(valInt: Int64);
    procedure SetConstRef(cons0: TAstConsDec);
    procedure SetAddrVar(var0: TAstVarDec);
    procedure SetAddrFun(fun0: TAstFunDec);
  public //Fields used when opType is otVariab.
//    offs   : integer;     //Offset to address when storage is stRamVarOf. *** Only Temporal because this field will pass to MIR.
    function IsCAvar: Boolean;  //Is Constant-Addressed variable.
    function IsCVAvar: Boolean; //Is Constant-Variable-addressed variable.
    function vardec: TAstVarDec;  //Reference to Variable declaration.
  public  //Initialization
    constructor Create; override;
    destructor Destroy; override;
  end;

type  //Structural elements

  { TExitCall }
//  //Clase que representa una llamada a la instrucción exit()
//  TExitCall = class
//    srcPos : TSrcPos;    //Posición en el código fuente
//    codeBlk: TAstCodeCont; {Must refer to a:
//                             - Body of a function/program.
//                             - The block section of a sentence that can contain block of
//                               code like: IF, FOR, WHILE, REPEAT.
//                             Other cases are not yet analyzed if are valid.  }
//    function IsObligat: boolean;
//  end;
//  TExitCalls = specialize TFPGObjectList<TExitCall>; //lista de variables

  { TAstBody }
  //Class to modelate the body of the main program or the procedures.
  TAstBody = class(TAstCodeCont)
    adrr   : integer;  //Physical address
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TAstBlock }
  //Class to modelate a block of code, like a BEGIN...END or the body of conditional.
  TAstBlock = class(TAstCodeCont)
    //adrr   : integer;  //dirección física
    constructor Create; override;
    destructor Destroy; override;
  end;

  TAstSentence = class;
  { TAstProgFrame }
  {Defines an element that have a strucure similar to a general Pascal program,
  including declaractions (VAR, CONST, PROCEDURE) and a Code container (BODY).
  be used as a general code container, like the main program,
  a procedure or a unit.}
  TAstProgFrame = class(TAstElement)
  public
    function BodyNode: TAstBody;
  public //Manejo de llamadas a exit()
    firstObligExit: TAstExpress;  {Referencia al primer exit(), en código obligatorio.
                           Si es NIL, significa que no hay ningún exit() en código
                           obligatorio, aunque podrían haber algunos en código condicional.
                           Solo importa el primero, porque después de este, ya no se
                           ejecutará ningún otro código.}
    procedure RegisterExitCall(exitSent: TAstExpress);
  public //Inicialización
    procedure Clear; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  //Clase para modelar al bloque principal
  { TAstProg }
  TAstProg = class(TAstProgFrame)
    //Como este nodo representa al programa principal, se incluye información física
    srcSize: integer;  {Tamaño del código compilado. En la primera pasada, es referencial,
                        porque el tamaño puede variar al reubicarse.}
    constructor Create; override;
  end;

  { TAstUnit }
  //Clase para modelar a las unidades
  TAstUnit = class(TAstProgFrame)
  public
    srcFile: string;   //El archivo en donde está físicamente la unidad.
    InterfaceElements: TAstElements;  //Lista de eleemntos en la sección INTERFACE
    procedure ReadInterfaceElements;
    constructor Create; override;
    destructor Destroy; override;
  end;
  TAstUnits = specialize TFPGObjectList<TAstUnit>; //lista de constantes

  { TAstFinal }
  //Clase para modelar al bloque FINALIZATION de una unidad
  TAstFinal = class(TAstCodeCont)
    adrr   : integer;  //dirección física
    constructor Create; override;
    destructor Destroy; override;
  end;

type  //Instructions relative elements
  //Sentences categories
  TSentenceType = (
    sntNull,       //Default value
    sntIF,         //Conditional IF
    sntREPEAT,     //REPEAT Loop
    sntWHILE,      //WHILE Loop
    sntFOR,        //FOR loop
    sntCASE        //Conditional CASE
//    sntExit        //Exit instruction
  );

  { TAstSentence }
  {Represents a Pascal instruction.}
  TAstSentence = class(TAstElement)
  public
    sntType: TSentenceType;  //Sentence type
    function sntTypeAsStr: string;
    constructor Create; override;
  end;

  //ASM instruction type
  TiType = (
    itOpcode,     //Common instruction with an Opcode and Operand.
    itLabel,      //An ASM label.
    itOrgDir,     //Instruction ORG
    itDefByte,    //Instruction DB
    itDefWord     //Instruction DW
  );
  //Valid operators for TAsmOperation
  TAsmOperator = (
    aopSelByte,  //Select a byte: operand.low, operand.high, >operand, <operand
    aopAddValue, //Add a value: operand + value
    aopSubValue  //Substract a value: operand - value
  );
  TAsmOperation = record
    oper: TAsmOperator;
    value: word;
  end;
  TAsmOperations = array of TAsmOperation;
  { TAsmOperand }
  TAsmOperand = object
    val: integer;    {The value of instruction operand, when it's a simple number.
                      When it's -1, the operand is a reference to an element and
                      should be read in "operRef".}
    ref: TAstElement; {Reference to element when operand refers to some Pascal or
                      ASM element.}
    nam: string;     {Operand name. Used when operand is an unsolved reference}
    used: boolean;   //Indicates if operand is used or not.
    //Operations
    operations: TAsmOperations;    //Operations applied on Operand
    procedure ClearOperations;
    procedure AddOperation(oper: TAsmOperator; value: word);
  end;
  { TAstAsmInstr }
  {Represents a line of assembler inside an ASM block.
  Consider this is a hardware dependent format}
  TAstAsmInstr = class(TAstElement)
    addr   : integer;  //Starting Address. Used only in code generation.
    iType  : TiType;   //ASM instruction type
    //Fields to generate instructions, using TP6502.codAsm() or similar.
    opcode : word;     {Formally should be TP6502Inst or similar. Defined as word
                        because we don't want to depend on unit P6502Utils here. }
    addMode: byte;     {Formally should be TP6502AddMode or similar. Defined as byte
                        because we don't want to depend on unit P6502Utils here. }
    operand: TAsmOperand;  //Operand for ASM instruction.
    operand2: TAsmOperand; //Second operand, used when it's needed.
    constructor Create; override;
  end;
  TAstAsmInstrs = specialize TFPGObjectList<TAstAsmInstr>;

  { TAstAsmBlock }
  {Represents an ASM block. An ASM block contains several ASM lines ()}
  TAstAsmBlock = class(TAstElement)
    undefInstrucs: TAstAsmInstrs;   //List of instruction with operands undefined
    constructor Create; override;
    destructor Destroy; override;
  end;

type  //Declaration elements (functions)

  //Function parameter
  TAstParam = record
    name    : string;      //Parameter name
    typ     : TAstTypeDec; //Reference to type
    vardec  : TAstVarDec;  //Reference to variable used for this parameter
    srcPos  : TSrcPos;     //Parameter location.
    adicVar : TAdicVarDec; //Aditional option for "vardec".
    isLocVar: boolean;     //Flag to indicate this parameter is not a parameter but a
                           //local variable. In ths way we can reuse this record to define
                           //local variabes too.
  end;
  TAstParamArray = array of TAstParam;

  TOperatorType = (
    opkNone,       //Not an operator
    opkUnaryPre,   //Unary Pre operator
    opkUnaryPost,  //Unary Post operator
    opkBinary      //Binary operator
  );
  TAsgMode = (
     asgNone        //Is not assign function
    ,asgSimple      //Simple Assignment: :=
    ,asgOperat      //Assignment with operation: +=, -=, ...
  );
  TFunGetset = (
    gsNone,         //Is not neither getter nor setter.
    gsGetInSimple,  //Getter INLINE simple:  _get()
    gsSetInSimple   //Setter INLINE simple: _set(value)
    );

  TCallType = (
    ctUsrNormal,   //Common user function
    ctUsrInline,   //Inline user function
    ctSysNormal,   //Common system function
    ctSysInline,   //Inline system function
    ctUsrExtern    //External function
  );
  { TAstFunBase }
  TAstFunBase = class(TAstProgFrame)
    retType    : TAstTypeDec;  //Type returned
    IsInterrupt: boolean;      //Indicates the function is an ISR
    IsForward  : boolean;      //Identifies a forward declaration.
  public  //Parameters management
    pars       : TAstParamArray; //parámetros de entrada
    procedure ClearParams;
    function ParamTypesList: string;
  public
    {These properties allows to have always the reference to the function declaration and
    the function implementation, when there is one.
    Declaration and implementation are separated only in:
      - Functions declared in a unit INTERFACE section.
      - Forward version.
    In other cases there, is just a function declaration without separated declaration.
    According to design:
     Declaration elements -> Contains information about:
        - The parameters and return value.
        - The calls.
        - The body. When there isn't implementation.
     Implementation elements -> Contains information about:
        - The parameters and return value.
        - Local variables.
        - The body (Calls to other elements.)
    }
    declar : TAstFunDec;  //Reference to declaration (When it's FORWARD or in INTERFACE)
    implem : TAstFunImp;  //Reference to implementation element.
  end;

  //Clase para almacenar información de las funciones
  //TCodSysInline = procedure(var fun: TMirOperand) of object;
  TCodSysNormal = procedure(funEleExp: TAstFunBase) of object;

  { TAstFunDec }
  {This element represents:
   - A single function declaration. When declaration includes the implementation too,like
   is common in a Pascal program:
   - A header declaration with separated implementation. Like it's used in INTERFACE
   section of a unit or a FORWARD declaration.
   When the declaration have a separated implementation. Most of the attributes are placed
   here.
  }
  TAstFunDec = class(TAstFunBase)
  public  //Main attributes
    sfi    : TSysFunID;
    adrr   : integer;  //Physical address where function is compiled.
    adrr2  : integer;  //Aditional physical address, for other entry point of the function.
    srcSize: integer;  {Tamaño del código compilado. En la primera pasada, es referencial,
                        porque el tamaño puede variar al reubicarse.}
    coded : boolean;   //Indicates the function was compiled in memory.
    procedure SetElementsUnused;
public mirFunDec: TObject;  {Formalmente debe ser TMirFunDec, pero se pone TObject porque,
                            por diseño, esta unidad no debe depender del MIR (en USES) y
                            así se evitan también o generar referencias circulares.}
  public  //Declaration
    function HasImplem: boolean; inline;
  public  //Operator
    operTyp    : TOperatorType; //Operand type
    oper       : string;   //Operator associated to the function when it works as a method.
    {Note that the precedence of the operators, is fixed in the compiler and depends
    only of operator.}
  public  //Flags for operators
    fConmutat  : boolean;      //Represents a conmutative binary operator.
    asgMode    : TAsgMode;     //Indicates if function is of the form: :=, +=, -=, ...
    getset     : TFunGetset;   //Indicates if function is getter or setter.
    funset     : TAstFunDec;  //Reference to related setter when this function is getter.
    funget     : TAstFunDec;  //Reference to related getter when this function is setter.
  public  //References
    callType    : TCallType;    //How to call the function.
//    //Callback to SIF Routine when callType is ctSysInline.
//    codSysInline: TMethod;    //Must be used after casting to TCodSysInline
    //Callback to SNF Routine when callType is ctSysNormal.
    codSysNormal: TCodSysNormal;
  public  //References information
    function nCalled: integer; override; //número de llamadas
    function nLocalVars: integer;
    function IsTerminal: boolean;
    function IsTerminal2: boolean;
  private //Manage of pending calls
    curSize: integer;
  public  //Manage of pending calls
    {Address of pending calls (JSR) made when the function was not still implemented }
    nAddresPend : integer;
    addrsPend   : array of word;
    procedure AddAddresPend(ad: word);
  public //Initialization
    {Reference to the elements list where is the body. It is:
      - TAstFunDec.elements, when there isn't a function implementation.
      - TAstFunImp.elements, when exists the a function implementation.
    }
    elemImplem: TAstElements;  //Reference to elements of implementation.
    {Reference to:
      - Body of function declaration  ,when there isn't a function implementation.
      - Body of function implementation, when exists one.
    }
    bodyImplem: TAstBody;
    constructor Create; override;
  end;
  TAstFunDecs = specialize TFPGObjectList<TAstFunDec>;

  { TAstFunImp }
  { Represents a function implementation (simple or inline) or a method (simple or inline).
  This element only exists if a function declaration exists.
  When this object exists, it contains some infromation that is not included in the
  function declaration:
    - The body of the function with all the instructions.
    - The local variables. They are present are children elements.}
  TAstFunImp = class(TAstFunBase)
  public  //Initialization
    constructor Create; override;
    destructor Destroy; override;
  end;
//  TAstFunImps = specialize TFPGObjectList<TAstFunImp>;

  function GenArrayTypeName(itTypeName: string; nItems: integer): string; inline;
  function GenPointerTypeName(refTypeName: string): string; inline;

implementation
{Functions to Generates standard names for dinamyc types creation. Have standard
names is important to let the compiler:
 * Reuse types definitions.
 * Implement compatibility for types.
}
function GenArrayTypeName(itTypeName: string; nItems: integer): string; inline;
begin
  if nItems=-1 then begin  //dynamic
    exit(PREFIX_ARR + '-' + itTypeName);
  end else begin          //static
    exit(PREFIX_ARR + IntToSTr(nItems) + '-' + itTypeName);
  end;
end;
function GenPointerTypeName(refTypeName: string): string; inline;
begin
  exit(PREFIX_PTR + '-' +refTypeName);
end;

{ TAstEleCaller }
function TAstEleCaller.CallerUnit: TAstElement;
{Devuelve el elemento unidad o programa principal, desde donde se hace esta llamada.}
var
  container: TAstElement;
begin
  {Se asume que la llamda se puede hacer solo desde dos puntos:
   - Desde una declaración.
   - Desde el cuerpo de una función.
  }
  if caller = nil then exit(nil);
  //La idea es retorceder hasta encontrar una unidad o el programa principal
  container := caller;
  while not (container.idClass in [eleUnit, eleProg]) do begin
    container := container.Parent;  //Go back in the Tree
  end;
  Result := container;
  //No debería haber otro caso
end;

{ TAstElement }
function TAstElement.AddElement(elem: TAstElement; position: integer=-1): TAstElement;
{Add an child element to the current node. Return reference. }
begin
  elem.Parent := self;  //Update reference
  if position<>-1 then begin
    elements.Insert(position, elem);   //Add to list of elements
  end else begin
    elements.Add(elem);   //Add to list of elements
  end;
  Result := elem;       //No so useful
end;
procedure TAstElement.Setname(AValue: string);
begin
  if Fname = AValue then Exit;
  Fname    := AValue;
  Funame   := Upcase(AValue);
end;
function TAstElement.LastNode: TAstElement;
{Devuelve una referencia al último nodo de "elements"}
begin
  if elements.Count = 0 then exit(nil);
  Result := elements[elements.Count-1];
end;
function TAstElement.Index: integer;
{Returns element location of node within its parent node.}
begin
  Result := Parent.elements.IndexOf(self);  //No so fast.
end;
//Gestion de llamadas al elemento
function TAstElement.nCalled: integer;
begin
  Result := lstCallers.Count;
end;
function TAstElement.IsCalledBy(callElem: TAstElement): boolean;
{Indica si el elemento es llamado por "callElem". Puede haber varias llamadas desde
"callElem", pero basta que haya una para devolver TRUE.}
var
  cal : TAstEleCaller;
begin
  for cal in lstCallers do begin
    if cal.caller = callElem then exit(true);
  end;
  exit(false);
end;
function TAstElement.IsCalledByChildOf(callElem: TAstElement): boolean;
{Indica si el elemento es llamado por algún elemento hijo de "callElem".
Puede haber varias llamadas desde "callElem", pero basta que haya una para devolver TRUE.}
var
  cal : TAstEleCaller;
begin
  for cal in lstCallers do begin
    if cal.caller.Parent = callElem then exit(true);
  end;
  exit(false);
end;
function TAstElement.IsCalledAt(callPos: TSrcPos): boolean;
{Indica si el elemento es llamado, desde la posición indicada.}
var
  cal : TAstEleCaller;
begin
  for cal in lstCallers do begin
    if cal.curPos.EqualTo(callPos) then exit(true);
  end;
  exit(false);
end;
function TAstElement.IsDeclaredAt(decPos: TSrcPos): boolean;
begin
  Result := srcDec.EqualTo(decPos);
end;
function TAstElement.FindCalling(callElem: TAstElement): TAstEleCaller;
{Busca la llamada de un elemento. Si no lo encuentra devuelve NIL.}
var
  cal : TAstEleCaller;
begin
  for cal in lstCallers do begin
    if cal.caller = callElem then exit(cal);
  end;
  exit(nil);
end;
function TAstElement.RemoveCallsFrom(callElem: TAstElement): integer;
{Elimina las referencias de llamadas desde un elemento en particular.
Devuelve el número de referencias eliminadas.}
var
  cal : TAstEleCaller;
  n, i: integer;
begin
  {La búsqueda debe hacerse al revés para evitar el problema de borrar múltiples
  elementos}
  n := 0;
  for i := lstCallers.Count-1 downto 0 do begin
    cal := lstCallers[i];
    if cal.caller = callElem then begin
//if callElem=nil then begin
//  debugln('+Eliminado de ' + self.name + ' caller: '+'nil');
//end else begin
//  debugln('+Eliminado de ' + self.name + ' caller: ' + callElem.name);
//end;
      lstCallers.Delete(i);
      inc(n);
    end;
  end;
  Result := n;
end;
procedure TAstElement.RemoveLastCaller;
//Elimina el último elemento llamador agregado.
begin
  if lstCallers.Count>0 then lstCallers.Delete(lstCallers.Count-1);
end;
procedure TAstElement.ClearCallers;
begin
  lstCallers.Clear;
end;
//function TAstElement.ExistsIn(list: TAstElements): boolean;
//{Debe indicar si el elemento está duplicado en la lista de elementos proporcionada.}
//var
//  ele: TAstElement;
//begin
//  for ele in list do begin
//    if ele.uname = uname then begin
//      exit(true);
//    end;
//  end;
//  exit(false);
//end;
//Gestión de los elementos llamados
procedure TAstElement.AddCalled(elem: TAstElement);
begin
  if lstCalled.IndexOf(elem) = -1 then begin
    lstCalled.Add(elem);
  end;
end;
function TAstElement.UpdateCalledAll: integer;
{Update list "lstCalledAll", using AddCalledAll_FromList().
  The return value is:
  * curNesting -> if not error happens.
  * <0  ->  If found recursion.
}
  function AddCalledAll(elem: TAstElement): boolean;
  {Add reference to lstCalledAll. That is, indicates some element is called from this
  element.
  If reference already exists, retunr FALSE.}
  begin
    //Solo agrega una vez el elemento
    if lstCalledAll.IndexOf(elem) = -1 then begin
      lstCalledAll.Add(elem);
      exit(true);
    end else begin
      exit(false);
    end;
  end;
  function AddCalledAll_FromList(lstCalled0: TAstListCalled): integer;
  {Add the call references (to lstCalledAll) of all elements of the list lstCalled0,
  including its called too (recursive).}
  var
    elem: TAstElement;
    err: Integer;
  begin
    inc(curNesting);    //incrementa el anidamiento
    if curNesting>maxNesting then maxNesting := curNesting;

    if lstCalled0.Count = 0 then exit;
    for elem in lstCalled0 do begin
//      debugln('Call to ' + elem.name + ' from ' + self.name);
//      if elem = self then begin
//        {This is some way to detect circular references like:
//        procedure proc2;
//        begin
//          proc1;
//        end;
//        procedure proc1;
//        begin
//          proc2;
//        end;
//        But fails when this element is not part of the circualr reference like
//        procedure proc2;   <-- We are proc2
//        begin
//          proc1;
//        end;
//        procedure proc1; <-- Here is the recursion
//        begin
//          proc1;
//        end;
//        In this case, several call to proc1() will be adding.
//        }
//        if curNesting = 1 then begin
//          exit(-1);
//        end else begin
//          exit(-2);
//        end;
//      end;
      //Add element reference
      if not AddCalledAll(elem) then begin
        //This is better way to detect circle references, because lstCalled, doesn't
        //contain duplicated calls.
        //exit(-1);  *** Commented in version 0.7.8 because a flase recursion detected
      end;
      if curNesting > 100 then begin
        //This is a secure way (but less elegant) for checking recursion. (If curNesting
        //grows too much). I don't expect this happens, unless exists some case I haven't
        //considered.
        exit(-1);
      end;
      //Verify if this element have other calls to add too.
      if elem.lstCalled.Count <> 0 then begin
        err := AddCalledAll_FromList(elem.lstCalled);
        if err<0 then exit(err);
      end;
    end;
    dec(curNesting);    //incrementa el anidamiento
    exit(curNesting);
  end;
begin
//debugln('UpdateCalledAll' + IntToStr(lstCalledAll.Count));
  lstCalledAll.Clear;  //By security
  curNesting := 0;
  maxNesting := 0;
  Result := AddCalledAll_FromList(lstCalled);
end;
function TAstElement.Path: string;
{Devuelve una cadena, que indica la ruta del elemento, dentro del árbol de sintaxis.}
var
  ele: TAstElement;
begin
  ele := self;
  Result := '';
  while ele<>nil do begin
    Result := '\' + ele.name + Result;
    ele := ele.Parent;
  end;
end;
function TAstElement.FindElemName(const eName: string; out ele: TAstElement
  ): boolean;
{Search a child element with the indicated name (eName). If found returns TRUE.}
var
  eleName: String;
  att: TAstElement;
begin
  eleName := UpCase(eName);
  for att in elements do begin
    if att.uname = eleName then begin
      ele := att;
      exit(true);
    end;
  end;
  ele := nil;
  exit(false);
end;
function TAstElement.FindIdxElemName(const eName: string; var idx0: integer): boolean;
{Busca un nombre en su lista de elementos. Inicia buscando desde idx0, hasta el inicio.
 Si encuentra, devuelve TRUE y deja en idx0, la posición en donde se encuentra.}
var
  i: Integer;
  uEleName: String;
begin
  uEleName := upcase(eName);
  //empieza la búsqueda en "idx0"
  for i := idx0 downto 0 do begin
    if elements[i].uname = uEleName then begin
      //sale dejando idx0 en la posición encontrada
      idx0 := i;
      exit(true);
    end;
  end;
  exit(false);
end;
function TAstElement.posXYin(const posXY: TPoint): boolean;
{Indica si la coordeda del cursor, se encuentra dentro de las coordenadas del elemento.}
var
  y1, y2: integer;
begin
  y1 := srcDec.row;
  y2 := srcEnd.row;
  //Primero verifica la fila
  if (posXY.y >= y1) and (posXY.y<=y2) then begin
    //Está entre las filas. Pero hay que ver también las columnas, si posXY, está
    //en los bordes.
    if y1 = y2 then begin
      //Es rango es de una sola fila
      if (posXY.X >= srcDec.col) and (posXY.X <= srcEnd.col) then begin
        exit(true)
      end else begin
        exit(false);
      end;
    end else if posXY.y = y1 then begin
      //Está en el límite superior
      if posXY.X >= srcDec.col then begin
        exit(true)
      end else begin
        exit(false);
      end;
    end else if posXY.y = y2 then begin
      //Está en el límite inferior
      if posXY.X <= srcEnd.col then begin
        exit(true)
      end else begin
        exit(false);
      end;
    end else begin
      //Está entre los límites
      exit(true);
    end;
  end else begin
    //Esta fuera del rango
    exit(false);
  end;
end;
//Inicialización
procedure TAstElement.Clear;
{Inicializa los campos del objeto. Este método es usado, solamente, para el Nodo Main,
porque los otors nodos son eliminados de la memoria al iniciar el árbol}
begin
  elements.Clear;
  lstCallers.Clear;
  lstCalled.Clear;
  lstCalledAll.Clear;
end;
constructor TAstElement.Create;
begin
  idClass := eleNone;
  elements := TAstElements.Create(true);   //Main container
  lstCallers:= TAstListCallers.Create(true);
  lstCalled := TAstListCalled.Create(false);  //solo guarda referencias
  lstCalledAll:= TAstListCalled.Create(false);
end;
destructor TAstElement.Destroy;
begin
  lstCalledAll.Destroy;
  lstCalled.Destroy;
  lstCallers.Destroy;
  elements.Destroy;
  inherited Destroy;
end;

{ TAsmOperand }
procedure TAsmOperand.ClearOperations;
begin
  setlength(Operations, 0);
end;
procedure TAsmOperand.AddOperation(oper: TAsmOperator; value: word);
var
  n: Integer;
begin
  n := high(Operations)+1;  //Number of elements
  setlength(Operations, n+1);
  Operations[n].oper  := oper;
  Operations[n].value := value;
end;

{ TConsValue }
procedure TConsValue.InitItems;
begin
  nItems := 0;
  curSize := CONS_ITEM_BLOCK;   //Block size
  setlength(items, curSize);  //initial size
end;
procedure TConsValue.AddConsItem(const c: TConsValue);
begin
  items[nItems] := c;
  inc(nItems);
  if nItems >= curSize then begin
    curSize += CONS_ITEM_BLOCK;   //Increase size by block
    setlength(items, curSize);  //make space
  end;
end;
procedure TConsValue.CloseItems;
begin
  setlength(items, nItems);
end;
function TConsValue.LByte: byte;
begin
  Result := LO(word(valInt));
end;
function TConsValue.HByte: byte;
begin
  Result := HI(word(valInt));
end;
function TConsValue.EByte: byte;
begin
  Result := (valInt >> 16) and $FF;
end;
function TConsValue.UByte: byte;
begin
  Result := (valInt >> 24) and $FF;
end;
function TConsValue.valuesAsString: string;
{Returns a string containing the abstract of values stored.}
begin
  Result := 'int=' + IntToStr(ValInt) + ',bool=' + IfThen(ValBool,'T','F');
end;
{ TAstBlock }
constructor TAstBlock.Create;
begin
  inherited Create;
  idClass := eleBlock;
end;
destructor TAstBlock.Destroy;
begin
  inherited Destroy;
end;
{ TAstExpress }
function TAstExpress.opTypeAsStr: string;
begin
  WriteStr(Result, opType);
end;
function TAstExpress.StoAsStr: string;
//Resturns storage as string.
begin
  WriteStr(Result, Sto);
end;
procedure TAstExpress.StringToArrayOfChar(str: string);
{Init the constant value as array of char from a string.}
var
  i: Integer;
begin
  value.nItems := length(str);
  setlength(value.items, value.nItems);
  for i:=0 to value.nItems-1 do begin
    value.items[i].ValInt := ord(str[i+1]);
  end;
end;
function TAstExpress.ValueIsZero: boolean;
{Check if the Expression is numéric and the value stored is 0. Only applies to constants.}
begin
  Result := (Typ.group in [t_uinteger, t_integer, t_float]) and  //Is a numeric type
            (value.ValInt = 0);  //Has value zero.
end;
function TAstExpress.IsConstantPlusVariable: boolean;
{Identifies if this operand is a function of type: constant + variable}
var
  op1, op2: TAstExpress;
begin
  if (opType = otFunct) and (elements.Count = 2) then begin
    //Two parameter function
    op1 := TAstExpress(elements[0]);
    op2 := TAstExpress(elements[1]);
    exit( (op1.Sto = stConst) and op1.evaluated and (op2.Sto = stRamFix) and (name='_add'));
  end else begin
    exit(false);
  end;
end;
function TAstExpress.IsVariablePlusConstant: boolean;
var
  op1, op2: TAstExpress;
begin
  if (opType = otFunct) and (elements.Count = 2) then begin
    //Two parameter function
    op1 := TAstExpress(elements[0]);
    op2 := TAstExpress(elements[1]);
    exit( (op1.Sto = stRamFix) and (op2.Sto = stConst) and op2.evaluated and (name='_add'));
  end else begin
    exit(false);
  end;
end;
procedure TAstExpress.exchange2Children;
{Exchange two children elements}
var
  tmp: TAstElement;
begin
  if (elements.Count = 2) then begin
    elements.Exchange(0,1);
  end;
end;
//Access to constant value
function TAstExpress.val: dword; inline;
begin
  Result := value.ValInt;
end;
function TAstExpress.valL: word; inline;
begin
  Result := LO(word(value.ValInt));
end;
function TAstExpress.valH: word; inline;
begin
  Result := HI(word(value.ValInt));
end;
function TAstExpress.valU: word; inline;
begin
  Result := (value.valInt >> 24) and $FF;
end;
function TAstExpress.valE: word; inline;
begin
  Result := (value.valInt >> 16) and $FF;
end;
function TAstExpress.valWlo: word; inline;
begin
  Result := word(value.ValInt);
end;
function TAstExpress.valWhi: word; inline;
begin
  Result := (value.valInt >> 16) and $FFFF;
end;
procedure TAstExpress.SetLiteralBoolConst(valBool: Boolean);
{Set the value of a Constant boolean expression.}
begin
  consType := ctLiteral;
  evaluated := true;
  value.valBool := valBool;    //Tal vez no sea necesario si usamos solo "value.ValInt"
  //Como en algunos casos se usa el valor numérico, lo fijamos también.
  if valBool then begin
    value.ValInt := 255;
  end else begin
    value.ValInt := 0;
  end;
end;
procedure TAstExpress.SetLiteraltIntConst(valInt: Int64);
begin
  consType := ctLiteral;
  evaluated := true;
  value.ValInt := valInt;
end;
procedure TAstExpress.SetConstRef(cons0: TAstConsDec);
begin
  consType := ctConsRef;
  consRef := cons0;  //Keep reference
  evaluated := false;  //To force evaluation
end;
procedure TAstExpress.SetAddrVar(var0: TAstVarDec);
begin
  consType := ctVarAddr;
  addrVar := var0;  //Keep reference
  evaluated := false;  //To force evaluation
end;
procedure TAstExpress.SetAddrFun(fun0: TAstFunDec);
begin
  consType := ctFunAddr;
  addrFun := fun0;  //Keep reference
  evaluated := false;  //To force evaluation
end;
function TAstExpress.IsCAvar: Boolean;
{Indicates if this Expression is a variable addressed by a constant address.
Only is valid when the Operand type is "otVariab" }
begin
  Result := (elements.Count=0) or  //Addressed by vardec0
            (elements.Count=1);    //Addressed by a consatnt expression
end;
function TAstExpress.IsCVAvar: Boolean;
{Indicates if this Expression is a variable addressed by a constant address and a variable
index.
Only is valid when the Operand type is "otVariab" }
begin
  Result := (elements.Count=2);    //Addressed by a consatnt expression and a variable expression
end;
function TAstExpress.vardec: TAstVarDec;
{Returns the reference to Variable declaration when this Expression is a simple
reference to a variable declared. Something like this:
VAR x: byte;
x := 0;
If this is not the case, we returns NIL.}
var
  constOffset: TAstExpress;
begin
  constOffset := TAstExpress(elements[0]);
  if constOffset.opType <> otConst then exit(nil);  //It's not constant;
  if constOffset.consType = ctVarAddr then begin
    //It's a constant of the form @<variable>.
    exit(constOffset.addrVar);
  end else begin
    exit(Nil);
  end;
end;
//Initialization
constructor TAstExpress.Create;
begin
  inherited Create;
  idClass := eleExpress;
end;
destructor TAstExpress.Destroy;
begin
  inherited Destroy;
end;
{ TAstSentence }
function TAstSentence.sntTypeAsStr: string;
begin
  WriteStr(Result, sntType);
end;
constructor TAstSentence.Create;
begin
  inherited Create;
  //name := 'sent';  Don't give name to optimize
  sntType := sntNull;
  idClass := eleSenten;
end;
{ TAstAsmInstr }
constructor TAstAsmInstr.Create;
begin
  inherited Create;
  idClass := eleAsmInstr;
end;
{ TAstAsmBlock }
constructor TAstAsmBlock.Create;
begin
  inherited Create;
  idClass := eleAsmBlock;
  undefInstrucs:= TAstAsmInstrs.Create(false);
end;
destructor TAstAsmBlock.Destroy;
begin
  undefInstrucs.Destroy;
  inherited Destroy;
end;
{ TExitCall }
//function TExitCall.IsObligat: boolean;
//{Indica si el exit se encuentra dentro de código obligatorio}
//begin
//  {Para detectar si el exit() está en código obligatorio, se verifica si se enceuntra
//  directamente en el Body, y no dentro de bloques de tipo
//  IF, WHILE, FOR, REPEAT. Este método no es del todo preciso si se considera que puede
//  haber también código obligatorio, dentro de bloques REPEAT o códigos IF definido en
//  tiempo de compilación. Se podrúa mejorar después.}
//  Result := (codeBlk.idClass = eleBody);  //Cuerpo de una función o el programa principal
//end;
{ TAstProgFrame }
function TAstProgFrame.BodyNode: TAstBody;
{Devuelve la referencia al cuerpo del programa. Si no lo encuentra, devuelve NIL.}
var
  elem: TAstElement;
begin
  if elements.Count = 0 then exit(nil);
  elem := elements[elements.Count-1];
  if elem.idClass <> eleBody then begin
    exit(nil);  //No debería pasar
  end;
  //Devuelve referencia
  Result := TAstBody(elem);
end;
procedure TAstProgFrame.RegisterExitCall(exitSent: TAstExpress);
{Registra una llamada a una instrucción exit(). De momento solo se usa para actualizar a
"firstObligExit".
"exitSent" es la instrucción exit() que queremos registrar. Debe haber sido ya incluida
en el AST.}
begin
  if firstObligExit = nil then begin
    //Aún no ha sido inicializado. Este puede ser el primero.
    //Verifica si está dentro de código obligatorio.
    {Para detectar si el exit() está en código obligatorio, se verifica si se enceuntra
     directamente en el Body, y no dentro de bloques de tipo
     IF, WHILE, FOR, REPEAT. Este método no es del todo preciso si se considera que puede
     haber también código obligatorio, dentro de bloques REPEAT o códigos IF definido en
     tiempo de compilación. Se podrúa mejorar después.}
    if exitSent.Parent.idClass = eleBody then begin
      {Es código obligatorio, porque si estuviera dentro de estructuras IF o REPEAT o
      WHILE; el "idClass" sería un elemento "eleBlock".}
      firstObligExit := exitSent;
    end;
  end;
end;

//procedure TAstProgFrame.AddExitCall(srcPos: TSrcPos; codeBlk: TAstCodeCont);
//var
//  exitCall: TExitCall;
//begin
//  exitCall := TExitCall.Create;
//  exitCall.srcPos := srcPos;
//  {Se guarda el ID, en lugar de la referencia al bloque, porque en el modo de trabajo
//   actual, los bloques se crean y destruyen, dinámicamente}
//  exitCall.codeBlk  := codeBlk;
//  lstExitCalls.Add(exitCall);
//end;
//function TAstProgFrame.ObligatoryExit: TExitCall;
//{Devuelve la referencia de una llamada a exit(), dentro de código obligatorio del Body.
//Esto ayuda a saber si ya el usuario incluyó la salida dentro del código y no es necesario
//agregar un RETURN al final.
//Si no encuentra ninguna llamada a exit() en código obligatorio, devuelve NIL.
//Según la documentación, el exit() en código obligatorio, solo debe estar al final del
//código del procedimiento. Si estuviera antes, dejaría código "no-ejecutable".}
//var
//  exitCall: TExitCall;
//begin
//  if lstExitCalls.Count = 0 then exit(nil);  //No incluye exit()
//  for exitCall in lstExitCalls do begin
//    //Basta detectar un exit(), porque no se espera que haya más.
//    if exitCall.IsObligat then begin
//      exit(exitCall);  //tiene una llamada en código obligatorio
//    end;
//  end;
//  //No se encontró ningún exit en el mismo "body"
//  exit(nil);
//end;
//Inicialización
procedure TAstProgFrame.Clear;
begin
  inherited Clear;
  //lstExitCalls.Clear;
end;
constructor TAstProgFrame.Create;
begin
  inherited Create;
  idClass := eleProgFrame;
  //lstExitCalls:= TExitCalls.Create(true);
end;
destructor TAstProgFrame.Destroy;
begin
  //lstExitCalls.Destroy;
  inherited Destroy;
end;
{ TAstConsDec }
constructor TAstConsDec.Create;
begin
  inherited;
  idClass := eleConsDec;
end;
{ TAstVarDec }
function TAstVarDec.Gettyp: TAstTypeDec;
begin
  if ftyp.copyOf<>nil then begin
    Result := ftyp.copyOf
  end else begin
    Result := ftyp;
  end;
end;
procedure TAstVarDec.Settyp(AValue: TAstTypeDec);
begin
  ftyp := AValue;
end;

constructor TAstVarDec.Create;
begin
  inherited;
  idClass:=eleVarDec;
end;
destructor TAstVarDec.Destroy;
begin
  inherited Destroy;
end;
function TAstTypeDec.getSize: word;
var
  nItms: integer;
begin
  if catType = tctArray then begin
    //Array size is calculated
    if nItems= -1 then exit(0) else exit(itmType.size * nItems);
  end else if catType = tctPointer then begin
    exit(2);  //Pointer are like words
  end else if catType = tctObject then begin
    exit(objSize);
    exit(0);
  end else begin
    exit(fSize)
  end;
end;
procedure TAstTypeDec.setSize(AValue: word);
begin
  fSize := AValue;
end;

function TAstTypeDec.groupStr: string;
begin
  WriteStr(Result, group);
end;
function TAstTypeDec.catTypeStr: string;
begin
  WriteStr(Result, catType);
end;
function TAstTypeDec.nItems: integer;
begin
  if copyOf<>nil then begin
    exit(copyOf.consNitm.value^.ValInt)
  end else begin
    exit(consNitm.value^.ValInt)
  end;
end;
{ TAstTypeDec }
function TAstTypeDec.IsByteSize: boolean;
{Indica si el tipo, tiene 1 byte de tamaño}
begin
//  if copyOf<>nil then exit(copyOf.IsByteSize);  //verifica
  Result := size = 1;
end;
function TAstTypeDec.IsWordSize: boolean;
{Indica si el tipo, tiene 2 bytes de tamaño}
begin
//  if copyOf<>nil then exit(copyOf.IsWordSize);  //verifica
  Result := size = 2;
end;
function TAstTypeDec.IsDWordSize: boolean;
{Indica si el tipo, tiene 4 bytes de tamaño}
begin
//  if copyOf<>nil then exit(copyOf.IsDWordSize);  //verifica
  Result := size = 4;
end;
function TAstTypeDec.IsArrayOf(itTyp: TAstTypeDec; numIt: integer): boolean;
{Indicates if this type is an array of the specified type and with the specified
number of elements.}
begin
  if catType <> tctArray then exit(false);
  //I'm an array
//  debugln('Buscando arreglo en: ' + self.name);
  if consNitm = nil then exit(false);  //Not yet set the size.
  exit( (nItems = numIt) and itmType.IsEquivalent(itTyp) );
end;
function TAstTypeDec.IsPointerTo(ptTyp: TAstTypeDec): boolean;
begin
  exit( (catType = tctPointer) and ptrType.IsEquivalent(ptTyp) );
end;
function TAstTypeDec.IsEquivalent(typ: TAstTypeDec): boolean;
{Indicates if the type is the same type as the specified or has the same definition.}
begin
  if self = typ then exit(true);
  if catType <> typ.catType then exit(false);
  //Have the same category
  if (self.copyOf = typ) or (typ.copyOf = self) then exit(true);
  if (self.copyOf<>nil) and (self.copyOf = typ.copyOf) then exit(true);
  if catType = tctArray then begin
    //Equivalence for arrays
    if (self.nItems = typ.nItems) and itmType.IsEquivalent(typ.itmType) then exit(true);
  end else if catType = tctPointer then begin
    //Equivalence for pointers
    if (self.ptrType.IsEquivalent(typ.ptrType)) then exit(true);
  end;
  exit(false);
end;
constructor TAstTypeDec.Create;
begin
  inherited;
  idClass:=eleTypeDec;
  //Ceeate list
  internalTypes:= TAstTypeDecs.Create(true);
end;
destructor TAstTypeDec.Destroy;
begin
  internalTypes.Destroy;
  inherited;
end;
{ TAstProg }
constructor TAstProg.Create;
begin
  inherited;
  idClass := eleProg;
  Parent := nil;  //la raiz no tiene padre
end;
{ TAstFunBase }
procedure TAstFunBase.ClearParams;
//Elimina los parámetros de una función
begin
  setlength(pars,0);
end;
function TAstFunBase.ParamTypesList: string;
{Devuelve una lista con los nombres de los tipos de los parámetros, de la forma:
(byte, word) }
var
  tmp: String;
  j: Integer;
begin
  tmp := '';
  for j := 0 to High(pars) do begin
    tmp += pars[j].name+', ';
  end;
  //quita coma final
  if length(tmp)>0 then tmp := copy(tmp,1,length(tmp)-2);
  Result := '('+tmp+')';
end;
{ TAstFunDec }
procedure TAstFunDec.SetElementsUnused;
{Marca todos sus elementos con "nCalled = 0". Se usa cuando se determina que una función
no es usada.}
var
  elem: TAstElement;
begin
  //Marca sus elementos, como no llamados
  for elem in elements do begin
    elem.ClearCallers;
  end;
end;
procedure TAstFunDec.AddAddresPend(ad: word);
{Add a pending address to the function to be completed later.}
begin
  addrsPend[nAddresPend] := ad;
  inc(nAddresPend);
  if nAddresPend > curSize then begin
    curSize += CONS_ITEM_BLOCK;   //Increase size by block
    setlength(addrsPend, curSize);  //make space
  end;
end;
function TAstFunDec.nCalled: integer;
begin
  if IsInterrupt then exit(1);   //Los INTERRUPT son llamados implícitamente
  Result := lstCallers.Count;
end;
function TAstFunDec.nLocalVars: integer;
{Returns the numbers of local variables for this function.}
var
  elem : TAstElement;
begin
  Result := 0;
  for elem in elements do begin
    if elem.idClass = eleVarDec then inc(Result);
  end;
end;
function TAstFunDec.IsTerminal: boolean;
{Indica si la función ya no llama a otras funciones. Para que funcione, se debe haber
llenado primero, "lstCalled".}
begin
  Result := (lstCalled.Count = 0);
end;
function TAstFunDec.IsTerminal2: boolean;
{Indica si la función es Terminal, en el sentido que cumple:
- Tiene variables locales.
- No llama a otras funciones o las funciones a las que llama no tienen variables locales.
Donde "Variables" locales, se refiere también a parámetros del procedimiento.}
var
  called   : TAstElement;
  nCallesFuncWithLocals: Integer;
begin
  if nLocalVars = 0 then exit(false);
  //Tiene variables locales
  //Verifica llamada a funciones
  nCallesFuncWithLocals := 0;
  for called in lstCalledAll do begin
    if called.idClass = eleFuncDec then begin
      if TAstFunDec(called).nLocalVars > 0 then inc(nCallesFuncWithLocals);
    end;
  end;
  if nCallesFuncWithLocals = 0 then begin
    //Todas las funciones a las que llama, no tiene variables locales
    exit(true);
  end else begin
    exit(false);
  end;
end;
function TAstFunDec.HasImplem: boolean;
{Indica si la declaración tiene implementación separada.}
begin
  exit(implem<>nil);
end;
constructor TAstFunDec.Create;
begin
  inherited Create;
  idClass := eleFuncDec;
  //Init addrsPend[]
  nAddresPend := 0;
  curSize := CONS_ITEM_BLOCK;   //Block size
  setlength(addrsPend, curSize);  //initial size
  declar := Self;
  //By default, we assume this is a declaration and implementation.
  elemImplem := elements;
end;
{ TAstFunImp }
//Inicialización
constructor TAstFunImp.Create;
begin
  inherited;
  idClass:=eleFuncImp;
  implem := Self;
  //self.declar must be set later.
end;
destructor TAstFunImp.Destroy;
begin
  inherited Destroy;
end;
{ TAstUnit }
procedure TAstUnit.ReadInterfaceElements;
{Actualiza la lista "InterfaceElements", con los elementos accesibles desde la
sección INTERFACE. De esta forma se facilita la exploración de elementos públicos.}
var
  ele: TAstElement;
begin
  InterfaceElements.Clear;
  //Solo basta explorar a un nivel
  for ele in elements do begin
    if ele.location = locInterface then begin
       InterfaceElements.Add(ele);
    end;
  end;
end;
constructor TAstUnit.Create;
begin
  inherited;
  idClass:=eleUnit;
  InterfaceElements:= TAstElements.Create(false);
end;
destructor TAstUnit.Destroy;
begin
  InterfaceElements.Destroy;
  inherited Destroy;
end;
{ TAstBody }
constructor TAstBody.Create;
begin
  inherited;
  idClass := eleBody;
end;
destructor TAstBody.Destroy;
begin
  inherited Destroy;
end;
{ TAstFinal }
constructor TAstFinal.Create;
begin
  inherited Create;
  idClass := eleFinal;
end;
destructor TAstFinal.Destroy;
begin
  inherited Destroy;
end;

end.
//2067
