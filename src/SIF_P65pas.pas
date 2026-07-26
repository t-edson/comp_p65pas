unit SIF_P65pas;
{$mode ObjFPC}{$H+}
{
Implementación del Generador de Código del compilador.
Esta implementación no permitirá recursividad, por las limitaciones de recursos de los
dispositivos más pequeños.
El compilador está orientado a uso de registros (solo hay uno) y memoria RAM. No se
manejan estructuras en pila.
Solo se manejan datos de tipo boolean, byte y word, y operaciones sencillas.
}
{La arquitectura definida aquí contempla:

Un registro de trabajo A, de 8 bits (el acumulador del PIC).
Dos registros auxiliares X e Y.
Tres registros de trabajo adicionales  U,E y H de 8 bits cada uno (Creados a demanda).

La forma de trabajo por tipos es:

TIPO BOOLEAN:
* Se almacenan en un byte. Cualquier valor diferente de cero se considera TRUE.
* Los resultados se devuelven en el bit Z, del registro SR.
TIPO CHAR Y BYTE:
* Se almacenan en un byte.
* Los resultados se devuelven en el registro acumulador A
TIPO WORD:
* Se almacenan en 2 bytes.
* Los resultados se devuelven en los registros (H,A).

Opcionalmente, si estos registros ya están ocupados, se guardan primero en la pila, o se
usan otros registros auxiliares.

Despues de ejecutar alguna operación booleana que devuelva una expresión, se
actualizan las banderas: BooleanBit y BooleanInverted, que implican que:
* Si BooleanInverted es TRUE, significa que la lógica de C o Z está invertida.
* La bandera BooleanBit, indica si el resultado se deja en C o Z.

Por normas de Xpres, se debe considerar que:
* Todas las BOR reciben sus dos parámetros en las variables p1^ y p2^.
* El resultado de cualquier expresión se debe dejar indicado en el objeto "res".

Si el objeto "res" es constante, almacena directamente sus valores en:
* "valInt" para tipos enteros y enteros sin signo.
* "valBool" para el tipo booleano.
* "valStr" para el tipo string.

Las rutinas de operación, deben devolver su resultado en "res".
Para mayor información, consultar la doc. técnica.
 }
interface
uses
  Classes, SysUtils, MirList,
  AstElemP65, {*** Esta dependencia debería desaparecer.}
  CompGlobals, CompOptions, alexiaLex, LazLogger, StrUtils, CPUCore,
  P65C02utils;
const
  CONS_ITEM_BLOCK = 20;
type
    {Información sobre los saltos con la instrucción IF_TRUE}
  TIfInfo = record
    igoto  : integer;   //Address where is GOTO
  end;
  //Modes the compiler can call to the Code Generation routines
  TCompMod = (
    cmGenCode,   //Generating code
    cmConsEval   //Evaluating constant
  );
  //Information about the las ASM code generated. Used for optimization.
  TLastASMcode = (
               lacNone,    //No special code generated.
               //Flags applied to boolean expression results.
               lacCopyZtoA, {Last ASM code is for obtaining boolean expression in regA
                             from Z.}
               lacInvZtoA,  {Last ASM code is for obtaining boolean expression in regA
                             from Z (inverted).}
               lacCopyCtoA, {Las ASM code if for obtaining, boolean expression in regA,
                            using the bit C and copied to A. }
               lacInvCtoA,  {Last ASM code is for obtaining boolean expression in regA
                             from C (inverted).}
               lacInvAtoA   {Value of regA is inverted in all bits to regA}
               );
  PtrTCPURam = ^TCPURam;

  { TGenTypeDec }
  //A class for defining types inside the Code Generator.
  TGenTypeDec = class(TAstCodeCont)
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
    OnRequireWR : procedure of object; //Used to detect dependencies on Work registers.  *** ¿Se necesita?
  public   //Identification
    copyOf  : TGenTypeDec;  //Indicates this type is copy of other
    group   : TTypeGroup;   //Type group (numéric, string, etc)
    catType : TCatType;   //Categoría del tipo
    property size: word read getSize write setSize;   //Tamaño en bytes del tipo
    function groupStr: string;
    function catTypeStr: string;
  public   //Fields when type is Array or pointer
    consNitm: TAstConsDec;  //Reference to constant defining the number of items.
    itmType : TGenTypeDec;  {Reference to the item type when it's array.
                                TArr = array[255] of byte;  //itemType = byte
                            }
    isDynam : boolean;      //Indicates the size is dynamic. No current supported except when initialized.
    ptrType : TGenTypeDec;  {Reference to the type pointed, when it's pointer.
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
    function IsArrayOf(itTyp: TGenTypeDec; numIt: integer): boolean;
    function IsPointerTo(ptTyp: TGenTypeDec): boolean;
    function IsEquivalent(typ: TGenTypeDec): boolean;
  public  //Initialization
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TGenConsValue }

  TGenConsValue = object
  public  //Status
    consType : TConsType;   //Constant type for the atomic type.
    function evaluated(typ: TGenTypeDec): Boolean;
    procedure evaluate(typ: TGenTypeDec);
  public  //Values the for atomic type.
    ValInt  : Int64;    //For values t_integer y t_uinteger
    ValFloat: extended; //For values t_float
    ValBool : boolean;  //For values t_boolean
    ValStr  : string;   //For values t_string
  public  //Aditional information
    consRef  : TMirConDec;  //Ref. to TAstConsDec when consType=ctConsRef *** ¿Se necesita además de "conDec"?
    addrVar  : TMirVarDec;   //Ref. to TAstVarDec  when consType=ctVarAddr
    addrFun  : TMirFunDec;   //Ref. to TEleFun when consType=ctFunAddr
  public //Support for Arrays and Objects
    items   : array of TGenConsValue;  //Ítems list
    nItems  : integer;  //Number of items
    curSize : integer;  //*** ¿Se usa?
    fname   : String;   //Field name. Used to identify a field when this constant is an object.
    procedure InitItems;
    procedure AddConsItem(const c: TGenConsValue);
    procedure CloseItems;
  public  //Access to ValInt
    function LByte: byte; inline;  //Returns low byte of integer value.
    function HByte: byte; inline;  //Returns high byte of integer value.
    function EByte: byte; inline;
    function UByte: byte; inline;
    function valuesAsString: string;
  end;


  //A class for defining variables inside the Code Generator.
  { TGenVarDec }
  TGenVarDec = Class(TMirElement)
    typ      : TGenTypeDec; //Variable type.
    vardec   : TAstVarDec;  //AST Declared variable, when it's associated to AST. If not it's NIL.
    IsParameter: Boolean;   //Flag for variables that are parameters.
    required : boolean;     {Indicates the variable is required to be allocated. Work
                            for variables used as registers. *** ¿Es necesario?}
  public   //Manejo de parámetros adicionales
    inival   : TGenConsValue;  //Constant value
    adicPar  : TAdicVarDec;  //Parámetros adicionales en la declaración de la variable.
  public  //Campos para guardar las direcciones físicas asignadas en RAM.
    allocated: boolean;    //Activated when variable is allocated (RAM or register).
    storage  : TStorage;   //Depend on adicPar.hasAdic.
    addr     : word;       //Base address.
    function addrL: word; inline;  //Devuelve la dirección absoluta de la variable (LOW)
    function addrH: word; inline;  //Devuelve la dirección absoluta de la variable (HIGH)
    function addrE: word; inline;  //Devuelve la dirección absoluta de la variable (EXTRA)
    function addrU: word; inline;  //Devuelve la dirección absoluta de la variable (ULTRA)
    function AddrString: string;   //Devuelve la dirección física como cadena
    procedure ResetAddress; //Limpia las direcciones físicas
    function stoStr: string;
  public
    constructor Create; virtual;
  end;

  { TGenFunDec }

  TGenFunDec = class(TAstFunBase)
  public  //Main attributes
    adrr   : integer;  //Physical address where function is compiled.
    adrr2  : integer;  //Aditional physical address, for other entry point of the function.
    srcSize: integer;  {Tamaño del código compilado. En la primera pasada, es referencial,
                        porque el tamaño puede variar al reubicarse.}
    coded : boolean;   //Indicates the function was compiled in memory.
public mirFunDec: TObject;  //Formalmente debe ser TMirFunDec, pero se pone TObject para no generar referencias circulares.
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
    //Callback to SIF Routine when callType is ctSysInline.
    codSysInline: TMethod;    //Must be used after casting to TCodSysInline
    //Callback to SNF Routine when callType is ctSysNormal.
    codSysNormal: TCodSysNormal;
  public  //References information
    IsTerminal2: boolean;      //Flag. Indicates function is terminal.
    IsTerminal: boolean;
    function nLocalVars: integer;
  private //Manage of pending calls
    curSize: integer;
  public  //Manage of pending calls
    {Address of pending calls (JSR) made when the function was not still implemented }
    nAddresPend : integer;
    addrsPend   : array of word;
    procedure AddAddresPend(ad: word);
  public //Initialization
    {Reference to the elements list where is the body. It is:
      - TEleFunDec.elements, when there isn't a function implementation.
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

  { TGenOperand }
  TGenOperand = object
    Text    : string;        //Label for the operand.
    opType  : TopType;       //Operand type (otVariab, otConst, otFunct) like AST elements.
    Sto     : TStorage;      //Storage of the value (memory, register, value)
    Typ     : TGenTypeDec;   //Data type for the operand.
    conDec  : TMirConDec;    //Ref. to constant declaration.
    astOperand: TAstExpress; //Ref. to AST element. Should be used only for error location.
    function StoAsStr: string;  //Storage as string
  public //Fields used when "opType" is otFunc.
    funDec  : TGenFunDec;    //Reference to function declaration, when it's accesible.
    elements: array of TGenOperand; //Parameter list.  ***Mejor Cambiar al nombre "pars"
    function FunCallText: string;
    procedure SetParAsVar(i: Integer; vardec0: TMirVarDec);
  public  //Fields used when "opType" is otConst
    value   : TGenConsValue;  //Constant value
    //Functions to read values.
    function val: dword;
    function valL: word;
    function valH: word;
    function valU: word;
    function valE: word;
    function valWlo: word;
    function valWhi: word;
    procedure SetCon_Literal(valBool: Boolean);
    procedure SetCon_Literal(valInt: Int64);
    procedure SetCon_ConstRef(cons0: TMirConDec);
    procedure SetCon_VarAddr(var0: TMirVarDec);
    procedure SetCon_FunAddr(fun0: TMirFunDec);
    procedure ToLiteral();         //Convert constant to "ctLiteral"
    function evaluated: boolean;   //Activated when constant is evaluated.
  public  //Fields used when "opType" is otVariab
    {We use until two fields to get the effective address. They are:
      - The constant offset. Stored in "value" field.
      - The index variable. Referenced by "idxvar" field.
    The use of some specific fields depends on "Sto":
      - stRamFix -> Uses only the constant offset.
      - stRamVar -> Uses only the index variable.
      - stRamVarOf -> Uses the constant offset and the index variable.
    }
    idxvar: TMirVarDec;  //It should be a declared variable.
    function allocated: boolean;
    function vardec: TMirVarDec;
    procedure SetVar_RamFix(vardec0: TMirVarDec);
    procedure SetVar_RamFix(addr: word);
    procedure SetVar_RamVarOf(vardec0: TMirVarDec; idxVar0: TMirVarDec);
  public  //Campos creados solo para compatibilidad para soportar la implementación de las SIF. Deberían cambiarse a futuro.
    function offs: integer;  //Dirección de una variable ???
    function add: word;  //Dirección de una variable ???
    function addL: word;  //Dirección de una variable ???
    function addH: word;
    function name: String;
    function srcDec: TSrcPos;  //Tal vez esta propiedad deba quedar aquí
    procedure Exchange(i1, i2: integer);
  end;

  //This type will be used to cast the field TAstTypeDec.OnLoadToWR.
  //This needs to be defined here because uses the TMirOperand type declared here.
  TProcLoadOperand = procedure(fun: TGenOperand) of object;

var
  OnError: procedure (msg: string);
  OnWarning: procedure (msg: string);
//  OnError: procedure (msg: string; const srcPos: TSrcPos);
  HayError: boolean;             //Flag for errors

  snfBytMulByt16: TMirFunDec;
  snfWordShift_l: TMirFunDec;
  snfDelayMs:     TMirFunDec;
  snfBytDivByt8:  TMirFunDec;
  snfWrdDivWrd16: TMirFunDec;
  //Referencia a las opciones del compilador
  opt: TCompOptions;
  //Copia de la propiedad TCompOptions.cpuMode. Se maneja como copia, solo para
  //acelerar el acceso.
  cpuMode: TCpuMode;
const
  STACK_SIZE = 8;      //tamaño de pila para subrutinas en el PIC
  MAX_REGS_AUX_BYTE = 6;   //cantidad máxima de registros a usar
  MAX_REGS_AUX_BIT = 4;    //cantidad máxima de registros bit a usar
  MAX_REGS_STACK_BYTE = 8; //cantidad máxima de registros a usar en la pila
  MAX_REGS_STACK_BIT = 4;  //cantidad máxima de registros a usar en la pila

var
  typNull : TGenTypeDec;

  typByte : TGenTypeDec;
  typBool : TGenTypeDec;
  typChar : TGenTypeDec;
  typWord : TGenTypeDec;
  typDWord: TGenTypeDec;

  pic        : TP6502;       //CPU object
  picCore    : TCPUCore;   //Objeto PIC Core. This is an abstraction. Real CPU is not yet specified.



procedure PutLabel(lbl: string);
procedure DefCompiler;

procedure arrayLow(var fun: TGenOperand);
procedure arrayHigh(var fun: TGenOperand);
procedure arrayLength(var fun: TGenOperand);
procedure SIF_arr_asig_arr(var fun: TGenOperand);
procedure SIF_obj_asig_obj(var fun: TGenOperand);
procedure LoadByteIndexWord(const idxvar: TMirVarDec; offset: word);
procedure LoadWordIndexWord(const idxvar: TAstVarDec; offset: word);
procedure LoadWordIndexWord2(const idxvar: TMirVarDec; offset: word);
procedure SIF_GetItemIdxByte(var fun: TGenOperand);
procedure SIF_GetItemIdxWord(var fun: TGenOperand);
procedure SetByteIndexWord(const idxvar: TMirVarDec; offset: word; parB: TGenOperand);
procedure SetWordIndexWord(const idxvar: TMirVarDec; offset: word; parB: TGenOperand);
procedure SIF_SetItemIndexByte(var fun: TGenOperand);
procedure SIF_SetItemIndexWord(var fun: TGenOperand);
function FillArray(parray: TGenOperand): boolean;
procedure SIF_ArrayClear(var fun: TGenOperand);
procedure SIF_GetPointer(var fun: TGenOperand);
procedure SIF_SetPointer(var fun: TGenOperand);
procedure SIF_word_mod_word(var fun: TGenOperand);
procedure DefineShortPointer(etyp: TAstTypeDec);
//Boolean operations
procedure SIF_bool_asig_bool(var fun: TGenOperand);
procedure SIF_bool_and_bool(var fun: TGenOperand);
procedure SIF_bool_xor_bool(var fun: TGenOperand);
procedure SIF_bool_equal_bool(var fun: TGenOperand);
procedure SIF_not_bool(var fun: TGenOperand);
//Byte operations
procedure SIF_byte_asig_byte(var fun: TGenOperand);
procedure SIF_byte_aadd_byte(var fun: TGenOperand);
procedure SIF_byte_asub_byte(var fun: TGenOperand);
procedure SIF_byte_sub_byte(var fun: TGenOperand);
procedure SIF_byte_add_byte(var fun: TGenOperand);
procedure SIF_byte_and_byte(var fun: TGenOperand);
procedure SIF_byte_or_byte(var fun: TGenOperand);
procedure SIF_byte_xor_byte(var fun: TGenOperand);
procedure SIF_byte_equal_byte(var fun: TGenOperand);
procedure SIF_byte_difer_byte(var fun: TGenOperand);
procedure SIF_byte_great_byte(var fun: TGenOperand);
procedure SIF_byte_less_byte(var fun: TGenOperand);
procedure SIF_byte_gequ_byte(var fun: TGenOperand);
procedure SIF_byte_lequ_byte(var fun: TGenOperand);
procedure SIF_byte_shr_byte(var fun: TGenOperand);
procedure SIF_byte_shl_byte(var fun: TGenOperand);
procedure SIF_byte_add_word(var fun: TGenOperand);
procedure SIF_byte_mul_byte(var fun: TGenOperand);
procedure SIF_not_byte(var fun: TGenOperand);
//Operaciones con Word
procedure SIF_word_asig_word(var fun: TGenOperand);
procedure SIF_word_asig_byte(var fun: TGenOperand);
procedure SIF_word_equal_word(var fun: TGenOperand);
procedure SIF_word_equal_byte(var fun: TGenOperand);
procedure SIF_word_difer_word(var fun: TGenOperand);
procedure SIF_word_add_byte(var fun: TGenOperand);
procedure SIF_word_add_word(var fun: TGenOperand);
procedure SIF_word_sub_byte(var fun: TGenOperand);
procedure SIF_word_sub_word(var fun: TGenOperand);
procedure SIF_word_aadd_byte(var fun: TGenOperand);
procedure SIF_word_aadd_word(var fun: TGenOperand);
procedure SIF_word_and_word(var fun: TGenOperand);
procedure SIF_word_asub_byte(var fun: TGenOperand);
procedure SIF_word_asub_word(var fun: TGenOperand);
procedure SIF_word_gequ_word(var fun: TGenOperand);
procedure SIF_word_great_word(var fun: TGenOperand);
procedure SIF_word_lequ_word(var fun: TGenOperand);
procedure SIF_word_less_word(var fun: TGenOperand);
procedure SIF_word_shl_byte(var fun: TGenOperand);
procedure SIF_word_shr_byte(var fun: TGenOperand);
procedure SIF_not_word(var fun: TGenOperand);
// Operations for DWord
procedure SIF_DWord(var fun: TGenOperand);
procedure SIF_dword_asig_dword(var fun: TGenOperand);
procedure SIF_dword_asig_byte(var fun: TGenOperand);
procedure SIF_dword_asig_word(var fun: TGenOperand);
procedure SIF_dword_add_dword(var fun: TGenOperand);
procedure SIF_dword_add_byte(var fun: TGenOperand);
procedure SIF_dword_add_word(var fun: TGenOperand);
//Operaciones con Char
procedure SIF_char_asig_char(var fun: TGenOperand);
procedure SIF_char_asig_string(var fun: TGenOperand);
procedure SIF_char_equal_char(var fun: TGenOperand);
procedure SIF_char_difer_char(var fun: TGenOperand);

//Operaciones con punteros
procedure SIF_pointer_add_byte(var fun: TGenOperand);
procedure SIF_pointer_sub_byte(var fun: TGenOperand);
procedure SIF_derefPointer(fun: TAstExpress; SetRes: boolean);
procedure SIF_pointer_add_word(var fun: TGenOperand);
procedure SIF_pointer_sub_word(var fun: TGenOperand);

procedure SIF_delay_ms(fun: TGenOperand);
procedure SIF_Inc(var fun: TGenOperand);
procedure SIF_Dec(var fun: TGenOperand);
procedure SIF_Ord(var fun: TGenOperand);
procedure SIF_Chr(var fun: TGenOperand);
procedure SIF_Word(var fun: TGenOperand);

procedure JUMP_IF_C_pre(Invert, longJump: boolean; igoto: integer);
procedure JUMP_IF_pre(OpRes: TAstExpress; boolVal, longJump: boolean;
  igoto: integer; out relatOver: boolean);
procedure JUMP_IF_Z_post(Invert, longJump: boolean; out curAddr: integer);
procedure JUMP_IF_C_post(Invert, longJump: boolean; out curAddr: integer);
procedure JUMP_IF_post(OpRes: TAstExpress; boolVal, longJump: boolean; out
  curAddr: integer);
procedure JUMP_IF_Z_pre(Invert, longJump: boolean; igoto: integer);
procedure BRA2JMP(var info: TIfInfo);

procedure GenCodeASMline(asmInst: TAstAsmInstr);
function GenCodeCodition(cond: TAstElement): TAstExpress;
procedure GenCodeExit(sen: TAstSentence);
procedure GenCodLoadToA(fun: TAstExpress);  { TODO : ¿Se necesita? No se usa }
procedure GenCodLoadToX(fun: TAstExpress);  { TODO : ¿Se necesita? No se usa }
procedure GenCodLoadToY(fun: TAstExpress);  { TODO : ¿Se necesita? No se usa }
procedure GenCondeIF(sen: TAstSentence);
procedure GenCodeWHILE(sen: TAstSentence);
procedure GenCodeFOR(sen: TAstSentence);
procedure GenCodeREPEAT(sen: TAstSentence);

procedure functCall(xfun: TMirFunDec; out AddrUndef: boolean);
procedure codRTS(isInterrupt: boolean);
procedure GenCodeExpr(eleExp: TAstExpress);

procedure IF_TRUE(OpRes: TAstExpress; longJump: boolean; out info: TIfInfo);
//    procedure IF_FALSE(OpRes: TEleExpress; out info: TIfInfo);
procedure IF_END(const info: TIfInfo; out relatOver: boolean);

//Access to CPU information
function RAMmax: integer;
function CompilerName: string;
//¿No deberían ser privados?
procedure Invert_A_to_A;
procedure Copy_Z_to_A;
procedure Invert_Z_to_A;
procedure Copy_C_to_A;
procedure Invert_C_to_A;
function Invert(fun: TGenOperand): boolean;

procedure CreateVarInRAM(xVar: TGenVarDec; shared: boolean);
procedure SetSharedUnused;
procedure SetSharedUsed;
procedure DoGenerateCode;

procedure DoGenerateHexFile(hexFile: string);

procedure SetLanguage;

implementation
var
  compMod: TCompMod;  //Mode of the compiler
  //Flags for boolean type.
  {These variables are reset in the procedures: SetFun<XXX>. They contains the state of
  the Register/Status-flags if the last UOR or BOR is executed. }
  lastASMcode : TLastASMcode;  //ASM code generated for last the UOR or BOR.
  lastASMaddr : integer;  //Memory address for the last code indicated by lastASMoper.
  AcumStatInZ : boolean;  {Indicates the Z flag contains the status of the value in A
                          register. For example if regA = 0, Z wil be 1.}

//Métodos para fijar el resultado


var
  TXT_SAVE_W, TXT_SAVE_Z, TXT_SAVE_H, MSG_NO_ENOU_RAM, MSG_VER_CMP_EXP,
  MSG_STACK_OVERF, MSG_NOT_IMPLEM, WA_UNUSED_VAR_, ER_NOT_IMPLEM_ ,
  ER_ASIG_EXPECT
  : string;
var
  MSG_INVAL_PARTYP, MSG_UNSUPPORTED : string;
  MSG_CANNOT_COMPL, MSG_IDX_BYT_WORD, ER_INV_MEMADDR, ER_INV_MAD_DEV: string;

var
  //Register work
    //Work register (WR)
    //A      : TCpuRegister;     //Registro Interno.
    //System variables used as registers
    H      : TAstVarDec;  //To load the high byte of words.
    E      : TAstVarDec;  //To load the high word of dwords.
    U      : TAstVarDec;  //To load the high word of dwords.
    IX     : TAstVarDec;  //To index operands

var
  linRep : string;       //línea para generar de reporte
  posFlash: Integer;
  lastASMLabel: string;  //Name of a label when the last instruction was a LABEL.


procedure SetLanguage;
begin
//  ParserDirec.SetLanguage;
  {$I _language\tra_GenCodBas.pas}
  {$I _language\tra_GenCod.pas}
end;

function GetAssignTarget(fun: TAstExpress; out target: TAstExpress): boolean;
var
  setFunct: TAstExpress;
begin
  setFunct := TAstExpress(fun.Parent);
  if setFunct = nil then exit(false);
  if setFunct.opType <> otFunct then exit(false);
  if setFunct.fundec.getset <> gsSetInSimple then exit(false);
  target := TAstExpress(setFunct.elements[0]);  //Parameter C := A + B
  exit(true);
end;

procedure GenError(msg: string);
{Genera un mensaje de error.}
begin
  HayError := True;   //Set flag
  if OnError<>nil then OnError(msg);
end;
procedure GenError(msg: String; const Args: array of const);
{Versión con parámetros de GenError.}
begin
  GenError(Format(msg, Args));
end;
procedure GenWarn(msg: string);
{Genera un mensaje de error.}
begin
  if OnWarning<>nil then OnWarning(msg);
end;

procedure SetFunConst_bool(fun: TGenOperand; valBool: Boolean);
begin
    fun.SetCon_Literal(valBool);
end;
procedure SetFunConst_byte(fun: TGenOperand; valByte: integer);
begin
    if (valByte <0) or (valByte>=256) then begin
      GenError('Numeric value exceeds a byte range.');
      exit;
    end;
  //  SetFunConst(fun);
  //  fun.evaluated := true;
  //  fun.value.valInt := valByte;
    fun.SetCon_Literal(valByte);
  end;
procedure SetFunConst_char(fun: TGenOperand; valByte: integer);
begin
  //  SetFunConst(fun);
  //  fun.evaluated := true;
  //  fun.value.valInt := valByte;
    fun.SetCon_Literal(valByte);
  end;
procedure SetFunConst_word(fun: TGenOperand; valWord: integer);
begin
    if (valWord <0) or (valWord>=65536) then begin
      GenError('Numeric value exceeds a word range.');
      exit;
    end;

  //  SetFunConst(fun);
  //  fun.evaluated := true;
  //  fun.value.valInt := valWord;
    fun.SetCon_Literal(valWord);
  end;
procedure SetFunConst_dword(fun: TGenOperand; valWord: DWord);
begin
    if (valWord <0) or (valWord>$FFFFFFFF) then begin
      GenError('Numeric value exceeds a dword range.');
      exit;
    end;
  //  SetFunConst(fun);
  //  fun.evaluated := true;
  //  fun.value.valInt := valWord;
    fun.SetCon_Literal(valWord);
  end;

//Codificación de instrucciones
function _PC: word; inline;
{Devuelve la dirección actual en Flash}
begin
  Result := pic.iRam;
end;
function _CLOCK: integer; inline;
{Devuelve la frecuencia de reloj del PIC}
begin
  Result := pic.frequen;
end;
procedure _LABEL_post(igoto: integer);
{Finish a previous absolute jump (JMP_post), or relative jump (BNE_post, BEQ_post, ...)
instructions.}
var
  offset: integer;
begin
  if pic.ram[igoto].value = 0 then begin
    //Es salto absoluto
    pic.ram[igoto].value   := lo(_PC);
    pic.ram[igoto+1].value := hi(_PC);
  end else begin
    //Es salto relativo
    if _PC > igoto then begin
      //Salto hacia adelante
      offset := _PC - igoto-1;
      if offset>127 then begin
        GenError('Block to long.');
        exit;
      end;
      pic.ram[igoto].value := offset;
    end else begin
      //Backward jump. Does this really happens?
      offset := _PC - igoto;  //negative
      if offset<-128 then begin
        GenError('Block to long.');
        exit;
      end;
      pic.ram[igoto].value := 256 + offset;
    end;
  end;
end;
procedure _LABEL_pre(out curAddr: integer);
{Set a label for a later jump BNE_pre, BEQ_pre, BCC_pre ... instructions.}
begin
  curAddr := pic.iRam;
end;
procedure _SELFMODw(a1, a2: integer);
begin
  pic.ram[a1].value   := (pic.iRam-2) and $FF;
  pic.ram[a1+1].value := (pic.iRam-2) >> 8;
  pic.ram[a2].value   := (pic.iRam-1) and $ff;
  pic.ram[a2+1].value := (pic.iRam-1) >> 8;
end;
procedure _SELFMODb(a1, a2: integer);
begin
  pic.ram[a1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
  pic.ram[a2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
end;

{%REGION Instrucciones simples}
procedure _ADCi(const k: word);
begin
  pic.codAsm(i_ADC, aImmediat, k);
end;
procedure _ADC(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_ADC, aZeroPage, addr);
  end else begin
    pic.codAsm(i_ADC, aAbsolute, addr);
  end;
end;
procedure _ANDi(const k: word);
begin
  pic.codAsm(i_AND, aImmediat, k);
end;
procedure _AND(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_AND, aZeroPage, addr);
  end else begin
    pic.codAsm(i_AND, aAbsolute, addr);
  end;
end;
procedure _ASL(const f: word);  //ASL Absolute/Zeropage
begin
  if f<256 then begin
    pic.codAsm(i_ASL, aZeroPage, f);
  end else begin
    pic.codAsm(i_ASL, aAbsolute, f);
  end;
end;
procedure _ASLa;
begin
  pic.codAsm(i_ASL, aAcumulat, 0);
end;
procedure _LSR(const f: word);  //LSR Absolute/Zeropage
begin
  if f<256 then begin
    pic.codAsm(i_LSR, aZeroPage, f);
  end else begin
    pic.codAsm(i_LSR, aAbsolute, f);
  end;
end;
procedure _LSRa;
begin
  pic.codAsm(i_LSR, aAcumulat, 0);
end;
procedure _JMP(const ad: word);
begin
  pic.codAsm(i_JMP, aAbsolute, ad);  //pone salto indefinido
end;
procedure _JMP_post(out igot: integer);
{Escribe una instrucción GOTO, pero sin precisar el destino aún. Devuelve la dirección
 donde se escribe el GOTO, para poder completarla posteriormente.
}
begin
  igot := pic.iRam+1;  //guarda posición de instrucción de salto
  pic.codAsm(i_JMP, aAbsolute, 0);  //1 en Offset indica que se completará con salto absoluto
end;
procedure _JSR(const ad: word);
begin
  pic.codAsm(i_JSR, aAbsolute, ad);  //1 en Offset indica que se completará con salto absoluto
end;
procedure _BEQ(const ad: ShortInt);
begin
  if ad>=0 then begin
    pic.codAsm(i_BEQ, aRelative, ad);
  end else begin
    pic.codAsm(i_BEQ, aRelative, 256+ad);
  end;
end;
procedure _BEQ_post(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BEQ, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure _BNE(const ad: ShortInt);
begin
  if ad>=0 then begin
    pic.codAsm(i_BNE, aRelative, ad);
  end else begin
    pic.codAsm(i_BNE, aRelative, 256+ad);
  end;
end;
procedure _BNE_post(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BNE, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure _BNE_pre(curAddr: integer);
begin
  pic.codAsm(i_BNE, aRelative, (curAddr - pic.iRam-2) and $ff);
end;
procedure _BCC(const ad: ShortInt);
begin
  if ad>=0 then begin
    pic.codAsm(i_BCC, aRelative, ad);
  end else begin
    pic.codAsm(i_BCC, aRelative, 256+ad);
  end;
end;
procedure _BCC_post(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BCC, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure _BCS(const ad: ShortInt);
begin
  if ad>=0 then begin
    pic.codAsm(i_BCS, aRelative, ad);
  end else begin
    pic.codAsm(i_BCS, aRelative, 256+ad);
  end;
end;
procedure _BCS_post(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BCS, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure _BLT_post(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BCC, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure _BGE_post(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BCS, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure _BPL(const ad: ShortInt);
begin
  if ad>=0 then begin
    pic.codAsm(i_BPL, aRelative, ad);
  end else begin
    pic.codAsm(i_BPL, aRelative, 256+ad);
  end;
end;
procedure _BPL_pre(curAddr: integer);
begin
  pic.codAsm(i_BPL, aRelative, (curAddr - pic.iRam-2) and $ff);
end;
procedure _BPL_post(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BPL, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure _CLC;
begin
  pic.codAsm(i_CLC, aImplicit, 0);
end;
procedure _CMPi(const k: word);
begin
  pic.codAsm(i_CMP, aImmediat, k);
end;
procedure _CMP(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_CMP, aZeroPage, addr);
  end else begin
    pic.codAsm(i_CMP, aAbsolute, addr);
  end;
end;
procedure _CPYi(const k: word);
begin
  pic.codAsm(i_CPY, aImmediat, k);
end;
procedure _CPY(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_CPY, aZeroPage, addr);
  end else begin
    pic.codAsm(i_CPY, aAbsolute, addr);
  end;
end;
procedure _DEX;
begin
  pic.codAsm(i_DEX, aImplicit, 0);
end;
procedure _DEY;
begin
  pic.codAsm(i_DEY, aImplicit, 0);
end;
procedure _DEC(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_DEC, aZeroPage, addr);
  end else begin
    pic.codAsm(i_DEC, aAbsolute, addr);
  end;
end;
procedure _EOR(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_EOR, aZeroPage, addr);
  end else begin
    pic.codAsm(i_EOR, aAbsolute, addr);
  end;
end;
procedure _EORi(const k: word);
begin
  pic.codAsm(i_EOR, aImmediat, k);
end;
procedure _INC;
begin
  pic.codAsm(i_INC, aImplicit, 0);
end;
procedure _INC(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_INC, aZeroPage, addr);
  end else begin
    pic.codAsm(i_INC, aAbsolute, addr);
  end;
end;
procedure _INX;
begin
  pic.codAsm(i_INX, aImplicit, 0);
end;
procedure _INY;
begin
  pic.codAsm(i_INY, aImplicit, 0);
end;
procedure _LDAi(const k: word);  //LDA Immediate
begin
  pic.codAsm(i_LDA, aImmediat, k);
end;
procedure _LDA(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_LDA, aZeroPage, addr);
  end else begin
    pic.codAsm(i_LDA, aAbsolute, addr);
  end;
end;
procedure _LDAx(const addr: word);
{Generate the LDA with addressing aZeroPagX or aAbsolutX}
begin
  if addr<256 then begin
    pic.codAsm(i_LDA, aZeroPagX, addr);
  end else begin
    pic.codAsm(i_LDA, aAbsolutX, addr);
  end;
end;
procedure _LDAin(const addr: word);
begin
  pic.codAsm(i_LDA, aIndirecZP, addr);
end;
procedure _LDAinx(const addr: word);
begin
  pic.codAsm(i_LDA, aIndirecX, addr);
end;
procedure _LDAiny(const addr: word);
begin
  pic.codAsm(i_LDA, aIndirecY, addr);
end;
procedure _LDXi(const k: word); inline;  //LDA Immediate
begin
  pic.codAsm(i_LDX, aImmediat, k);
end;
procedure _LDX(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_LDX, aZeroPage, addr);
  end else begin
    pic.codAsm(i_LDX, aAbsolute, addr);
  end;
end;
procedure _LDYi(const k: word); inline;  //LDA Immediate
begin
  pic.codAsm(i_LDY, aImmediat, k);
end;
procedure _LDY(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_LDY, aZeroPage, addr);
  end else begin
    pic.codAsm(i_LDY, aAbsolute, addr);
  end;
end;
procedure _NOP; inline;
begin
  pic.codAsm(i_NOP, aImplicit, 0);
end;
procedure _ORAi(const k: word);
begin
  pic.codAsm(i_ORA, aImmediat, k);
end;
procedure _ORA(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_ORA, aZeroPage, addr);
  end else begin
    pic.codAsm(i_ORA, aAbsolute, addr);
  end;
end;
procedure _PHA; inline;
begin
  pic.codAsm(i_PHA, aImplicit, 0);
end;
procedure _PHP;
begin
  pic.codAsm(i_PHP, aImplicit, 0);
end;
procedure _PLA;
begin
  pic.codAsm(i_PLA, aImplicit, 0);
end;
procedure _PLP;
begin
  pic.codAsm(i_PLP, aImplicit, 0);
end;
procedure _ROLa;
begin
  pic.codAsm(i_ROL, aAcumulat, 0);
end;
procedure _ROL(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_ROL, aZeroPage, addr);
  end else begin
    pic.codAsm(i_ROL, aAbsolute, addr);
  end;
end;
procedure _RORa;
begin
  pic.codAsm(i_ROR, aAcumulat, 0);
end;
procedure _ROR(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_ROR, aZeroPage, addr);
  end else begin
    pic.codAsm(i_ROR, aAbsolute, addr);
  end;
end;
procedure _RTS; inline;
begin
  pic.codAsm(i_RTS, aImplicit, 0);
end;
procedure _RTI; inline;
begin
  pic.codAsm(i_RTI, aImplicit, 0);
end;
procedure _SEC; inline;
begin
  pic.codAsm(i_SEC, aImplicit, 0);
end;
procedure _SED; inline;
begin
  pic.codAsm(i_SED, aImplicit, 0);
end;
procedure _SBCi(const k: word); inline;  //SBC Immediate
begin
  pic.codAsm(i_SBC, aImmediat, k);
end;
procedure _SBC(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_SBC, aZeroPage, addr);
  end else begin
    pic.codAsm(i_SBC, aAbsolute, addr);
  end;
end;
procedure _STA(addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_STA, aZeroPage, addr);
  end else begin
    pic.codAsm(i_STA, aAbsolute, addr);
  end;
end;
procedure _STAx(addr: integer; forceAbsolute: boolean = false);
begin
  if forceAbsolute then begin
    pic.codAsm(i_STA, aAbsolutX, addr);
  end else begin
    if addr<256 then begin
      pic.codAsm(i_STA, aZeroPagX, addr);
    end else begin
      pic.codAsm(i_STA, aAbsolutX, addr);
    end;
  end;
end;
procedure _STAin(const addr: integer);
begin
  pic.codAsm(i_STA, aIndirecZP, addr);
end;
procedure _STAinx(const addr: integer);
begin
  pic.codAsm(i_STA, aIndirecX, addr);
end;
procedure _STAiny(const addr: integer);
begin
  pic.codAsm(i_STA, aIndirecY, addr);
end;
procedure _STX(const addr: integer);  //STA Absolute/Zeropage
begin
  if addr<256 then begin
    pic.codAsm(i_STX, aZeroPage, addr);
  end else begin
    pic.codAsm(i_STX, aAbsolute, addr);
  end;
end;
procedure _STY(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_STY, aZeroPage, addr);
  end else begin
    pic.codAsm(i_STY, aAbsolute, addr);
  end;
end;
procedure _TAX;
begin
  pic.codAsm(i_TAX, aImplicit, 0);
end;
procedure _TAX_opt;
{TAX version that delete the possible sequence TXA-TAX}
var
  ramcell : ^TCPURamCell;
begin
  ramcell := @pic.ram[pic.iRam-1];
  if opt.RemUnOpcod and (ramcell^.used = ruCodeOp) and (ramcell^.value = $8A) then begin
    //We have a TXA before.
    pic.iRam := pic.iRam-1;
  end else begin
    _TAX;          //Save A in X
  end;
end;
procedure _TAY;
begin
  pic.codAsm(i_TAY, aImplicit, 0);
end;
procedure _TYA;
begin
  pic.codAsm(i_TYA, aImplicit, 0);
end;
procedure _TXA;
begin
  pic.codAsm(i_TXA, aImplicit, 0);
end;
procedure _STZ(addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_STZ, aZeroPage, addr);
  end else begin
    pic.codAsm(i_STZ, aAbsolute, addr);
  end;
end;
procedure _STZx(addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_STZ, aZeroPagX, addr);
  end else begin
      pic.codAsm(i_STZ, aAbsolutX, addr);
  end;
end;
{%ENDREGION}

procedure SetOnLoadToWR(typ: TAstTypeDec; OnLoadToWR: TProcLoadOperand);
{Procedimiento que, por seguridad, debería ser el único acceso a TAstTypeDec.OnLoadToWR.
Así se garantiza que el "casting" se haga apropiadamente.}
begin
  typ.OnLoadToWR := TMethod(OnLoadToWR);
end;


//Memory managing routines for variables.
procedure WriteVaLueToRAM(target: PtrTCPURam; add: word; typ: TGenTypeDec;
  const value: TGenConsValue);
//Write a constant value, of any type, to a some position in the RAM.
var
  i: Integer;
begin
  if typ.catType = tctAtomic then begin
    if typ = typByte then begin
      target^[add].value := value.ValInt and $ff;
    end else if typ = typChar then begin
      target^[add].value := value.ValInt and $ff;
    end else if typ = typBool then begin
      if value.ValBool then target^[add].value := 1
      else target^[add].value := 0;
    end else if typ = typWord then begin
      target^[add].value := value.ValInt and $ff;
      target^[add+1].value := (value.ValInt >> 8) and $ff;
    end else begin
      GenError(MSG_NOT_IMPLEM);
    end;
  end else if typ.catType = tctArray then begin
    //Composite type
    for i:=0 to high(value.items) do begin
      WriteVaLueToRAM(target, add, typ.itmType, value.items[i]);  //Recursion
      if HayError then exit;
      inc(add, typ.itmType.size);
    end;
  end else if typ.catType = tctPointer then begin
    //Pointer are as words
    target^[add].value := value.ValInt and $ff;
    target^[add+1].value := (value.ValInt >> 8) and $ff;
  end else begin
    GenError(MSG_NOT_IMPLEM);
  end;
end;
procedure CreateVarInRAM(xVar: TGenVarDec; shared: boolean);
{Assign physical location in RAM to a variable (If it's not defined as REGISTER).
Variables are created starting at the "GeneralORG" compiler option position.
Variables are created in Free RAM location, except if they are ABSOLUTE. }
var
  varName: String;
  nbytes: integer;
  typ: TGenTypeDec;
  startAdd: word;
  i: integer;
  in_DATA_ADDR: boolean;  //Variable located in DATA_ADDR section.
  outOfProgram: Boolean;
begin
  //Validation
  if xVar.adicPar.hasAdic in [decRegis, decRegisA, decRegisX, decRegisY] then begin
    //Register variables don't use RAM.
    exit;
  end;
  varName := xVar.text;
  typ := xVar.typ;
  //Get the size of the variable
  nbytes := typ.size;
  //Find the memory address where to place the variable.
  pic.freeStart := opt.GeneralORG;  //Find at the current program block.
  in_DATA_ADDR := false;
  if xVar.adicPar.hasAdic = decAbsol then begin
    //It's ABSOLUTE to something
    {*** Por revisar
    if xVar.adicPar.absVar<>nil then begin
      //ABSOLUTE to a variable.
      startAdd := xVar.adicPar.absVar.addr;  //Se supone que "xVar.adicPar.absVar" ya está mapeada en RAM.
    end else begin
      //ABSOLUTE to a fixed address.
      startAdd := xVar.adicPar.absAddr;
    end;
    }
  end else if xVar.adicPar.hasAdic = decZeroP then begin
    //Required to locate in Zero page.
    if not pic.GetFreeBytes(nbytes, 0, 255, startAdd) then begin
      GenError('Not free bytes in Zero page to allocate: ' + xVar.text);
      exit;
    end;
  end else if xVar.adicPar.hasAdic = decDatSec then begin
    //Required to locate in the Data section.
    if not pic.GetFreeBytes(nbytes, opt.GeneralORG, pic.cpuMAXRAM-1, startAdd) then begin
      GenError('Not free bytes in Data section to allocate: ' + xVar.text);
      exit;
    end;
  end else begin
    //Compiler decides where to locate.
    //First search in the Data zone, defined by {$SET_DATA_ADDR}
    if (pic.dataAddr1<>-1) and pic.GetFreeBytes(nbytes, pic.dataAddr1, pic.dataAddr2, startAdd) then begin
      //OK. We found a free zone here.
      in_DATA_ADDR := true;
    end else begin
      //Lets try in the Normal Data section
      if not pic.GetFreeBytes(nbytes, opt.GeneralORG, pic.CPUMAXRAM-1, startAdd) then begin
        GenError(MSG_NO_ENOU_RAM);
        exit;
      end;
    end;
  end;
  xVar.addr:=startAdd;  //Set address
  xVar.allocated := true;
  //Detect if variable location is out of the code block.
  //We assume absolute variables are out of code to protect from initialization
  {The problem is in the *.PRG format we use for output, doesn't allow to specify
  separates blocks of memory to fill. For example if we have specified an address like
  $FFFF for an absolute variable, and the program start at $0000, all the RAM must be
  included in *.PRG.}
  outOfProgram := (xVar.adicPar.hasAdic in [decAbsol, decZeroP]) or
                  in_DATA_ADDR;  //The variable has been placed in the primary data address.
  //Mark as used as variable Data. Not instruction.
  if outOfProgram then begin
    //Out of the program block, mark as "ruAbsData", in order to not be considered
    //to generate the PRG file.
    if startAdd+nbytes-1>high(pic.ram) then begin
      GenError('Cannot allocate variable: %s', [varName]);
      exit;
    end;
    for i:=startAdd to startAdd+nbytes-1 do begin
      pic.ram[i].used := ruAbsData;
      if shared then begin
        pic.ram[i].shared := true;  //Marca como compartido
      end;
    end;
  end else begin
    //In the program block
    for i:=startAdd to startAdd+nbytes-1 do begin
      pic.ram[i].used := ruData;
      if shared then begin
        pic.ram[i].shared := true;  //Marca como compartido
      end;
    end;
  end;
  //Set name to that position
  if typ.IsByteSize then begin
    pic.SetNameRAM(startAdd, xVar.text);
  end else if typ.IsWordSize then begin
    pic.SetNameRAM(startAdd, xVar.text + '@0');
    pic.SetNameRAM(startAdd+1, xVar.text + '@1');
  end else begin
    pic.SetNameRAM(startAdd, xVar.text);
  end;
  //Set initial value.
  if xVar.adicPar.hasInit<>nil then begin
    if outOfProgram then begin  //Only allowed in the program block
      GenError('Cannot initialize variable "%s" in location $%x.',
                       [varName, startAdd]);
    end;
    //Here, we need to know the type
    WriteVaLueToRAM(@pic.ram, startAdd, typ, xVar.inival);
    if HayError then  exit;
  end;
end;
procedure CreateValueInCode(typ: TGenTypeDec;
  const value: TGenConsValue; out startAddr: integer);
{Write a constant value in RAM, in the current code section, adding the correspondent JMP
instruction. Returns in "startAddr", the address where start the value.}
var
  j1, i: integer;
  nbytes: SmallInt;
begin
  nbytes := typ.size;
  _JMP_post(j1);   //Salto hasta después del espacio de variables
  startAddr := pic.iRam;
  WriteVaLueToRAM(@pic.ram, pic.iRam, typ, value);
  for i:=pic.iRam to pic.iRam+nbytes-1 do begin
    pic.ram[i].used := ruData;
  end;
  if (typ.catType = tctArray) and (typ.itmType = typChar) and opt.str_nullterm then begin
    //Special case. Literal arrays of chars (strings) with Null character
    pic.ram[pic.iRam+nbytes].used := ruData;
    pic.ram[pic.iRam+nbytes].value := 0;
    inc(nBytes);
  end;
  inc(pic.iRam, nBytes);  //Move pointer.
_LABEL_post(j1);   //Termina de codificar el salto
end;

procedure expr_start;
//Se ejecuta siempre al iniciar el procesamiento de una expresión.
begin
  //Inicia banderas de estado para empezar a calcular una expresión
  //A.used := false;        //Su ciclo de vida es de instrucción
  //Guarda información de ubicación, en la ubicación actual
//  pic.addPosInformation(curCtx.row, curCtx.col, curCtx.idCtx);
end;
procedure expr_end({posExpres: TPosExpres});
//Se ejecuta al final de una expresión, si es que no ha habido error.
begin
//  if exprLevel = 1 then begin  //el último nivel
////    Code('  ;fin expres');
//  end;
//  //Muestra informa
end;
procedure SIF_address(fun: TMirOperand);
{Return the address of any operand.}
var
  startAddr: integer;
  par: TMirOperand;
begin
//  par := (fun.elements[0]);  //Only one parameter
//  //Process special modes of the compiler.
//  if compMod = cmConsEval then begin
//    //*** Ver si es necesario Completar
//    exit;
//  end;
//  //Code generation
//  case par.Sto of
//  stConst : begin
//    if par.Typ.catType = tctArray then begin
//      //We allow to get the address for constant arrays, storing first in RAM.
//      if pic.disableCodegen then begin
//        //Cannot generate code
//        SetFunExpres(fun); //Still as a function
//      end else begin
//        CreateValueInCode(par.Typ, par.Value, startAddr);
//        SetFunConst(fun);
//        fun.value.ValInt := startAddr;
//        fun.value.consType := ctLiteral; // .evaluated := true;
//      end;
//    end else begin
//      genError('Cannot obtain address of constant.');
//    end;
//  end;
//  stRamFix: begin
//    //Es una variable normal
//    //La dirección de una variable es constante
//    if par.vardec.allocated then begin
//      SetFunConst(fun);
//      fun.value.valInt := par.vardec.addr;
//      fun.evaluated := par.vardec.allocated;
//    end else begin
//      SetFunExpres(fun); //Still as a function
//    end;
//  end;
//  stRegister: begin
//    genError('Cannot obtain address of an expression.');
//  end;
//  else
//    genError('Cannot obtain address of this operand.');
//  end;
end;

procedure ValidRAMaddr(addr: integer);
{Validate a physical RAM address. If error generate error.}
begin
  if (addr<0) or (addr>$ffff) then begin
    //Debe set Word
    GenError(ER_INV_MEMADDR);
    exit;
  end;
  if not pic.ValidRAMaddr(addr) then begin
    GenError(ER_INV_MAD_DEV);
    exit;
  end;
end;

procedure DefCompiler;
{}
begin
//  //Define métodos a usar
//  OnExprStart := @expr_start;
//  OnExprEnd   := @expr_End;
//
end;

//Operations for parameters or Binary Operators
function BinOperationStr(fun: TGenOperand): string;
{Returns a string representing a binary operation.}
var
  parA, parB: TGenOperand;
  Oper: String;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  Oper := IfThen(fun.fundec.operTyp = opkBinary, fun.fundec.oper, fun.name);
  Result := parA.StoAsStr+'(' + parA.Typ.name + ') ' + Oper + ' ' +
            parB.StoAsStr+'(' + parB.Typ.name + ')';
end;
function stoOperation(parA, parB: TGenOperand): TStoOperandsBSIF;
begin
  //Combinación de los almacenamientos de los operandos
  Result := TStoOperandsBSIF((Ord(parA.Sto) << 4) or ord(parB.Sto));
end;
procedure Exchange(var parA, parB: TGenOperand);
{Intercambia el orden de los operandos.}
var
  tmp: TGenOperand;
begin
  //Invierte los operandos
  tmp := parA;
  parA := parB;
  parB := tmp;
end;

procedure ProcByteUsed(offs: word; regPtr: TCPURamCellPtr);
begin
  linRep := linRep + regPtr^.name +
            ' DB ' + '$' + IntToHex(offs, 3) + LineEnding;
end;
procedure SetSharedUnused;
begin
  pic.SetSharedUnused;
end;
procedure SetSharedUsed;
begin
  pic.SetSharedUsed;
end;
procedure word_ClearItems(const OpPtr: pointer);
begin

end;
procedure PutLabel(lbl: string);
{Agrega uan etiqueta antes de la instrucción. Se recomienda incluir solo el nombre de
la etiqueta, sin ":", ni comentarios, porque este campo se usará para desensamblar.}
begin
//  pic.addTopLabel(lbl);  //agrega línea al código ensmblador
  pic.ram[pic.iRam].name := lbl;  //Add as cell name
end;
procedure PutTopComm(cmt: string; replace: boolean = true);
//Agrega comentario al inicio de la posición de memoria
begin
  pic.addTopComm(cmt, replace);  //agrega línea al código ensmblador
end;
procedure PutComm(cmt: string);
//Agrega comentario lateral al código. Se llama después de poner la instrucción.
begin
  pic.addSideComm(cmt, true);  //agrega línea al código ensmblador
end;
procedure PutFwdComm(cmt: string);
//Agrega comentario lateral al código. Se llama antes de poner la instrucción.
begin
  pic.addSideComm(cmt, false);  //agrega línea al código ensmblador
end;
function ReportRAMusage: string;
{Genera un reporte de uso de la memoria RAM}
begin
  linRep := '';
  pic.ExploreUsed(@ProcByteUsed);
  Result := linRep;
end;
function ValidateByteRange(n: integer): boolean;
//Verifica que un valor entero, se pueda convertir a byte. Si no, devuelve FALSE.
begin
  if (n>=0) and (n<256) then
     exit(true)
  else begin
    GenError('Numeric value exceeds a byte range.');
    exit(false);
  end;
end;
function ValidateWordRange(n: integer): boolean;
//Verifica que un valor entero, se pueda convertir a byte. Si no, devuelve FALSE.
begin
  if (n>=0) and (n<65536) then
     exit(true)
  else begin
    GenError('Numeric value exceeds a word range.');
    exit(false);
  end;
end;
function ValidateDWordRange(n: Int64): boolean;
begin
  if (n>=0) and (n<$100000000) then
     exit(true)
  else begin
    GenError('Numeric value exceeds a dword range.');
    exit(false);
  end;
end;
procedure LoadToWR(const fun: TGenOperand);
{Carga un operando a los Registros de Trabajo (WR).}
var
  OnLoadToWR: TProcLoadOperand;
begin
  //Reconstruye el tipo de evento.
  OnLoadToWR := TProcLoadOperand(fun.Typ.OnLoadToWR);
  //Llama al evento.
  if OnLoadToWR=nil then begin
    //No implementado
    GenError(ER_NOT_IMPLEM_, ['LoadToRT']);
  end else begin
    OnLoadToWR(fun);
  end;
end;

procedure StopCodeGen;
{Required Stop the Code generation}
begin
  posFlash := pic.iRam; //Probably not the best way.
end;
procedure StartCodeGen;
{Required Start the Code generation}
begin
  pic.iRam := posFlash; //Probably not the best way.
end;

function requireA: boolean;
begin
  //if ModeRequire then a.used := True;
  exit(true);   //Always available
end;

///////////// System functions
procedure codif_1mseg;
//Codifica rutina de retardo de 1mseg.
var
  nCyc1m: word;
  i: Integer;
begin
  PutFwdComm(';1 msec routine.');
  nCyc1m := round(_CLOCK/1000);  //Número de ciclos necesarios para 1 mseg
  if nCyc1m < 10 then begin
    //Tiempo muy pequeño, se genera con NOP
    for i:=1 to nCyc1m div 2 do begin
      _NOP;
    end;
  end else if nCyc1m < 1275 then begin
    //Se puede lograr con bucles de 5 ciclos
    //Lazo de 5 ciclos por vuelta
    _LDXi(nCyc1m div 5);  //2 cycles
  //delay:
    _DEX;       //2 cycles (1 byte)
    _BNE(-3);   //3 cycles in loop (in same page), 2 cycles at end (2 bytes)
  end else begin
    GenError('Clock frequency %d not supported for delay_ms().', [_CLOCK]);
  end;
end;
procedure SNF_delay_ms(fun: TAstFunBase);
//Codifica rutina de retardo en milisegundos
var
  delay: Word;
  LABEL1, ZERO: integer;
begin
  PutLabel('__delay_ms');
  {Esta rutina recibe los milisegundos en los registros en (H,A) o en (A)
  En cualquier caso, siempre usa el registros H , el acumulador "A" y un reg. auxiliar.
  Se supone que para pasar los parámetros, ya se requirió H, así que no es necesario
  crearlo.}
//  _LDXi(0);     PutComm(' ;enter when parameters in (0,A)');
//  _STX(H);
//  fun.adrr2 := pic.iRam;  {Se hace justo antes de generar código por si se crea
//                          la variable _H}
  _TAY; //PutComm(';enter when parameters in (H,A)');
  //Se tiene el número en H,Y
delay:= _PC;
  _TYA;
  _BNE_post(LABEL1);  //label
  //A (and Y) is zero
  _LDA(H.addr);
  _BEQ_post(ZERO); //H is zero too (not decremented in that case)
  _DEC(H.addr);
_LABEL_post(LABEL1);
  _DEY;
  codif_1mseg;   //codifica retardo 1 mseg
  if HayError then exit;
  _JMP(delay);
_LABEL_post(ZERO);
  _RTS();
end;


procedure SetFunNull(var fun: TGenOperand);
{Fija el resultado como NULL.}
begin
  fun.Typ := typNull;
  fun.Sto := stNone;
  lastASMcode := lacNone;
  AcumStatInZ := true;
end;
procedure SetFunConst(var fun: TGenOperand);
{Fija los parámetros del resultado de una subexpresion. Este método se debe ejecutar,
siempre antes de evaluar cada subexpresión.}
begin
  fun.opType := otConst;
  fun.Sto := stConst;  //La única opción es esta.
  lastASMcode := lacNone;
  AcumStatInZ := true;
end;
procedure SetFunVariab(var fun: TGenOperand; vardec: TMirVarDec);
{Set an operand TxpEleExpress to type otVariab and storage stRamFix.}
begin
  fun.SetVar_RamFix(vardec);
  lastASMcode := lacNone;
  AcumStatInZ := true;   //Default TRUE is explained in Documentation.
end;
procedure SetFunVariab(var fun: TGenOperand; vardec: TAstVarDec);
{Set an operand TxpEleExpress to type otVariab and storage stRamFix.
*** Tal vez esta versión de SetFunVariab no deba existir.}
begin
  fun.SetVar_RamFix(TMirVarDec(vardec.mirVarDec));
  lastASMcode := lacNone;
  AcumStatInZ := true;   //Default TRUE is explained in Documentation.
end;
procedure SetFunVariab(var fun: TGenOperand; addr: word);
{Fija los parámetros del resultado de una subexpresion. Este método se debe ejecutar,
siempre antes de evaluar cada subexpresión.}
begin
  fun.SetVar_RamFix(addr);
  lastASMcode := lacNone;
  AcumStatInZ := true;   //Default TRUE is explained in Documentation.
end;
procedure SetFunVariab_RamVarOf(var fun: TGenOperand; vardec: TAstVarDec;
  offset: integer; offsetVar: TAstVarDec);
{Set an operand TxpEleExpress to type otVariab and storage stRamVarOf.}
begin
//  SetVariabCVA(fun, offsetVar, vardec);
  fun.SetVar_RamVarOf(TMirVarDec(offsetVar.mirVarDec), TMirVarDec(vardec.mirVarDec));
  lastASMcode := lacNone;
  AcumStatInZ := true;   //Default TRUE is explained in Documentation.
end;
procedure SetFunExpres(fun: TGenOperand);
{Fija los parámetros del resultado de una subexpresion. Este método se debe
ejecutar, siempre antes de evaluar cada subexpresión.}
begin
  fun.opType := otFunct; //Fija como expresión
  fun.Sto := stRegister; //Almacenamiento por defecto

  //Limpia el estado. Esto es útil que se haga antes de generar el código para una operación
  lastASMcode := lacNone;
  AcumStatInZ := true;
end;

{%REGION Routines for arrays and pointers}
procedure arrayLow(var fun: TGenOperand);
//Devuelve el índice mínimo de un arreglo
var
  par: TGenOperand;
begin
  par := (fun.elements[0]);  //Only one parameter
  SetFunConst(fun);
  fun.value.ValInt := 0;
end;
procedure arrayHigh(var fun: TGenOperand);
//Devuelve el índice máximo de un arreglo
var
  par: TGenOperand;
begin
  par := (fun.elements[0]);  //Only one parameter
  SetFunConst(fun);
  fun.value.ValInt := par.Typ.nItems-1;
end;
procedure arrayLength(var fun: TGenOperand);
//Devuelve la cantidad de elementos de un arreglo
var
  par: TGenOperand;
begin
  par := (fun.elements[0]);  //Only one parameter
  SetFunConst(fun);
  fun.value.ValInt := par.Typ.nItems;
end;
procedure SIF_arr_asig_arr(var fun: TGenOperand);
{Array assigment.}
var
  nItems, itSize, i: Integer;
  nBytes, des: Integer;
  itType: TMirTypDec;
  src: Word;
  //tmpvar: TAstVarDec;
  values: array of TConsValue;
//  opr1: TxpOperator;
  startAddr, j2: integer;
  parA, parB: TGenOperand;
  buffer: TCPURam;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  if parA.Typ.nItems <> parB.Typ.nItems then begin
    GenError('Array sizes doesn''t match.');
    exit;
  end;
  if parA.Sto = stRamFix then begin
    nItems := parA.Typ.nItems;
    nBytes := parA.vardec.typ.size;
    itType := parA.vardec.typ.itmType;
    itSize := itType.size;
    case parB.Sto of
    stConst: begin
      if nBytes < 5 then begin
        setlength(buffer, 5);  //Temporal space for constant.
        //Just a little bytes
        WriteVaLueToRAM(@buffer, 0, parA.Typ, parB.value);
        //values := parB.Value.items;
        for i:=0 to nBytes-1 do begin
          _LDAi(buffer[i].value);
          _STA(parA.add+i);
        end;
      end else if nBytes< 256 then begin
        //Several ítems, we first write Op2 in RAM.
        CreateValueInCode(parB.Typ, parB.Value, startAddr);
        //Now we have Op2 created in RAM. Lets move.
        _LDXi(nBytes);
_LABEL_pre(j2);
        _LDAx((startAddr-1) and $FFFF);  //Fix address to fit the index loop
        _STAx((parA.vardec.addr-1) and $FFFF);  //Fix address to fit the index loop
        _DEX;
        _BNE_pre(j2);
      end else begin
        GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
      end;
    end;
    stRamFix: begin
      if nBytes < 5 then begin
        des:=parA.vardec.addr;
        for src:=parB.vardec.addr to parB.vardec.addr+nBytes-1 do begin
          _LDA(src);
          _STA(des);
          inc(des);
        end;
      end else if nBytes< 256 then begin
        //Several ítems, we will use a loop to copy.
        //Now we have the variable created in RAM. Lets move
        _LDXi(nBytes);
_LABEL_pre(j2);
        _LDAx((parB.vardec.addr-1) and $FFFF);  //Fix address to fit the index loop
        _STAx((parA.vardec.addr-1) and $FFFF);  //Fix address to fit the index loop
        _DEX;
        _BNE_pre(j2);
      end else begin
        GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
      end;
    end;
//    stRegister: begin   //se asume que está en A
//      SetResultExpres(fun);  //Realmente, el resultado no es importante
//      _STA(parA.addL);
//      _LDA(0);
//      _STA(parA.addH);
//    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else begin
    GenError('Cannot assign to this Operand.');
    exit;
  end;
end;
procedure SIF_obj_asig_obj(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
  nBytes: SmallInt;
  des, src: Word;
  j2: integer;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  if parA.Sto = stRamFix then begin
    nBytes := parA.vardec.typ.size;
    case parB.Sto of
//    stConst: begin
//    end;
    stRamFix: begin
      if nBytes < 5 then begin
        des:=parA.vardec.addr;
        for src:=parB.vardec.addr to parB.vardec.addr+nBytes-1 do begin
          _LDA(src);
          _STA(des);
          inc(des);
        end;
      end else if nBytes< 256 then begin
        //Several ítems, we will use a loop to copy.
        //Now we have the variable created in RAM. Lets move
        _LDXi(nBytes);
_LABEL_pre(j2);
        _LDAx((parB.vardec.addr-1) and $FFFF);  //Fix address to fit the index loop
        _STAx((parA.vardec.addr-1) and $FFFF);  //Fix address to fit the index loop
        _DEX;
        _BNE_pre(j2);
      end else begin
        GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
      end;
    end;
//    stRegister: begin   //se asume que está en A
//    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else begin
    GenError('Cannot assign to this Operand.');
    exit;
  end;
end;
procedure LoadByteIndexWord(const idxvar: TMirVarDec; offset: word);
{Load in A register, the value indexed by "idxvar" variable and added
by "offset". Parameter "idxvar" must by word-size.
NOTE: Doesn't use IX register. We prefer Self-modifying code.}
var
  ad1, ad2: Integer;
begin
  if idxvar.typ.size>2 then begin
    GenError('Not supported this index or pointer type.');
    exit;
  end;
  if (idxvar.addr<256) and (offset<256) then begin
    //Special case
    _LDYi(offset);    //Could be zero.
    pic.codAsm(i_LDA, aIndirecY, idxvar.addr);
  end else if offset=0 then begin
    //Self-modifying.
    if pic.iRam < 256-13 then begin  //Everything can be done on zero-page.
      _LDA(idxvar.addrL);
      _STA($FF); ad1:=pic.iRam-1;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _STA($FF); ad2:=pic.iRam-1;  //Save address.
      _LDA($FFFF);     //Load byte
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
    end else begin
      _LDA(idxvar.addrL);
      _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
      _LDA($FFFF);     //Load byte
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    end;
  end else if offset<256 then begin   //Byte offset
    if pic.iRam < 256-15 then begin  //Everything can be done on zero-page.
      _LDA(idxvar.addrL);
      _STA($FF); ad1:=pic.iRam-1;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _STA($FF); ad2:=pic.iRam-1;  //Save address.
      _LDYi(offset);
      pic.codAsm(i_LDA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
    end else begin
      _LDA(idxvar.addrL);
      _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
      _LDYi(offset);
      pic.codAsm(i_LDA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    end;
  end else begin
    //Self-modifying.
    if pic.iRam < 256-18 then begin  //Everything can be done on zero-page.
      _CLC;
      _LDA(idxvar.addrL);
      _ADCi(lo(offset));
      _STA($FF); ad1:=pic.iRam-1;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _ADCi(hi(offset));
      _STA($FF); ad2:=pic.iRam-1;  //Save address.
      _LDA($FFFF);     //Load byte
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
    end else begin
      _CLC;
      _LDA(idxvar.addrL);
      _ADCi(lo(offset));
      _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _ADCi(hi(offset));
      _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
      _LDA($FFFF);     //Load byte
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    end;
  end;
end;
procedure LoadWordIndexWord(const idxvar: TAstVarDec; offset: word);
{Load in H,A register, the value indexed by "idxVar" variable multiplied by 2 and
added by "offset". Parameter "idxVar" must by word-size.
IMPORTANT: Require IX defined and stored at zero-page
**** Not tested ****. We prefer to use LoadWordIndexWord2() because it's tested and well
optimized although it doesn't use IX register.}
begin
  if idxvar.addr<256 then begin  //*** Good Luck. Index is in Zero-page
    if offset<255 then begin  //Less than 255 because it will be incremeneted
      //Copy in IX.addr+1
      _LDA(idxvar.addr+1);
      _STA(IX.addr+1);
      //Multiply by 2 and Update IX.addr
      _LDA(idxvar.addr);
      _ASLa;
      _STA(IX.addr);
      _ROL(IX.addr+1);
      //Load LSB
      _LDYi(offset);    //Could be zero.
      pic.codAsm(i_LDA, aIndirecY, IX.addr);
    end else begin
      //Load in WR
      _LDA(idxvar.addr);    //LSB
      _LDX(idxvar.addr+1);  //MSB
      _STX(H.addr);  //Could be optimized for offset=0 if using IX.addr+1 instead of H.addr.
      //Multiply by 2
      _ASLa;
      _ROL(H.addr);
      //Add offset and store in IX
      _CLC;
      _ADCi(lo(offset));
      _STA(IX.addr);
      _LDA(H.addr);
      _ADCi(hi(offset));
      _STA(IX.addr+1);
      //Load LSB
      _LDYi(0);
      pic.codAsm(i_LDA, aIndirecY, IX.addr);
    end;
  end else begin                 //*** Bad. Index is in other page.
    //Similar to case "idxvar.addr<256".
    //Load in WR
    _LDA(idxvar.addr);    //LSB
    _LDX(idxvar.addr+1);  //MSB
    _STX(H.addr);  //Could be optimized for offset=0 if using IX.addr+1 instead of H.addr.
    //Multiply by 2
    _ASLa;
    _ROL(H.addr);
    //Add offset and store in IX
    if offset=0 then begin
      _STA(IX.addr);
      _LDA(H.addr);
      _STA(IX.addr+1);
    end else begin
      _CLC;
      _ADCi(lo(offset));
      _STA(IX.addr);
      _LDA(H.addr);
      _ADCi(hi(offset));
      _STA(IX.addr+1);
    end;
    //Load LSB
    _LDYi(0);
    pic.codAsm(i_LDA, aIndirecY, IX.addr);
  end;
  //Load MSB
  _INY;  //To point next byte
  pic.codAsm(i_LDX, aIndirecY, IX.addr);
  _STY(H.addr);     //Returns in H register.
end;
procedure LoadWordIndexWord2(const idxvar: TMirVarDec; offset: word);
{Load in H,A register, the value indexed by "idxvar" variable multiplied by 2 and
added by "offset". Parameter "idxvar" must by word-size.
NOTE: Doesn't use IX register.}
var
  ad1, ad2, lab1: Integer;
begin
  if idxvar.typ.size>2 then begin
    GenError('Not supported this index or pointer type.');
    exit;
  end;
  if (idxvar.addr<256) and (offset<255) then begin  //We need <255
    //Special case
//    //Keep MSB
//    _LDX(idxvar.addr+1);
    //Multiply by 2
    _ASL(idxvar.addr);
    _ROL(idxvar.addr+1);
    //Load MSB
    _LDYi(offset+1);
    pic.codAsm(i_LDA, aIndirecY, idxvar.addr);
    _STA(H.addr);     //Returns in H register.
    //Load LSB
    _DEY;  //To point to LSB
    pic.codAsm(i_LDA, aIndirecY, idxvar.addr);
    //Restore "idxvar"
    _ROR(idxvar.addr+1);  //Restore. Flag C must be still valid before the _ROR.
    _ROR(idxvar.addr);    //Restore OK
//    _STX(idxvar.addr+1);  //Restore all bits.
  end else if offset=0 then begin
    //Load in WR
    _LDA(idxvar.addr);    //LSB
    _LDX(idxvar.addr+1);  //MSB
    _STX(H.addr);  //Could be optimized if writing directly in LDA $FFFF.
    //Multiply by 2
    _ASLa;
    _ROL(H.addr);
    //Load in A
    _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
    _LDA(H.addr);
    _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
    //Start a two-cicles loop to load in H,A
    _LDYi(1);  //Initial offset
_LABEL_pre(lab1);
    _STA(H.addr);  //A->H. Used at the second iteration.
    pic.codAsm(i_LDA, aAbsolutY, $FFFF);  //Instruction will be overwritten
    //Complete the addresses.
    pic.ram[ad1].value   := (pic.iRam-2) and $FF;
    pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
    pic.ram[ad2].value   := (pic.iRam-1) and $ff;
    pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    //Complete the loop
    _DEY;
    _BPL_pre(lab1);  //Stop loop when negative
  end else if offset<255 then begin   //Needs to be <255
    //Similar to case offset=0, but we load offset in Y register.
    //Load in WR
    _LDA(idxvar.addr);    //LSB
    _LDX(idxvar.addr+1);  //MSB
    _STX(H.addr);  //Could be optimized if writing directly in LDA $FFFF.
    //Multiply by 2
    _ASLa;
    _ROL(H.addr);
    //Load in A
    _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
    _LDA(H.addr);
    _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
    //Start a two-cicles loop to load in H,A
    _LDYi(offset+1);  //Initial offset
_LABEL_pre(lab1);
    _STA(H.addr);  //A->H. Used at the second iteration.
    pic.codAsm(i_LDA, aAbsolutY, $FFFF);  //Instruction will be overwritten
    //Complete the addresses.
    pic.ram[ad1].value   := (pic.iRam-2) and $FF;
    pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
    pic.ram[ad2].value   := (pic.iRam-1) and $ff;
    pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    //Complete the loop
    _DEY;
    _CPYi(offset-1);
    _BNE_pre(lab1);  //Stop loop when negative
  end else begin
    //Load in WR
    _LDA(idxvar.addr);    //LSB
    _LDX(idxvar.addr+1);  //MSB
    _STX(H.addr);
    //Multiply by 2 -> H,A
    _ASLa;
    _ROL(H.addr);
    //Add offset and Load in A
    if pic.iRam < 256-21 then begin  //Everything can be done on zero-page.
      _CLC;     //***** Not tested yet.
      _ADCi(lo(offset));
      _STA($FF); ad1:=pic.iRam-1;  //Save address.
      _LDA(H.addr);
      _ADCi(hi(offset));
      _STA($FF); ad2:=pic.iRam-1;  //Save address.
      //Start a two-cicles loop to load in H,A
      _LDYi(1);  //Initial offset
_LABEL_pre(lab1);
      _STA(H.addr);  //A->H. Used at the second iteration.
      pic.codAsm(i_LDA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
      //Complete the loop
      _DEY;
      _BPL_pre(lab1);  //Stop loop when negative
    end else begin      //We need to point to other page
      _CLC;
      _ADCi(lo(offset));
      _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDA(H.addr);
      _ADCi(hi(offset));
      _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
      //Start a two-cicles loop to load in H,A
      _LDYi(1);  //Initial offset
_LABEL_pre(lab1);
      _STA(H.addr);  //A->H. Used at the second iteration.
      pic.codAsm(i_LDA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
      //Complete the loop
      _DEY;
      _BPL_pre(lab1);  //Stop loop when negative
    end;
  end;
end;
procedure SIF_GetItemIdxByte(var fun: TGenOperand);
{SIF for _getitem() method when index is Byte. }
var
  arrVar, idx, op1, op2: TGenOperand;
  itemType: TGenTypeDec;
  offset: Word;
begin
  arrVar := (fun.elements[0]);
  idx := (fun.elements[1]);
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    {Although this SIF can return a stRamFix when index is constant, cannot generate
    a constant.}
    exit;
  end;
  if arrVar.sto = stRamFix then begin
    //Applied to a variable array. The normal.
    itemType := arrVar.Typ.itmType; //Reference to the item type
    //Generate code according to the index storage.
    case idx.Sto of
    stConst: begin  //Constant index. Can return a stRavFix if allocated.
      if arrVar.allocated then begin
        SetFunVariab(fun, arrVar.add + idx.value.valInt * itemType.size);
      end else begin
        //Not yet allocated. We keep as expression to simplify later.
        SetFunExpres(fun);
      end;
    end;
    stRamFix: begin  //Index by variable
      //Calculate offset
      offset := arrVar.add;
      if itemType.IsByteSize then begin  //Must return a byte
        SetFunExpres(fun);
        _LDX(idx.add);
        _LDAx(offset);
      end else if itemType.IsWordSize then begin
        SetFunExpres(fun);
        _LDA(idx.add);  // Load index.
        _ASLa;          // A*2->A. Only work for A<128
        _TAX;           //Move to X
        _INX;           //To point to MSB
        _LDAx(offset);
        _STA(H.addr);
        _DEX;           //To point to LSB
        _LDAx(offset);
      end else begin
        GenError('Cannot get item from this array type: %s.', [arrVar.Typ.name]);
      end;
    end;
    else
//Additional forms could be evaluated here
//      if idx.IsConstantPlusVariable then begin
//        //Is <constant> + <variable> that the SplitExpressions() routines
//        //has allowed pass because knows we can optimize here.
//        op1 := TAstExpress(idx.elements[0]);   //Constant evaluated.
//        op2 := TAstExpress(idx.elements[1]);   //Variable
//        SetFunVariab_RamVarOf(fun, op2.vardec, op1.val, arrVar.vardec); //Index by variable and an offset
//      end else begin
        GenError('Not supported this index.');
        exit;
//      end;
    end;
  end else begin
    GenError('Cannot index array with storage %s.', [arrVar.StoAsStr]);
  end;
end;
procedure SIF_GetItemIdxWord(var fun: TGenOperand);
{SIF for _getitem() method when index is Word. }
var
  arrVar, idx, op1, op2: TGenOperand;
  itemType: TGenTypeDec;
  offset: Word;
begin
  arrVar := (fun.elements[0]);
  idx := (fun.elements[1]);
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    {Although this SIF can return a stRamFix when index is constant, cannot generate
    a constant.}
    exit;
  end;
  if arrVar.sto = stRamFix then begin
    //Applied to a variable array. The normal.
    itemType := arrVar.Typ.itmType; //Reference to the item type
    //Generate code according to the index storage.
    case idx.Sto of
    stConst: begin  //Constant index. Can return a stRavFix if allocated.
      if arrVar.allocated then begin
        SetFunVariab(fun, arrVar.add + idx.value.valInt * itemType.size);
      end else begin
        //Not yet allocated. We keep as expression to simplify later.
        SetFunExpres(fun);
      end;
    end;
    stRamFix: begin  //Index by variable
      //Calculate offset
      offset := arrVar.add;
      if itemType.IsByteSize then begin  //Must return a byte
        SetFunExpres(fun);
        //Variable index is word-size byte.
        LoadByteIndexWord(idx.vardec, offset);
      end else if itemType.IsWordSize then begin
        SetFunExpres(fun);
        //Variable index is word-size byte.
        //LoadWordIndexWord(idx.vardec, offset);   //Require IX
        LoadWordIndexWord2(idx.vardec, offset)
      end else begin
        GenError('Cannot get item from this array type: %s.', [arrVar.Typ.name]);
      end;
    end;
    else
      GenError('Not supported this index.');
      exit;
    end;
  end else begin
    GenError('Cannot index array with storage %s.', [arrVar.StoAsStr]);
  end;
end;
procedure SetByteIndexWord(const idxvar: TMirVarDec; offset: word; parB: TGenOperand);
{Write a value, in the variable indexed by "idxvar" variable and added
by "offset". Parameter "idxvar" must by word-size.
NOTE: Doesn't use IX register. We prefer Self-modifying code.}
var
  ad1, ad2: Integer;
begin
  if idxvar.typ.size>2 then begin
    GenError('Not supported this index or pointer type.');
    exit;
  end;
  if (idxvar.addr<256) and (offset<256) then begin
    //Special case
    case parB.Sto of
    stConst : _LDAi(parB.val);
    stRamFix: _LDA(parB.add);
    stRegister: ; //Operand is already in A
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
    _LDYi(offset);    //Could be zero.
    pic.codAsm(i_STA, aIndirecY, idxvar.addr);
  end else if offset=0 then begin
    //Self-modifying.
    if pic.iRam < 256-13 then begin  //Everything can be done on zero-page.
      _LDX(idxvar.addrL);   //We use LDX to preserve A
      _STX($FF); ad1:=pic.iRam-1;  //Save address.
      _LDX(idxvar.addrH);   //LDA absolute
      _STX($FF); ad2:=pic.iRam-1;  //Save address.
      //Load operand
      case parB.Sto of
      stConst : _LDAi(parB.val);
      stRamFix: _LDA(parB.add);
      stRegister: ; //Operand is already in A
      else
        GenError(MSG_UNSUPPORTED); exit;
      end;
      _STA($FFFF);     //Store byte
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
    end else begin
      _LDX(idxvar.addrL);   //We use LDX to preserve A
      _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDX(idxvar.addrH);   //LDA absolute
      _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
      //Load operand
      case parB.Sto of
      stConst : _LDAi(parB.val);
      stRamFix: _LDA(parB.add);
      stRegister: ; //Operand is already in A
      else
        GenError(MSG_UNSUPPORTED); exit;
      end;
      _STA($FFFF);     //Store byte
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    end;
  end else if offset<256 then begin   //Byte offset
    if pic.iRam < 256-15 then begin  //Everything can be done on zero-page.
      _LDX(idxvar.addrL);   //We use LDX to preserve A
      _STX($FF); ad1:=pic.iRam-1;  //Save address.
      _LDX(idxvar.addrH);   //LDA absolute
      _STX($FF); ad2:=pic.iRam-1;  //Save address.
      _LDYi(offset);
      //Load operand
      case parB.Sto of
      stConst : _LDAi(parB.val);
      stRamFix: _LDA(parB.add);
      stRegister: ; //Operand is already in A
      else
        GenError(MSG_UNSUPPORTED); exit;
      end;
      pic.codAsm(i_STA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
    end else begin
      _LDX(idxvar.addrL);   //We use LDX to preserve A
      _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDX(idxvar.addrH);   //LDA absolute
      _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
      _LDYi(offset);
      //Load operand
      case parB.Sto of
      stConst : _LDAi(parB.val);
      stRamFix: _LDA(parB.add);
      stRegister: ; //Operand is already in A
      else
        GenError(MSG_UNSUPPORTED); exit;
      end;
      pic.codAsm(i_STA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    end;
  end else begin
    //Self-modifying.
    if pic.iRam < 256-18 then begin  //Everything can be done on zero-page.
      //Load operand in X
      case parB.Sto of
      stConst : _LDXi(parB.val);
      stRamFix: _LDX(parB.add);
      stRegister: _TAX_opt; //Operand is already in A
      else
        GenError(MSG_UNSUPPORTED); exit;
      end;
      _CLC;
      _LDA(idxvar.addrL);
      _ADCi(lo(offset));
      _STA($FF); ad1:=pic.iRam-1;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _ADCi(hi(offset));
      _STA($FF); ad2:=pic.iRam-1;  //Save address.
      _STX($FFFF);     //Write byte
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
    end else begin
      //Load operand in X
      case parB.Sto of
      stConst : _LDXi(parB.val);
      stRamFix: _LDX(parB.add);
      stRegister: _TAX_opt; //Operand is already in A
      else
        GenError(MSG_UNSUPPORTED); exit;
      end;
      _CLC;
      _LDA(idxvar.addrL);
      _ADCi(lo(offset));
      _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _ADCi(hi(offset));
      _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
      _STX($FFFF);     //Write byte
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    end;
  end;
end;
procedure SetWordIndexWord(const idxvar: TMirVarDec; offset: word; parB: TGenOperand);
{Write a value, in te variable indexed by "idxVar" variable multiplied by 2 and
added by "offset". Parameter "idxVar" must by word-size.
NOTE: Doesn't use IX register.}
var
  ad1, ad2, lab1: Integer;
begin
  if idxvar.typ.size>2 then begin
    GenError('Not supported this index or pointer type.');
    exit;
  end;
  if (idxvar.addr<256) and (offset<255) then begin  //We need <255
    //Special case
    //Multiply by 2
    _ASL(idxvar.addr);
    _ROL(idxvar.addr+1);
    case parB.Sto of
    stConst : begin
      //Write LSB
      _LDYi(offset);
      _LDAi(parB.valL);
      pic.codAsm(i_STA, aIndirecY, idxvar.addr);
      //Write MSB
      _INY;  //To point to MSB
      _LDAi(parB.valH);
      pic.codAsm(i_STA, aIndirecY, idxvar.addr);
    end;
    stRamFix: begin
      //Write LSB
      _LDYi(offset);
      _LDA(parB.add);
      pic.codAsm(i_STA, aIndirecY, idxvar.addr);
      //Write MSB
      _INY;  //To point to MSB
      _LDA(parB.add+1);
      pic.codAsm(i_STA, aIndirecY, idxvar.addr);
    end;
    stRegister: begin //Operand is already in A
      //Write LSB
      _LDYi(offset);
      //_LDA(parB.add);
      pic.codAsm(i_STA, aIndirecY, idxvar.addr);
      //Write MSB
      _INY;  //To point to MSB
      _LDA(H.addr);
      pic.codAsm(i_STA, aIndirecY, idxvar.addr);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
    //Restore "idxvar"
    _ROR(idxvar.addr+1);  //Restore. Flag C must be still valid before the _ROR.
    _ROR(idxvar.addr);    //Restore OK
  end else if offset=0 then begin
    if parB.Sto = stRegister then _TAX_opt;  //Save A
    //Load in WR
    _LDA(idxvar.addr);    //LSB
    _LDX(idxvar.addr+1);  //MSB
    _STX(H.addr);  //Could be optimized if writing directly in LDA $FFFF.
    //Multiply by 2
    _ASLa;
    _ROL(H.addr);
    //Load in A
    _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
    _LDA(H.addr);
    _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
    //Start a two-cicles loop to load in H,A
    _LDYi(1);  //Initial offset
    //Load MSB
    case parB.Sto of
    stConst   : _LDAi(parB.valH);
    stRamFix  : _LDA(parB.addH);
    stRegister: _LDA(H.addr);
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
_LABEL_pre(lab1);
    pic.codAsm(i_STA, aAbsolutY, $FFFF);  //Instruction will be overwritten
    //Complete the addresses.
    pic.ram[ad1].value   := (pic.iRam-2) and $FF;
    pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
    pic.ram[ad2].value   := (pic.iRam-1) and $ff;
    pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    //Load LSB, for the next iteration.
    case parB.Sto of
    stConst   : _LDAi(parB.valL);
    stRamFix  : _LDA(parB.addL);
    stRegister: _TXA;  //Restore A
    end;
    //Complete the loop
    _DEY;
    _BPL_pre(lab1);  //Stop loop when negative
  end else if offset<255 then begin   //Needs to be <255
    //Similar to case offset=0, but we load offset in Y register.
    if parB.Sto = stRegister then _TAX_opt;  //Save A
    //Load in WR
    _LDA(idxvar.addr);    //LSB
    _LDX(idxvar.addr+1);  //MSB
    _STX(H.addr);  //Could be optimized if writing directly in LDA $FFFF.
    //Multiply by 2
    _ASLa;
    _ROL(H.addr);
    //Load in A
    _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
    _LDA(H.addr);
    _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
    //Start a two-cicles loop to load in H,A
    _LDYi(offset+1);  //Initial offset
    //Load MSB
    case parB.Sto of
    stConst   : _LDAi(parB.valH);
    stRamFix  : _LDA(parB.addH);
    stRegister: _LDA(H.addr);
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
_LABEL_pre(lab1);
    pic.codAsm(i_STA, aAbsolutY, $FFFF);  //Instruction will be overwritten
    //Complete the addresses.
    pic.ram[ad1].value   := (pic.iRam-2) and $FF;
    pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
    pic.ram[ad2].value   := (pic.iRam-1) and $ff;
    pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    //Load LSB, for the next iteration.
    case parB.Sto of
    stConst   : _LDAi(parB.valL);
    stRamFix  : _LDA(parB.addL);
    stRegister: _TXA;  //Restore A
    end;
    //Complete the loop
    _DEY;
    _CPYi(offset-1);
    _BNE_pre(lab1);  //Stop loop when negative
  end else begin
    if parB.Sto = stRegister then begin
      //We need to save the (H,A).
      _TAX_opt;      //Save A in X
      _LDA(H.addr);  //Save H in Stack
      _PHA;
    end;
    //Load in WR
    _LDA(idxvar.addr);    //LSB
    _LDY(idxvar.addr+1);  //MSB
    _STY(H.addr);
    //Multiply by 2 -> H,A
    _ASLa;
    _ROL(H.addr);
    //Add offset and Load in A
    if pic.iRam < 256-21 then begin  //Everything can be done on zero-page.
      _CLC;     //***** Not tested yet.
      _ADCi(lo(offset));
      _STA($FF); ad1:=pic.iRam-1;  //Save address.
      _LDA(H.addr);
      _ADCi(hi(offset));
      _STA($FF); ad2:=pic.iRam-1;  //Save address.
      //Start a two-cicles loop to load in H,A
      _LDYi(1);  //Initial offset
      //Load MSB
      case parB.Sto of
      stConst   : _LDAi(parB.valH);
      stRamFix  : _LDA(parB.addH);
      stRegister: _PLA;  //Restore H
      else
        GenError(MSG_UNSUPPORTED); exit;
      end;
_LABEL_pre(lab1);
      pic.codAsm(i_STA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
      //Load LSB, for the next iteration.
      case parB.Sto of
      stConst   : _LDAi(parB.valL);
      stRamFix  : _LDA(parB.addL);
      stRegister: _TXA;  //Restore A
      end;
      //Complete the loop
      _DEY;
      _BPL_pre(lab1);  //Stop loop when negative
    end else begin      //We need to point to other page
      _CLC;
      _ADCi(lo(offset));
      _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDA(H.addr);
      _ADCi(hi(offset));
      _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
      //Load MSB
      case parB.Sto of
      stConst   : _LDAi(parB.valH);
      stRamFix  : _LDA(parB.addH);
      stRegister: _PLA;  //Restore H
      else
        GenError(MSG_UNSUPPORTED); exit;
      end;
      //Start a two-cicles loop to load in H,A
      _LDYi(1);  //Initial offset
_LABEL_pre(lab1);
      pic.codAsm(i_STA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
      //Load LSB, for the next iteration.
      case parB.Sto of
      stConst   : _LDAi(parB.valL);
      stRamFix  : _LDA(parB.addL);
      stRegister: _TXA;  //Restore A
      end;
      //Complete the loop
      _DEY;
      _BPL_pre(lab1);  //Stop loop when negative
    end;
  end;
end;
procedure SIF_SetItemIndexByte(var fun: TGenOperand);
{Write a value to an array item indexed by a BYTE.}
var
  arrVar, idx, parB: TGenOperand;
  itemType: TGenTypeDec;
  parA_add: DWord;
  offset: Word;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  arrVar := (fun.elements[0]);
  idx := (fun.elements[1]);
  parB := (fun.elements[2]);  //Value to assign.
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  if arrVar.sto <> stRamFix then begin
    //Applied to a variable array.
    GenError('Cannot write to this array.');
    exit;
  end;
  if not arrVar.allocated then begin
    GenError('Array not allocated.');
    exit;
  end;
  itemType := arrVar.Typ.itmType;    //Must be the same as parB.typ.
  if itemType.size<>parB.Typ.size then begin
    {Type compatibility is done in Analysis but can be relaxed because of some BOR like
    word := byte.}
    //genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    genError('Incompatible types');
    exit;
  end;
  //Generate code according to the index storage.
  if          idx.Sto = stConst then begin  //Constant index
    //It's like assign to a simple variable
    if itemType.IsByteSize then begin
      parA_add := arrVar.add + idx.val;
      case parB.Sto of
      stConst: begin
        _LDAi(parB.val);
        _STA(parA_add);
      end;
      stRamFix: begin
        _LDA(parB.add);
        _STA(parA_add);
      end;
      stRegister, stRegistA: begin  //Already in A
        _STA(parA_add);
      end;
      stRegistX: begin
        _STX(parA_add);
      end;
      stRegistY: begin
        _STY(parA_add);
      end;
      else
        GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
      end;
    end else if itemType.IsWordSize then begin
      parA_add := arrVar.add + idx.val*2;
      case parB.Sto of
      stConst : begin
        if parB.valL = parB.valH then begin  //Lucky case
          _LDAi(parB.valL);
          _STA(parA_add);
          _STA(parA_add+1);
        end else begin  //General case
          //Caso general
          _LDAi(parB.valL);
          _STA(parA_add);
          _LDAi(parB.valH);
          _STA(parA_add+1);
        end;
      end;
      stRamFix: begin
        _LDA(parB.addL);
        _STA(parA_add);
        _LDA(parB.addH);
        _STA(parA_add+1);
      end;
      stRegister: begin   //se asume que se tiene en (H,A)
        _STA(parA_add);
        _LDA(H.addr);
        _STA(parA_add+1);
      end;
      else
        GenError(MSG_UNSUPPORTED); exit;
      end;
    end else begin
      GenError('Cannot set item to this array type: %s.', [arrVar.Typ.name]);
    end;
  end else if idx.Sto = stRamFix then begin  //Indexed by variable.
    if itemType.IsByteSize then begin
      offset := arrVar.add;
      case parB.Sto of
      stConst: begin
        _LDAi(parB.val);
        _LDX(idx.add);
        _STAx(offset);
      end;
      stRamFix: begin
        _LDA(parB.add);
        _LDX(idx.add);
        _STAx(offset);
      end;
      stRegister, stRegistA: begin  //Already in A
        _LDX(idx.add);
        _STAx(offset);
      end;
      stRegistX: begin
        _TXA;
        _LDX(idx.add);
        _STAx(offset);
      end;
      stRegistY: begin
        _TYA;
        _LDX(idx.add);
        _STAx(offset);
      end;
      else
        GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
      end;
    end else if itemType.IsWordSize then begin
      offset := arrVar.add;
      case parB.Sto of
      stConst : begin
        _LDA(idx.add);  // Load index.
        _ASLa;          // A*2->A. Only work for A<128
        _TAX;           //Move to X
        if parB.valL = parB.valH then begin  //Lucky case
          _LDAi(parB.valL);
          _STAx(offset);
          _STAx(offset+1);
        end else begin  //General case
          _LDAi(parB.valL);
          _STAx(offset);
          _LDAi(parB.valH);
          _STAx(offset+1);
        end;
      end;
      stRamFix: begin
        _LDA(idx.add);  // Load index.
        _ASLa;          // A*2->A. Only work for A<128
        _TAX;           //Move to X
        _LDA(parB.add);
        _STAx(offset);
        _LDA(parB.add+1);
        _STAx(offset+1);
      end;
      stRegister: begin   //se asume que se tiene en A
        _TAY;     //Save A
        _LDA(idx.add);  // Load index.
        _ASLa;          // A*2->A. Only work for A<128
        _TAX;           //Move to X
        if offset<255 then begin
          pic.codAsm(i_STY, aZeroPagX, offset);
          _LDA(H.addr);
          pic.codAsm(i_STY, aZeroPagX, offset+1);
        end else begin
          _TYA;           //Restore A
          _STAx(offset);
          _LDA(H.addr);
          _STAx(offset+1);
        end;
      end;
      else
        genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
        exit;
      end;
    end else begin
      GenError('Cannot set item to this array type: %s.', [arrVar.Typ.name]);
    end;
  end else begin
    GenError('Not supported this index.');
  end;
end;
procedure SIF_SetItemIndexWord(var fun: TGenOperand);
{Write a value to an array item indexed by a WORD.}
var
  arrVar, idx, parB: TGenOperand;
  itemType: TGenTypeDec;
  parA_add: DWord;
  offset: Word;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  arrVar := (fun.elements[0]);
  idx := (fun.elements[1]);
  parB := (fun.elements[2]);  //Value to assign.
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  if arrVar.sto <> stRamFix then begin
    //Applied to a variable array.
    GenError('Cannot write to this array.');
    exit;
  end;
  if not arrVar.allocated then begin
    GenError('Array not allocated.');
    exit;
  end;
  itemType := arrVar.Typ.itmType;  //Must be the same as parB.typ.
  if itemType.size<>parB.Typ.size then begin
    {Type compatibility is done in Analysis but can be relaxed because of some BOR like
    word := byte.}
    //genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    genError('Incompatible types');
    exit;
  end;
  //Generate code according to the index storage.
  if          idx.Sto = stConst then begin  //Constant index
    //It's like assign to a simple variable
    if itemType.IsByteSize then begin
      parA_add := arrVar.add + idx.val;
      case parB.Sto of
      stConst: begin
        _LDAi(parB.val);
        _STA(parA_add);
      end;
      stRamFix: begin
        _LDA(parB.add);
        _STA(parA_add);
      end;
      stRegister, stRegistA: begin  //Already in A
        _STA(parA_add);
      end;
      stRegistX: begin
        _STX(parA_add);
      end;
      stRegistY: begin
        _STY(parA_add);
      end;
      else
        GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
      end;
    end else if itemType.IsWordSize then begin
      parA_add := arrVar.add + idx.val*2;
      case parB.Sto of
      stConst : begin
        if parB.valL = parB.valH then begin  //Lucky case
          _LDAi(parB.valL);
          _STA(parA_add);
          _STA(parA_add+1);
        end else begin  //General case
          //Caso general
          _LDAi(parB.valL);
          _STA(parA_add);
          _LDAi(parB.valH);
          _STA(parA_add+1);
        end;
      end;
      stRamFix: begin
        _LDA(parB.addL);
        _STA(parA_add);
        _LDA(parB.addH);
        _STA(parA_add+1);
      end;
      stRegister: begin   //se asume que se tiene en (H,A)
        _STA(parA_add);
        _LDA(H.addr);
        _STA(parA_add+1);
      end;
      else
        GenError(MSG_UNSUPPORTED); exit;
      end;
    end else begin
      GenError('Cannot set item to this array type: %s.', [arrVar.Typ.name]);
    end;
  end else if idx.Sto = stRamFix then begin  //Indexed by variable.
    //Calculate offset
    offset := arrVar.add;
    if itemType.IsByteSize then begin
      SetByteIndexWord(idx.vardec, offset, parB);
    end else if itemType.IsWordSize then begin
      SetWordIndexWord(idx.vardec, offset, parB);
    end else begin
      GenError('Not supported assigning to array of: %s.', [itemType.name]);
    end;
  end else begin
    GenError('Cannot use this index storage: %s.', [idx.StoAsStr]);
  end;
end;
function FillArray(parray: TGenOperand): boolean;
{Generates code to fill an array with the value of the A register.
If error, returns FALSE}
var
  n, add_end, n2: Word;
  i, lab1: Integer;
begin
  n := parray.Typ.size;
  add_end := parray.add + n -1;
  if n = 0 then begin
     //Nothing to clear
  end else if n = 1 then begin   //Just one byte
    _STA(parray.add);
  end else if n = 2 then begin  //Es de 2 bytes
    _STA(parray.add);
    _STA(parray.add+1);
  end else if n = 3 then begin  //Es de 3 bytes
    _STA(parray.add);
    _STA(parray.add+1);
    _STA(parray.add+2);
  end else if n = 4 then begin  //Es de 4 bytes
    _STA(parray.add);
    _STA(parray.add+1);
    _STA(parray.add+2);
    _STA(parray.add+3);
  end else if n<256 then begin  //Tamaño pequeño
    _LDXi(n);
_LABEL_pre(lab1);
    _DEX;
    _STAx(parray.add, add_end>255);
    _BNE_pre(lab1);
  end else if n=256 then begin  //Tamaño pequeño
    _LDXi(0);
_LABEL_pre(lab1);
    _STAx(parray.add);
    _INX;
    _BNE_pre(lab1);
  end else if n<512 then begin  //Tamaño pequeño
    n2 := n div 2;
    _LDXi(n2);
_LABEL_pre(lab1);
    _DEX;
    _STAx(parray.add, true);
    _STAx(parray.add + n2, true);
    _BNE_pre(lab1);
    if n mod 2 <> 0 then begin
      _STA(parray.add + n -1);
    end;
  end else if n=512 then begin  //Tamaño pequeño
    _LDXi(0);
_LABEL_pre(lab1);
    _STAx(parray.add, true);
    _STAx(parray.add+256, true);
    _INX;
    _BNE_pre(lab1);
  end else if n<1024 then begin  //Tamaño pequeño
    n2 := n div 4;
    _LDXi(n2);
_LABEL_pre(lab1);
    _DEX;
    _STAx(parray.add, true);
    _STAx(parray.add + n2, true);
    _STAx(parray.add + 2*n2, true);
    _STAx(parray.add + 3*n2, true);
    _BNE_pre(lab1);
    for i:=0 to n mod 4 -1 do begin
      _STA(n2*4 + i);
    end;
  end else if n=1024 then begin
    _LDXi(0);
_LABEL_pre(lab1);
    _STAx(parray.add, true);
    _STAx(parray.add+256, true);
    _STAx(parray.add+512, true);
    _STAx(parray.add+768, true);
    _INX;
    _BNE_pre(lab1);
  end else begin  //Tamaño mayor
    exit(false);
  end;
  //Code geenrated
  exit(true);
end;
procedure SIF_ArrayClear(var fun: TGenOperand);
{Used to clear all items of an array operand.}
var
  parray, pvalue: TGenOperand;
begin
  parray := (fun.elements[0]);
  SetFunNull(fun);
//  //Return the same operand
//  SetResultVariab(fun, parray.add);
//  fun.Typ := parray.Typ;
//  fun.Sto := parray.Sto;
  //Clear the array
  case parray.Sto of
  stRamFix: begin
    if length(fun.elements) = 2 then begin
      //There is value to fill.
      pvalue := (fun.elements[1]);  //Value to fill
      if          pvalue.Sto = stConst then begin
        _LDAi(pvalue.valL);
        if not FillArray(parray) then begin
          GenError('Cannot clear a big array');
        end;
      end else if pvalue.Sto = stRamFix then begin
        _LDA(pvalue.addL);
        if not FillArray(parray) then begin
          GenError('Cannot clear a big array');
        end;
      end else if pvalue.Sto in [stRegister, stRegistA] then begin
        if not FillArray(parray) then begin
          GenError('Cannot clear a big array');
        end;
      end else begin
        GenError('Clear error.');
      end;
    end else begin
      //Normal clear
      _LDAi(0);
      if not FillArray(parray) then begin
        GenError('Cannot clear a big array');
      end;
    end;
  end;
  stConst: begin
    GenError('Cannot clear a constant array');
  end
  else
    GenError('Cannot clear this array');
  end;
end;
procedure SIF_GetPointer(var fun: TGenOperand);
{SIF for getting the value referenced by pointer: p^}
var
  ptrVar: TGenOperand;
  ad1, ad2, lab1: Integer;
begin
  ptrVar := (fun.elements[0]);
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    {Cannot generate a constant.}
    exit;
  end;
  if ptrVar.sto = stConst then begin
    //Applied to a constant pointer.
    SetFunVariab(fun, ptrVar.val);  //Generates a variable
  end else if ptrVar.sto = stRamFix then begin
    //Applied to a variable pointer. The normal.
    if ptrVar.allocated then begin
      SetFunExpres(fun);  //Devolvemos expresión
      if ptrVar.Typ.ptrType = typByte then begin          //^byte
        if ptrVar.add<256 then begin   //In zero page
          _LDXi(0);
          pic.codAsm(i_LDA, aIndirecX, ptrVar.add);
        end else begin
          _LDA(ptrVar.add);    //LSB
          _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
          _LDA(ptrVar.add+1);  //MSB
          _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
          _LDA($FFFF);  //Self modified code
          //Complete the addresses.
          pic.ram[ad1].value   := (pic.iRam-2) and $FF;
          pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
          pic.ram[ad2].value   := (pic.iRam-1) and $ff;
          pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
        end;
      end else if ptrVar.Typ.ptrType = typWord then begin  //^word
        if ptrVar.add<256 then begin   //In zero page
          _LDYi(1);
          pic.codAsm(i_LDA, aIndirecY, ptrVar.add);
          _STA(H.addr);
          _DEY;
          pic.codAsm(i_LDA, aIndirecY, ptrVar.add);
        end else begin
          //Load in WR
          _LDA(ptrVar.add);    //LSB
          _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
          _LDX(ptrVar.add+1);  //MSB
          _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
          //Start a two-cicles loop to load in H,A
          _LDYi(1);  //Initial offset
      _LABEL_pre(lab1);
          _STA(H.addr);  //A->H. Used at the second iteration.
          pic.codAsm(i_LDA, aAbsolutY, $FFFF);  //Instruction will be overwritten
          //Complete the addresses.
          pic.ram[ad1].value   := (pic.iRam-2) and $FF;
          pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
          pic.ram[ad2].value   := (pic.iRam-1) and $ff;
          pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
          //Complete the loop
          _DEY;
          _BPL_pre(lab1);  //Stop loop when negative
        end;
      end else begin  //*** Faltaría implementar los tipos complejos como arreglos
          {Aquí lo que podemos hacer es simplemente generar una variable "stRamVar"
          que sería lo más universal para cualquier tipo de datos. De hecho no se
          necesitaría ya stRamVarOf o stRamReg, porque la regla sería es que todos
          los puteros se "dereferencien" en stRegister (como se hacen con los tipos
          byte y word), pero que se dejen en "stRamVar" para los otros tipos.
          Ya las rutina correspondientes verían como interpretar el "stRamVar",
          como las asignaciones.}
          GenError('Cannot get ^%s.', [ptrVar.Typ.ptrType.name]);
      end;
    end else begin
      {No allocated. }
      GenError('Variable %s not allocated.', [ptrVar.name]);
    end;
  end else begin
    GenError('Cannot get variable pointed by storage %s.', [ptrVar.StoAsStr]);
  end;
end;
procedure SIF_SetPointer(var fun: TGenOperand);
{Setter for asignent values to pointer: p^ := }
var
  ptrVar, parB: TGenOperand;
  ptrTypeTo: TGenTypeDec;
  ad1, ad2, lab: Integer;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  ptrVar := (fun.elements[0]);
  parB := (fun.elements[1]);  //Value to assign.
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  if ptrVar.sto <> stRamFix then begin
    //Applied to a variable array.
    GenError('Cannot write to this pointer: %s.', [ptrVar.name]);
    exit;
  end;
  if not ptrVar.allocated then begin
    GenError('Variable not allocated: %s.', [ptrVar.name]);
    exit;
  end;
  ptrTypeTo := ptrVar.Typ.ptrType;    //Must be the same as parB.typ.
  if ptrTypeTo.size<>parB.Typ.size then begin
    {Type compatibility is done in Analysis but can be relaxed because of some BOR like
    word := byte.}
    //genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    genError('Incompatible types');
    exit;
  end;
  //Generate code
  if ptrTypeTo.IsByteSize then begin     //^byte
    case parB.Sto of
    stConst: begin
      _LDAi(parB.val);
      if ptrVar.add<256 then begin       //In zero page.
        if cpuMode = cpu65C02 then begin
          _STAin(ptrVar.add);
        end else begin
          _LDYi(0);
          _STAiny(ptrVar.add);
        end;
      end else begin
        _LDX(ptrVar.add);    //LSB
        _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDX(ptrVar.add+1);  //MSB
        _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
        _STA($FFFF);  //Self modified code
        //Complete the addresses.
        pic.ram[ad1].value   := (pic.iRam-2) and $FF;
        pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
        pic.ram[ad2].value   := (pic.iRam-1) and $ff;
        pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
      end;
    end;
    stRamFix: begin
      _LDA(parB.add);
      if ptrVar.add<256 then begin       //In zero page.
        if cpuMode = cpu65C02 then begin
          _STAin(ptrVar.add);
        end else begin
          _LDYi(0);
          _STAiny(ptrVar.add);
        end;
      end else begin
        _LDX(ptrVar.add);    //LSB
        _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDX(ptrVar.add+1);  //MSB
        _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
        _STA($FFFF);  //Self modified code
        //Complete the addresses.
        pic.ram[ad1].value   := (pic.iRam-2) and $FF;
        pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
        pic.ram[ad2].value   := (pic.iRam-1) and $ff;
        pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
      end;
    end;
    stRegister, stRegistA: begin  //Already in A
      if ptrVar.add<256 then begin       //In zero page.
        if cpuMode = cpu65C02 then begin
          _STAin(ptrVar.add);
        end else begin
          _LDYi(0);
          _STAiny(ptrVar.add);
        end;
      end else begin
        _LDX(ptrVar.add);    //LSB
        _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDX(ptrVar.add+1);  //MSB
        _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
        _STA($FFFF);  //Self modified code
        //Complete the addresses.
        pic.ram[ad1].value   := (pic.iRam-2) and $FF;
        pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
        pic.ram[ad2].value   := (pic.iRam-1) and $ff;
        pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
      end;
    end;
    stRegistX: begin
      _TXA;
      if ptrVar.add<256 then begin       //In zero page.
        if cpuMode = cpu65C02 then begin
          _STAin(ptrVar.add);
        end else begin
          _LDYi(0);
          _STAiny(ptrVar.add);
        end;
      end else begin
        _LDX(ptrVar.add);    //LSB
        _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDX(ptrVar.add+1);  //MSB
        _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
        _STA($FFFF);  //Self modified code
        //Complete the addresses.
        pic.ram[ad1].value   := (pic.iRam-2) and $FF;
        pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
        pic.ram[ad2].value   := (pic.iRam-1) and $ff;
        pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
      end;
    end;
    stRegistY: begin
      _TYA;
      if ptrVar.add<256 then begin       //In zero page.
        if cpuMode = cpu65C02 then begin
          _STAin(ptrVar.add);
        end else begin
          _LDYi(0);
          _STAiny(ptrVar.add);
        end;
      end else begin
        _LDX(ptrVar.add);    //LSB
        _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDX(ptrVar.add+1);  //MSB
        _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
        _STA($FFFF);  //Self modified code
        //Complete the addresses.
        pic.ram[ad1].value   := (pic.iRam-2) and $FF;
        pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
        pic.ram[ad2].value   := (pic.iRam-1) and $ff;
        pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
      end;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if ptrTypeTo.IsWordSize then begin
    case parB.Sto of
    stConst : begin                    // w^ := $1234;
      if ptrVar.add<256 then begin
        _LDYi(0);
        _LDAi(parB.valL);
        _STAiny(ptrVar.add);
        _INY;
        if parB.valH <> parB.valL then
          _LDAi(parB.valH);
        _STAiny(ptrVar.add);
      end else begin
        _LDA(ptrVar.addL);
        _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDA(ptrVar.addH);
        _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
        _LDXi(1);
        _LDAi(parB.valH);
  _LABEL_pre(lab);
        _STAx($FFFF); _SELFMODw(ad1, ad2);
        if parB.valH <> parB.valL then
          _LDAi(parB.valL);
        _DEX;
        _BPL_pre(lab);
      end;
    end;
    stRamFix: begin                    // w^ := word_var;
      if ptrVar.add<256 then begin
        _LDYi(0);
        _LDA(parB.addL);
        _STAiny(ptrVar.add);
        _INY;
        if parB.addH <> parB.addL then
          _LDA(parB.addH);
        _STAiny(ptrVar.add);
      end else begin
        _LDA(ptrVar.addL);
        _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDA(ptrVar.addH);
        _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
        _LDXi(1);
        _LDA(parB.addH);
  _LABEL_pre(lab);
        _STAx($FFFF); _SELFMODw(ad1, ad2);
        if parB.addH <> parB.addL then
          _LDA(parB.addL);
        _DEX;
        _BPL_pre(lab);
      end;
    end;
    stRegister: begin                  // w^ := expression; of type word (_H/A)
        _LDX(ptrVar.addL);
        _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDX(ptrVar.addH);
        _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
        _LDXi(1);
        _TAY;
        _LDA(H.addr);
  _LABEL_pre(lab);
        _STAx($FFFF); _SELFMODw(ad1, ad2);
        _TYA;
        _DEX;
        _BPL_pre(lab);
    end;
    else
      genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
      exit;
    end;
  end else begin
    GenError('Cannot set item to this pointer type: %s.', [ptrVar.Typ.name]);
  end;
end;
procedure SIF_word_div_word(var fun: TGenOperand);
  var parA, parB: TGenOperand;
      AddrUndef: boolean;
      fdiv: TMirFunDec;
      Dividend, Divisor: TMirVarDec;

  procedure DivbyConst;
    procedure Div2(n: integer);
      var i: integer;
    begin
        _LDA(parA.addH);
        _LSRa;
        _STA(H.addr);
        _LDA(parA.addL);
        _RORa;
      for i := 1 to n do begin
        _LSR(H.addr);
        _RORa;
      end;
    end;
    procedure Div2H(n: integer);
      var i: integer;
    begin
      if cpuMode = cpu65C02 then
        _STZ(H.addr)
      else begin
        _LDAi(0);
        _STA(H.addr);
      end;
        _LDA(parA.addH);
      for i := 1 to n do
        _LSRa;
    end;

  begin
    case parB.val of
      1: begin
        _LDA(parA.addH);
        _STA(H.addr);
        _LDA(parA.addL);
      end;
          2: Div2(0);
          4: Div2(1);
          8: Div2(2);
         16: Div2(3);
         32: Div2(4);
         64: Div2(5);
        128: Div2(6);
        256: Div2H(0);
        512: Div2H(1);
       1024: Div2H(2);
       2048: Div2H(3);
       4096: Div2H(4);
       8192: Div2H(5);
      16384: Div2H(6);
      32768: begin
        Div2H(0);  // to zero H
        _ASLa;
        _LDAi(0);
        _ROLa;
      end
    else
        _LDA(parA.addH);
        _STA(Dividend.addrH);
        _LDA(parA.addL);
        _STA(Dividend.addrL);
        _LDAi(parB.valH);
        _STA(Divisor.addrH);
        _LDAi(parB.valL);
        _STA(Divisor.addrL);
        functCall(fdiv, AddrUndef);
        _LDA(Dividend.addrH);  // Dividend contain DIV
        _STA(H.addr);
        _LDA(Dividend.addrL);
    end;
  end;

begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  fdiv := snfWrdDivWrd16;
  Dividend := fdiv.pars[0].vardec;
  Divisor  := fdiv.pars[1].vardec;
  if compMod = cmConsEval then begin
    if (parA.Sto = stConst) and (parB.Sto = stConst) then
      SetFunConst_word(fun, parA.val div parB.val);
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const:
    SetFunConst_word(fun, parA.val div parB.val);
  stRamFix_Const: begin
    SetFunExpres(fun);
    DivbyConst;
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.valH);
    _STA(Dividend.addrH);
    _LDAi(parA.valL);
    _STA(Dividend.addrL);
    _LDA(parB.addH);
    _STA(Divisor.addrH);
    _LDA(parB.addL);
    _STA(Divisor.addrL);
    functCall(fdiv, AddrUndef);
    _LDA(Dividend.addrH);  // Dividend contain DIV
    _STA(H.addr);
    _LDA(Dividend.addrL);
  end;
  stRamFix_RamFix: begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _STA(Dividend.addrH);
    _LDA(parA.addL);
    _STA(Dividend.addrL);
    _LDA(parB.addH);
    _STA(Divisor.addrH);
    _LDA(parB.addL);
    _STA(Divisor.addrL);
    functCall(fdiv, AddrUndef);
    _LDA(Dividend.addrH);  // Dividend contain DIV
    _STA(H.addr);
    _LDA(Dividend.addrL);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_word_mod_word(var fun: TGenOperand);
  var parA, parB: TGenOperand;
      AddrUndef: boolean;
      fdiv: TMirFunDec;
      Dividend, Divisor, Remainder: TMirVarDec;

  procedure ModbyConst;

    procedure Mod2(n: integer);
    begin
      if cpuMode = cpu65C02 then
        _STZ(H.addr)
      else begin
        _LDAi(0);
        _STA(H.addr);
      end;
        _LDA(parA.addL);
      if n <> 0 then
        _ANDi(n);
    end;

    procedure Mod2H(n: integer);
    begin
        _LDA(parA.addH);
        _ANDi(n);
        _STA(H.addr);
        _LDA(parA.addL);
    end;

  begin
    case parB.val of
      0: ;  // there is no no mod0
      1: begin
        _LDAi(0);
        _STA(H.addr);
      end;
          2: Mod2(1);
          4: Mod2(3);
          8: Mod2(7);
         16: Mod2(15);
         32: Mod2(31);
         64: Mod2(63);
        128: Mod2(127);
        256: Mod2(0);
        512: Mod2H(1);
       1024: Mod2H(3);
       2048: Mod2H(7);
       4096: Mod2H(15);
       8192: Mod2H(31);
      16384: Mod2H(63);
      32768: Mod2H(127);
    else
        _LDA(parA.addH);
        _STA(Dividend.addrH);
        _LDA(parA.addL);
        _STA(Dividend.addrL);
        _LDAi(parB.valH);
        _STA(Divisor.addrH);
        _LDAi(parB.valL);
        _STA(Divisor.addrL);
        functCall(fdiv, AddrUndef);
        _LDA(Remainder.addrH);  // Remainder contain MOD
        _STA(H.addr);
        _LDA(Remainder.addrL);
    end;
  end;

begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  fdiv := snfWrdDivWrd16;
  Dividend := fdiv.pars[0].vardec;
  Divisor  := fdiv.pars[1].vardec;
  Remainder := TMirVarDec(fdiv.declars.items[0]);  //Acceso a variable local
  if compMod = cmConsEval then begin
    if (parA.Sto = stConst) and (parB.Sto = stConst) then
      SetFunConst_word(fun, parA.val mod parB.val);
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const:
    SetFunConst_word(fun, parA.val div parB.val);
  stRamFix_Const: begin
    SetFunExpres(fun);
    ModbyConst;
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.valH);
    _STA(Dividend.addrH);
    _LDAi(parA.valL);
    _STA(Dividend.addrL);
    _LDA(parB.addH);
    _STA(Divisor.addrH);
    _LDA(parB.addL);
    _STA(Divisor.addrL);
    functCall(fdiv, AddrUndef);
    _LDA(Remainder.addrH);  // Dividend contain DIV
    _STA(H.addr);
    _LDA(Remainder.addrL);
  end;
  stRamFix_RamFix: begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _STA(Dividend.addrH);
    _LDA(parA.addL);
    _STA(Dividend.addrL);
    _LDA(parB.addH);
    _STA(Divisor.addrH);
    _LDA(parB.addL);
    _STA(Divisor.addrL);
    functCall(fdiv, AddrUndef);
    _LDA(Remainder.addrH);  // Dividend contain DIV
    _STA(H.addr);
    _LDA(Remainder.addrL);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure DefineShortPointer(etyp: TAstTypeDec);
{Configura las operaciones que definen la aritmética de punteros.}
//var
//  opr: TxpOperator;
begin
  //Asignación desde Byte y Puntero
//  opr:=etyp.CreateBinaryOperator(':=',2,'_set');
//  opr.isSetter :=
//  opr.CreateOperation(typByte, @SIF_byte_asig_byte);
//  opr.CreateOperation(etyp   , @SIF_byte_asig_byte);
//  //Agrega a los bytes, la posibilidad de ser asignados por punteros
//  typByte.operAsign.CreateOperation(etyp, @SIF_byte_asig_byte);
//
//  opr:=etyp.CreateBinaryOperator('=',3,'equal');  //asignación
//  opr.CreateOperation(typByte, @SIF_byte_equal_byte);
//  opr:=etyp.CreateBinaryOperator('+',4,'add');  //suma
//  opr.CreateOperation(typByte, @SIF_pointer_add_byte);
//  opr:=etyp.CreateBinaryOperator('-',4,'add');  //resta
//  opr.CreateOperation(typByte, @SIF_pointer_sub_byte);
//
//  etyp.CreateUnaryPostOperator('^',6,'deref', @SIF_derefPointer);  //dereferencia
end;
{%ENDREGION}
//////////////// Tipo Byte /////////////
procedure byte_LoadToWR(fun: TMirOperand);
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
{*** ¿Es necesario aquí?
        AddCallerToFromCurr(IX);  //We declare using IX
}
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
procedure byte_DefineRegisters;
begin
  //No es encesario, definir registros adicionales a A
end;
procedure byte_SaveToStk;
begin
  _PHA;
end;
//////////////// Tipo Word /////////////
procedure word_RequireWR;
{Generate de callings to Work Register used when loading a Word in Work registers.}
begin
  {*** ¿Es necesario aquí?
  AddCallerToFromCurr(H);
  }
end;
procedure word_LoadToWR(fun: TMirOperand);
{Carga el valor de una expresión a los registros de trabajo.}
var
  idx: TAstVarDec;
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
procedure word_DefineRegisters;
begin
  {*** ¿Es necesario aquí?
  //Changed from versión 0.7.1
  AddCallerToFromCurr(H);
  }
end;
procedure word_SaveToStk;
begin
  //guarda A
  _PHA;
  //guarda H
  _LDA(H.addr);
  _PHA;
end;
procedure word_Low(var fun: TMirOperand);
{Acceso al byte de menor peso de un word.}
var
  par: TMirOperand;
begin
  {*** Revisar esto
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
  }
end;
procedure word_High(var fun: TMirOperand);
{Acceso al byte de mayor peso de un word.}
var
  par: TMirOperand;
begin
  {*** ¿Es necesario aquí?
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
  }
end;


{%REGION Byte operations}
procedure SIF_not_byte(var fun: TGenOperand);
var
  par: TGenOperand;
begin
  par := (fun.elements[0]);  //Only one parameter
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    if par.Sto = stConst then SetFunConst_byte(fun, (not par.value.valInt) and $FF);
    exit;
  end;
  //Code generation
  case par.Sto of
  stConst : begin
    SetFunConst_byte(fun, (not par.value.valInt) and $FF);
  end;
  stRamFix: begin
    SetFunExpres(fun);
    _LDA(par.vardec.addr);
    _EORi($FF);
  end;
  else
    genError('Not implemented: "%s"', [fun.name]);
  end;
end;
procedure SIF_byte_asig_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
  parBsto: TStorage;
  offset: word;
  idxvar: TAstVarDec;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  //Simplify parB
  parBsto := parB.Sto;  //Save storage
  if parB.Sto = stRamVarOf then begin
     LoadToWR(parB);  //Could require IX
     if HayError then exit;
     parB.Sto := stRegister;
  end;
  //Validates parA.
  if parA.opType<>otVariab then begin //The only valid type.
    GenError('Only variables can be assigned.');
    exit;
  end;
  //Implements assignment
  if parA.Sto = stRamFix then begin
    //Assignment to a common variable (constant Address)
    case parB.Sto of
    stConst: begin
      if (ParB.val = 0) and (cpuMode = cpu65C02) then
        _STZ(parA.add)
      else begin
        _LDAi(parB.val);
        _STA(parA.add);
      end;
    end;
    stRamFix: begin
      _LDA(parB.add);
      _STA(parA.add);
    end;
    stRegister, stRegistA: begin  //Already in A
      _STA(parA.add);
    end;
    stRegistX: begin
      _STX(parA.add);
    end;
    stRegistY: begin
      _STY(parA.add);
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto in [stRegistA, stRegister] then begin
    //Assignment to register A
    case parB.Sto of
    stConst : begin
      _LDAi(parB.val);
    end;
    stRamFix: begin
      _LDA(parB.add);
    end;
    stRegister, stRegistA: begin  //Already in A
    end;
    stRegistX: begin
      _TXA;
    end;
    stRegistY: begin
      _TYA;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto = stRegistX then begin
    //Assignment to register X
    case parB.Sto of
    stConst : begin
      _LDXi(parB.val);
    end;
    stRamFix: begin
      _LDX(parB.add);
    end;
    stRegister, stRegistA: begin  //Already in A
      _TAX_opt;
    end;
    stRegistX: begin  //Already in X
    end;
    stRegistY: begin
      _TYA;  //Modify A
      _TAX;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto = stRegistY then begin
    //Assignment to register Y
    case parB.Sto of
    stConst : begin
      _LDYi(parB.val);
    end;
    stRamFix: begin
      _LDY(parB.add);
    end;
    stRegister, stRegistA: begin  //Already in A
      _TAY;
    end;
    stRegistX: begin
      _TXA;  //Modify A
      _TAY;
    end;
    stRegistY: begin //Already in Y
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_byte_and_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_byte(fun, parA.val and parB.val);
      { TODO : Completar con otros casos }
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetFunConst_byte(fun, parA.val and parB.val);  //puede generar error
  end;
  stConst_RamFix: begin
    if parA.val = 0 then begin  //Caso especial
      SetFunConst_byte(fun, 0);  //puede generar error
      exit;
    end else if parA.val = 255 then begin  //Caso especial
      SetFunVariab(fun, parB.vardec);  //puede generar error
      exit;
    end;
    SetFunExpres(fun);
    _LDA(parB.add);
    _ANDi(parA.val);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    if parA.val = 0 then begin  //Caso especial
      SetFunConst_byte(fun, 0);  //puede generar error
      exit;
    end else if parA.val = 255 then begin  //Caso especial
      SetFunExpres(fun);  //No es necesario hacer nada. Ya está en A
      exit;
    end;
    SetFunExpres(fun);
    _ANDi(parA.val);
  end;
  stRamFix_Const: begin
    if parB.val = 0 then begin  //Caso especial
      SetFunConst_byte(fun, 0);  //puede generar error
      exit;
    end else if parB.val = 255 then begin  //Caso especial
      SetFunVariab(fun, parA.vardec);  //puede generar error
      exit;
    end;
    SetFunExpres(fun);
    _LDAi(parB.val);
    _AND(parA.add);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parB.add);
    _AND(parA.add);   //leave in A
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _AND(parA.add);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    if parB.val = 0 then begin  //Caso especial
      SetFunConst_byte(fun, 0);  //puede generar error
      exit;
    end else if parA.val = 255 then begin  //Caso especial
      SetFunExpres(fun);  //No es necesario hacer nada. Ya está en A
      exit;
    end;
    SetFunExpres(fun);
    _ANDi(parB.val)
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _AND(parB.add);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_byte_or_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_byte(fun, parA.val or parB.val);
      { TODO : Completar con otros casos }
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetFunConst_byte(fun, parA.val or parB.val);  //puede generar error
  end;
  stConst_RamFix: begin
    if parA.val = 0 then begin  //Caso especial
      SetFunVariab(fun, parB.vardec);
      exit;
    end else if parA.val = 255 then begin  //Caso especial
      SetFunConst_byte(fun, 255);
      exit;
    end;
    SetFunExpres(fun);
    _LDAi(parA.val);
    _ORA(parB.add);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    if parA.val = 0 then begin  //Caso especial
      SetFunExpres(fun);  //No es necesario hacer nada. Ya está en A
      exit;
    end else if parA.val = 255 then begin  //Caso especial
      SetFunConst_byte(fun, 255);
      exit;
    end;
    SetFunExpres(fun);
    _ORA(parA.val);
  end;
  stRamFix_Const: begin
    if parB.val = 0 then begin  //Caso especial
      SetFunVariab(fun, parA.vardec);
      exit;
    end else if parA.val = 255 then begin  //Caso especial
      SetFunConst_byte(fun, 255);
      exit;
    end;
    SetFunExpres(fun);
    _LDAi(parB.val);
    _ORA(parA.add);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.add);
    _ORA(parB.add);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _ORA(parA.add);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    if parB.val = 0 then begin  //Caso especial
      SetFunExpres(fun);  //No es necesario hacer nada. Ya está en A
      exit;
    end else if parB.val = 255 then begin  //Caso especial
      SetFunConst_byte(fun, 255);
      exit;
    end;
    SetFunExpres(fun);
    _ORA(parB.val);
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _ORA(parB.add);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_byte_xor_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_byte(fun, parA.val xor parB.val);
      { TODO : Completar con otros casos }
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetFunConst_byte(fun, parA.val xor parB.val);  //puede generar error
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.val);
    _EOR(parB.add)
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _EORi(parA.val);  //leave in A
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _LDA(parA.add);   //leave in A
    _EORi(parB.val);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.add);   //leave in A
    _EOR(parB.add);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _EOR(parA.add);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _EORi(parB.val);
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _EOR(parB.add);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_byte_equal_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_bool(fun, parA.val = parB.val);
    end;
    exit;
  end;
  //Code generation
  if parA.Sto = stConst then begin
    case parB.Sto of
    stConst: begin  //compara constantes. Caso especial
      SetFunConst_bool(fun, parA.val = parB.val);
    end;
    stRamFix: begin
      SetFunExpres(fun);   //Se pide Z para el resultado
      if parA.val = 0 then begin  //caso especial
        _LDA(parB.add);
      end else begin
        _LDA(parB.add);
        _CMPi(parA.val);
      end;
      Copy_Z_to_A;
    end;
    stRegister, stRegistA: begin  //la expresión p2 se evaluó y esta en A
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      if parA.val = 0 then begin  //caso especial
        //Nothing
      end else begin
        _CMPi(parA.val);
      end;
      Copy_Z_to_A;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst: begin
      SetFunExpres(fun);
      if parB.val = 0 then begin  //caso especial
        _LDA(parA.add);
      end else begin
        _LDA(parA.add);
        _CMPi(parB.val);
      end;
      Copy_Z_to_A;
    end;
    stRamFix:begin
      SetFunExpres(fun);
      _LDA(parB.add);
      _CMP(parA.add);
      Copy_Z_to_A;
    end;
    stRegister, stRegistA:begin   //parB evaluated in regA
      SetFunExpres(fun);
      _CMP(parA.add);
      Copy_Z_to_A;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto in [stRegister, stRegistA] then begin
    case parB.Sto of
    stConst: begin   //la expresión p1 se evaluó y esta en A
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      SetFunExpres(fun);
      if parB.val = 0 then begin  //caso especial
        //Nothing
      end else begin
        _CMPi(parB.val);
      end;
      Copy_Z_to_A;
    end;
    stRamFix:begin  //parA evaluated in regA
      SetFunExpres(fun);
      _CMP(parB.add);
      Copy_Z_to_A;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_byte_difer_byte(var fun: TGenOperand);
begin
  SIF_byte_equal_byte(fun);  //usa el mismo código
  if not Invert(fun) then begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_byte_aadd_byte(var fun: TGenOperand);
{Operación de asignación suma: +=}
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  //Special assigment
  if parA.Sto = stRamFix then begin
    SetFunNull(fun);  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case parB.Sto of
    stConst : begin
      if parB.val=0 then begin
        //Caso especial. No hace nada
      end else if parB.val=1 then begin
        //Caso especial.
        _INC(parA.add);
      end else if parB.val=2 then begin
        //Caso especial.
        _INC(parA.add);
        _INC(parA.add);
      end else begin
        _CLC;
        _LDA(parA.add);
        _ADCi(parB.val);
        _STA(parA.add);
      end;
    end;
    stRamFix: begin
      _LDA(parA.add);
      _CLC;
      _ADC(parB.add);
      _STA(parA.add);
    end;
    stRegister: begin  //ya está en A
      _CLC;
      _ADC(parA.add);
      _STA(parA.add);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else if parA.Sto = stRegister then begin
//    {Este es un caso especial de asignación a un puntero a byte dereferenciado, pero
//    cuando el valor del puntero es una expresión. Algo así como (ptr + 1)^}
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Asignación normal
//      if parB.val=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        kMOVWF(FSR);  //direcciona
//        _ADDWF(0, toF);
//      end;
//    end;
//    stVariab: begin
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(parB.add, toW);
//      _ADDWF(0, toF);
//    end;
//    stExpres: begin
//      //La dirección está en la pila y la expresión en A
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      rVar := GetVarByteFromStk;
//      kMOVF(rVar.adrByte0, toW);  //opera directamente al dato que había en la pila. Deja en A
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(aux, toW);
//      _ADDWF(0, toF);
//      aux.used := false;
//      exit;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
//  end else if parA.Sto = stVarRef then begin
//    //Asignación a una variable
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Asignación normal
//      if parB.val=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        //Caso especial de asignación a puntero dereferenciado: variable^
//        kMOVF(parA.add, toW);
//        kMOVWF(FSR);  //direcciona
//        _ADDWF(0, toF);
//      end;
//    end;
//    stVariab: begin
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      kMOVF(parA.add, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(parB.add, toW);
//      _ADDWF(0, toF);
//    end;
//    stExpres: begin  //ya está en A
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      kMOVF(parA.add, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(aux, toW);
//      _ADDWF(0, toF);
//      aux.used := false;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure SIF_byte_asub_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  //Caso especial de asignación
  if parA.Sto = stRamFix then begin
    SetFunNull(fun);  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case parB.Sto of
    stConst : begin
      if parB.val=0 then begin
        //Caso especial. No hace nada
      end else if parB.val=1 then begin
        //Caso especial.
        _DEC(parA.add);
      end else if parB.val=2 then begin
        //Caso especial.
        _DEC(parA.add);
        _DEC(parA.add);
      end else begin
        _SEC;
        _LDA(parA.add);
        _SBCi(parB.val);
        _STA(parA.add);
      end;
    end;
    stRamFix: begin
      _SEC;
      _LDA(parA.add);
      _SBC(parB.add);
      _STA(parA.add);
    end;
    stRegister: begin  //ya está en A
      _SEC;
      _SBC(parA.add);   //a - p1 -> a
      //Invierte
      _EORi($ff);
      _CLC;
      _ADCi(1);
      //Devuelve
      _STA(parA.add);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
//  end else if parA.Sto = stExpRef then begin
//    {Este es un caso especial de asignación a un puntero a byte dereferenciado, pero
//    cuando el valor del puntero es una expresión. Algo así como (ptr + 1)^}
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Asignación normal
//      if parB.val=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        kMOVWF(FSR);  //direcciona
//        _SUBWF(0, toF);
//      end;
//    end;
//    stRamFix: begin
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(parB.add, toW);
//      _SUBWF(0, toF);
//    end;
//    stRegister: begin
//      //La dirección está en la pila y la expresión en A
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      rVar := GetVarByteFromStk;
//      kMOVF(rVar.adrByte0, toW);  //opera directamente al dato que había en la pila. Deja en A
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(aux, toW);
//      _SUBWF(0, toF);
//      aux.used := false;
//      exit;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
//  end else if parA.Sto = stVarRef then begin
//    //Asignación a una variable
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case parB.Sto of
//    stConst : begin
//      //Asignación normal
//      if parB.val=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        //Caso especial de asignación a puntero dereferenciado: variable^
//        kMOVF(parA.add, toW);
//        kMOVWF(FSR);  //direcciona
//        _SUBWF(0, toF);
//      end;
//    end;
//    stRamFix: begin
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      kMOVF(parA.add, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(parB.add, toW);
//      _SUBWF(0, toF);
//    end;
//    stRegister: begin  //ya está en A
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      kMOVF(parA.add, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(aux, toW);
//      _SUBWF(0, toF);
//      aux.used := false;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure SIF_byte_add_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
  stoo: TStoOperandsBSIF;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_byte(fun, parA.val + parB.val);
    end;
    exit;
  end;
  //Code generation
  stoo := stoOperation(parA, parB);
  case stoo of
  stConst_Const: begin
    SetFunConst_byte(fun, parA.val + parB.val);  //puede generar error
  end;
  stConst_RamFix, stRamFix_Const: begin
    if stoo = stRamFix_Const then Exchange(parA, parB);
    if parA.val = 0 then begin
      //Caso especial
      SetFunVariab(fun, parB.vardec);  //devuelve la misma variable
      exit;
    end else if parA.val = 1 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDX(parB.add);
      _INX;
      _TXA;
      exit;
    end;
    SetFunExpres(fun);
    _CLC;
    _LDAi(parA.val);
    _ADC(parB.add);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _CLC;
    _ADCi(parA.val);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _CLC;
    _LDA(parA.add);
    _ADC(parB.add);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _CLC;
    _ADC(parA.add);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _CLC;
    _ADCi(parB.val);
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _CLC;
    _ADC(parB.add);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_byte_add_word(var fun: TGenOperand);
begin
  fun.Exchange(0,1);  //Convert to word_add_byte
  SIF_word_add_byte(fun);
  fun.Exchange(0,1);
end;
procedure SIF_byte_sub_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_byte(fun, parA.val-parB.val);
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const:begin  //suma de dos constantes. Caso especial
    SetFunConst_byte(fun, parA.val-parB.val);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _SEC;
    _LDAi(parA.val);
    _SBC(parB.add);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _STA(H.addr);
    _SEC;
    _LDAi(parA.val);
    _SBC(H.addr);
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.add);
    _SBCi(parB.val);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.add);
    _SBC(parB.add);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _SEC;
    _SBC(parA.add);   //a - p1 -> a
    //Invierte
    _EORi($FF);
    _CLC;
    _ADCi(1);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _SEC;
    _SBCi(parB.val);
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _SEC;
    _SBC(parB.add);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SNF_byt_mul_byt_16(fun: TAstFunBase);
//Routine to multiply 8 bits X 8 bits
//pasA * parB -> [H:A]  Usa registros: A,H,E,U
//Based on https://codebase64.org/doku.php?id=base:short_8bit_multiplication_16bit_product
var
  m0, m1: integer;
  fac1,  fac2: TAstVarDec;
begin
    fac1 := fun.pars[0].vardec;
    fac2 := fun.pars[1].vardec;
    PutLabel('__byt_mul_byt_16');
    //A*256 + X = FAC1 * FAC2
    _ldai($00);
    _ldxi($08);
    _clc;
_LABEL_pre(m0);
    _BCC_post(m1);
    _clc;
    _adc(fac2.addr);
_LABEL_post(m1);
    _RORa;
    _ror(fac1.addr);
    _dex;
    _BPL_pre(m0);
    //Returns in H,A
    _STA(H.addr);
    _LDA(fac1.addr);
    _RTS();
end;
procedure SIF_byte_mul_byte(var fun: TGenOperand);
var
  AddrUndef: boolean;
  fmul: TMirFunDec;
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  fmul := snfBytMulByt16;
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_word(fun, parA.val * parB.val);
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    SetFunConst_word(fun, parA.val*parB.val);  //puede generar error
  end;
  stConst_RamFix: begin
    if parA.val = 0 then begin
      //Caso especial
      SetFunConst_word(fun, 0);  //devuelve la misma variable
      exit;
    end else if parA.val = 1 then begin
      //Caso especial
      SetFunVariab(fun, parB.vardec);  //devuelve la misma variable
      exit;
    end else if parA.val = 2 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);
      _LDA(parB.add);
      _ASLa;
      _ROL(H.addr);
      exit;
    end else if parA.val = 4 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);
      _LDA(parB.add);
      _ASLa;
      _ROL(H.addr);
      _ASLa;
      _ROL(H.addr);
      exit;
    end else if parA.val = 8 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      _LDA(parB.add);
      //Loop
      _LDXi(3);  //Counter
//      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
      exit;
    end else if parA.val = 16 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      _LDA(parB.add);
      //Loop
      _LDXi(4);  //Counter
//      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
      exit;
    end else if parA.val = 32 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      _LDA(parB.add);
      //Loop
      _LDXi(5);  //Counter
//      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
      exit;
    end;
    //General case
    SetFunExpres(fun);
    _LDAi(parA.val);
    _STA(fmul.pars[0].vardec.addr);
    _LDA(parB.add);
    _STA(fmul.pars[1].vardec.addr);
//    AddCallerToFromCurr(fmul);  //Declare use
//    AddCallerToFromCurr(fmul.pars[0].vardec);  //Declare use
//    AddCallerToFromCurr(fmul.pars[1].vardec);  //Declare use
    functCall(fmul, AddrUndef);   //Code the "JSR"
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    //Es casi el mismo código de stConst_RamFix
    if parA.val = 0 then begin
      //Caso especial
      SetFunConst_word(fun, 0);  //devuelve la misma variable
      exit;
    end else if parA.val = 1 then begin
      //Caso especial
      SetFunExpres(fun);  //devuelve la misma variable
      exit;
    end else if parA.val = 2 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);
      //_LDA(parB.add);
      _ASLa;
      _ROL(H.addr);
      exit;
    end else if parA.val = 4 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);
      //_LDA(parB.add);
      _ASLa;
      _ROL(H.addr);
      _ASLa;
      _ROL(H.addr);
      exit;
    end else if parA.val = 8 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      //_LDA(parB.add);
      //Loop
      _LDXi(3);  //Counter
//      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
      exit;
    end else if parA.val = 16 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      //_LDA(parB.add);
      //Loop
      _LDXi(4);  //Counter
//      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
      exit;
    end else if parA.val = 32 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      //_LDA(parB.add);
      //Loop
      _LDXi(5);  //Counter
//      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
      exit;
    end;
    //General case
    SetFunExpres(fun);
    //_LDAi(parA.val);
    _STA(fmul.pars[0].vardec.addr);
    _LDA(parA.val);
    _STA(fmul.pars[1].vardec.addr);
//    AddCallerToFromCurr(fmul);  //Declare use
//    AddCallerToFromCurr(fmul.pars[0].vardec);  //Declare use
//    AddCallerToFromCurr(fmul.pars[1].vardec);  //Declare use
    functCall(fmul, AddrUndef);   //Code the "JSR"
  end;
  stRamFix_Const: begin
    fun.Exchange(0,1);
    SIF_byte_mul_byte(fun);
    fun.Exchange(0,1);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.add);
    _STA(fmul.pars[0].vardec.addr);
    _LDA(parB.add);
    _STA(fmul.pars[1].vardec.addr);
//    AddCallerToFromCurr(fmul);  //Declare use
//    AddCallerToFromCurr(fmul.pars[0].vardec);  //Declare use
//    AddCallerToFromCurr(fmul.pars[1].vardec);  //Declare use
    functCall(fmul, AddrUndef);   //Code the "JSR"
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    //_LDA(parA.add);
    _STA(fmul.pars[0].vardec.addr);
    _LDA(parA.add);
    _STA(fmul.pars[1].vardec.addr);
//    AddCallerToFromCurr(fmul);  //Declare use
//    AddCallerToFromCurr(fmul.pars[0].vardec);  //Declare use
//    AddCallerToFromCurr(fmul.pars[1].vardec);  //Declare use
    functCall(fmul, AddrUndef);   //Code the "JSR"
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    fun.Exchange(0,1);
    SIF_byte_mul_byte(fun);  //, true);
    fun.Exchange(0,1);
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    fun.Exchange(0,1);
    SIF_byte_mul_byte(fun);  //, true);
    fun.Exchange(0,1);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_byte_great_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_bool(fun, parA.value.valBool > parB.value.valBool);
    end else if (parA.Sto = stConst) and (parA.val = 0) then begin
      SetFunConst_bool(fun, false);
    end else if (parB.Sto = stConst) and (parB.val = 255) then begin
      SetFunConst_bool(fun, false);
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_bool(fun, parA.val > parB.val);
  end;
  stConst_RamFix: begin
    if parA.val = 0 then begin
      //0 es mayor que nada
      SetFunConst_bool(fun, false);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetFunExpres(fun);
      _LDA(parB.add);
      _CMPi(parA.val); //Result in C (inverted)
      Invert_C_to_A; //Copy C to A (still inverted)
    end;
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    if parA.val = 0 then begin
      //0 es mayor que nada
      SetFunConst_byte(fun, 0);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      //Se necesita asegurar que p1, es mayo que cero.
      SetFunExpres(fun);
      //p2, already in A
      _CMPi(parA.val); //Result in C (inverted)
      Invert_C_to_A; //Copy C to A (still inverted)
    end;
  end;
  stRamFix_Const: begin
    if parB.val = 255 then begin
      //Nada es mayor que 255
      SetFunConst_bool(fun, false);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetFunExpres(fun);
      _LDAi(parB.val);
      _CMP(parA.add); //Result in C (inverted)
      Invert_C_to_A; //Copy C to A (still inverted)
    end;
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parB.add);
    _CMP(parA.add); //Result in C (inverted)
    Invert_C_to_A; //Copy C to A (still inverted)
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    //p2, already in A
    _CMP(parA.add); //Result in C (inverted)
    Invert_C_to_A; //Copy C to A (still inverted)
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    if parB.val = 255 then begin
      //Nada es mayor que 255
      SetFunConst_byte(fun, 0);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetFunExpres(fun);
      //p1, already in A
      _CMPi(parB.val+1); //p1 >= p2+1. We've verified parB.val<255
      Copy_C_to_A; //Copy C to A
    end;
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _CLC;   //A trick to get p1>p2 in C, after _SBC
    _SBC(parB.add);
    Copy_C_to_A; //Copy C to A
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_byte_less_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //A < B es lo mismo que B > A
  fun.Exchange(0,1);
  SIF_byte_great_byte(fun);
  fun.Exchange(0,1);
end;
procedure SIF_byte_gequ_byte(var fun: TGenOperand);
begin
  SIF_byte_less_byte(fun);
  if not Invert(fun) then begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_byte_lequ_byte(var fun: TGenOperand);
begin
  SIF_byte_great_byte(fun);
  if not Invert(fun) then begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_byte_shr_byte(var fun: TGenOperand);  //Desplaza a la derecha
var
  L2, L1: integer;
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_byte(fun, parA.val >> parB.val);
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_byte(fun, parA.val >> parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);   //Se pide Z para el resultado
    _LDAi(parA.val);
    _LDX(parB.add);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    _TAX_opt;
    _BEQ_post(L2);
    _LDAi(parA.val);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);   //Se pide Z para el resultado
    //Verifica casos simples
    if parB.val = 0 then begin
      _LDA(parA.add);  //solo devuelve lo mismo en A
    end else if parB.val = 1 then begin
      _LDA(parA.add);
      _LSRa;
    end else if parB.val = 2 then begin
      _LDA(parA.add);
      _LSRa;
      _LSRa;
    end else if parB.val = 3 then begin
      _LDA(parA.add);
      _LSRa;
      _LSRa;
      _LSRa;
    end else if parB.val = 4 then begin
      _LDA(parA.add);
      _LSRa;
      _LSRa;
      _LSRa;
      _LSRa;
    end else begin
      //Caso general
      _LDA(parA.add);
      _LDXi(parB.val);
_LABEL_pre(L1);
      _LSRa;
      _DEX;
      _BNE_pre(L1);  //loop1
    end;
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);   //Se pide Z para el resultado
    _LDA(parA.add);
    _LDX(parB.add);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    _TAX_opt;
    _BEQ_post(L2);
    _LDA(parA.add);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    //Verifica casos simples
    if parB.val = 0 then begin
      //solo devuelve lo mismo en A
    end else if parB.val = 1 then begin
      _LSRa;
    end else if parB.val = 2 then begin
      _LSRa;
      _LSRa;
    end else if parB.val = 3 then begin
      _LSRa;
      _LSRa;
      _LSRa;
    end else if parB.val = 4 then begin
      _LSRa;
      _LSRa;
      _LSRa;
      _LSRa;
    end else begin
      _LDXi(parB.val);
_LABEL_pre(L1);
      _LSRa;
      _DEX;
      _BNE_pre(L1);  //loop1
    end;
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    _LDX(parB.add);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_byte_shl_byte(var fun: TGenOperand);   //Desplaza a la izquierda
var
  L1, L2: integer;
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_byte(fun, parA.val << parB.val);
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_byte(fun, parA.val << parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);   //Se pide Z para el resultado
    _LDAi(parA.val);
    _LDX(parB.add);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    _TAX_opt;
    _BEQ_post(L2);
    _LDAi(parA.val);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);   //Se pide Z para el resultado
    //Verifica casos simples
    if parB.val = 0 then begin
      _LDA(parA.add);  //solo devuelve lo mismo en A
    end else if parB.val = 1 then begin
      _LDA(parA.add);
      _ASLa;
    end else if parB.val = 2 then begin
      _LDA(parA.add);
      _ASLa;
      _ASLa;
    end else if parB.val = 3 then begin
      _LDA(parA.add);
      _ASLa;
      _ASLa;
      _ASLa;
    end else if parB.val = 4 then begin
      _LDA(parA.add);
      _ASLa;
      _ASLa;
      _ASLa;
      _ASLa;
    end else begin
      //Caso general
      _LDA(parA.add);
      _LDXi(parB.val);
_LABEL_pre(L1);
      _ASLa;
      _DEX;
      _BNE_pre(L1);  //loop1
    end;
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);   //Se pide Z para el resultado
    _LDA(parA.add);
    _LDX(parB.add);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    _TAX_opt;
    _BEQ_post(L2);
    _LDA(parA.add);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    //Verifica casos simples
    if parB.val = 0 then begin
      //solo devuelve lo mismo en A
    end else if parB.val = 1 then begin
      _ASLa;
    end else if parB.val = 2 then begin
      _ASLa;
      _ASLa;
    end else if parB.val = 3 then begin
      _ASLa;
      _ASLa;
      _ASLa;
    end else if parB.val = 4 then begin
      _ASLa;
      _ASLa;
      _ASLa;
      _ASLa;
    end else begin
      _LDXi(parB.val);
_LABEL_pre(L1);
      _ASLa;
      _DEX;
      _BNE_pre(L1);  //loop1
    end;
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    _LDX(parB.add);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
{%ENDREGION}
{%REGION Boolean operations}
procedure bool_LoadToRT(fun: TAstExpress);
begin
  case fun.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if fun.value.valBool then _LDAi(2) else _LDAi(0);
  end;
  stRamFix: begin
    _LDA(fun.vardec.addr);  //values $00 or $02
  end;
  stRegister: begin  //Already in WR
  end;
  //stVarRef, stExpRef, stVarConRef: begin
  // Must be similar to byte type
  //end
  else
    //Almacenamiento no implementado
    GenError(MSG_NOT_IMPLEM);
  end;
end;

procedure SIF_not_bool(var fun: TGenOperand);
var
  par: TGenOperand;
begin
  par := (fun.elements[0]);  //Only one parameter
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    if par.Sto = stConst then SetFunConst_bool(fun, not par.value.valBool);
    exit;
  end;
  //Code generation
  case par.Sto of
  stConst : begin
    //NOT for a constant is defined easily
    SetFunConst_bool(fun, not par.value.valBool);
  end;
  stRamFix: begin
    SetFunExpres(fun);
    //We have to return logical value inverted in A
    _LDA(par.vardec.addr);
    _EORi($FF);
  end;
  stRegister, stRegistA: begin
    SetFunExpres(fun);
    //We have to return logical value inverted in A
    _EORi($FF);  //Operand already in regA
  end;
  else
    genError('Not implemented: "%s"', [fun.name]);
  end;
end;
procedure SIF_bool_asig_bool(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  //Validates parA.
  if parA.opType<>otVariab then begin //The only valid type.
    GenError('Only variables can be assigned.');
    exit;
  end;
  //Realiza la asignación
  if parA.Sto = stRamFix then begin
    //Assignment to a common variable (constant Address)
    case parB.Sto of
    stConst : begin
      _LDAi(parB.value.ValInt);  //We have 255 or 0
      _STA(parA.add);
    end;
    stRamFix: begin
      _LDA(parB.add);
      _STA(parA.add);
    end;
    stRegister, stRegistA: begin  //ya está en A
      _STA(parA.add);
    end;
    stRegistX: begin
      _STX(parA.add);
    end;
    stRegistY: begin
      _STY(parA.add);
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto in [stRegistA, stRegister] then begin
    //Assignment to register A
    case parB.Sto of
    stConst : begin
      _LDAi(parB.value.ValInt);  //We have 255 or 0
    end;
    stRamFix: begin
      _LDA(parB.add);
    end;
    stRegister, stRegistA: begin  //Already in A
    end;
    stRegistX: begin
      _TXA;
    end;
    stRegistY: begin
      _TYA;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto = stRegistX then begin
    //Assignment to register X
    case parB.Sto of
    stConst : begin
      _LDXi(parB.value.ValInt);
    end;
    stRamFix: begin
      _LDX(parB.add);
    end;
    stRegister, stRegistA: begin  //Already in A
      _TAX_opt;
    end;
    stRegistX: begin  //Already in X
    end;
    stRegistY: begin
      _TYA;  //Modify A
      _TAX;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto = stRegistY then begin
    //Assignment to register Y
    case parB.Sto of
    stConst : begin
      _LDYi(parB.value.ValInt);
    end;
    stRamFix: begin
      _LDY(parB.add);
    end;
    stRegister, stRegistA: begin  //Already in A
      _TAY;
    end;
    stRegistX: begin
      _TXA;  //Modify A
      _TAY;
    end;
    stRegistY: begin //Already in Y
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure SIF_bool_and_bool(var fun: TGenOperand);
var
  sale0: integer;
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_bool(fun, parA.value.valBool and parB.value.valBool);
    end else if (parA.Sto = stConst) and (parA.value.ValBool = false) then begin
      SetFunConst_bool(fun, false);
    end else if (parB.Sto = stConst) and (parB.value.ValBool = false) then begin
      SetFunConst_bool(fun, false);
    end;
    exit;
  end;
  //Code generation
  if parA.Sto = stConst then begin
     case parB.Sto of
     stConst: begin
       SetFunConst_bool(fun, parA.value.valBool and parB.value.valBool);
     end;
     stRamFix: begin
        if parA.value.valBool = false then begin  //Special case.
          SetFunConst_bool(fun, false);
        end else begin  //Special case.
          SetFunVariab(fun, parB.vardec);  //Can be problematic return "var". Formaly it should be an expression.
        end;
     end;
     stRegister, stRegistA: begin
       if parA.value.valBool = false then begin  //Special case.
         SetFunConst_bool(fun, false);
       end else begin  //Special case.
         SetFunExpres(fun);  //No needed do anything. Result already in A
       end;
     end;
     else
       GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
     end;
  end else if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = false then begin  //Special case.
        SetFunConst_bool(fun, false);
        exit;
      end else begin  //Special case.
        SetFunVariab(fun, parA.vardec);  //Can be problematic return "var". Formaly it should be an expression.
        exit;
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);
      _LDA(parA.add);
      _AND(parB.add)
    end;
    stRegister, stRegistA: begin
      SetFunExpres(fun);
      //parB already in A
      _AND(parA.add)
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto in [stRegister, stRegistA] then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = false then begin  //Special case.
        SetFunConst_bool(fun, false);
      end else begin  //Special case.
        SetFunExpres(fun);  //No needed do anything. Result already in A
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);
      //parA already in A
      _AND(parB.add)
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_bool_or_bool(var fun: TGenOperand);
var
  sale0: integer;
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_bool(fun, parA.value.valBool or parB.value.valBool);
    end else if (parA.Sto = stConst) and (parA.value.ValBool = true) then begin
      SetFunConst_bool(fun, true);
    end else if (parB.Sto = stConst) and (parB.value.ValBool = true) then begin
      SetFunConst_bool(fun, true);
    end else begin
      exit;
    end;
  end;
  if parA.Sto = stConst then begin
     case parB.Sto of
     stConst: begin
       SetFunConst_bool(fun, parA.value.valBool or parB.value.valBool);
     end;
     stRamFix: begin
        if parA.value.valBool = true then begin  //Special case.
          SetFunConst_bool(fun, true);
        end else begin  //Special case.
          SetFunVariab(fun, parB.vardec);  //Can be problematic return "var". Formaly it should be an expression.
        end;
     end;
     stRegister, stRegistA: begin
       if parA.value.valBool = true then begin  //Special case.
         SetFunConst_bool(fun, true);
       end else begin  //Special case.
         SetFunExpres(fun);  //No needed do anything. Result already in A
       end;
     end;
     else
       GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
     end;
  end else if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = true then begin  //Special case.
        SetFunConst_bool(fun, true);
        exit;
      end else begin  //Special case.
        SetFunVariab(fun, parA.vardec);  //Can be problematic return "var". Formaly it should be an expression.
        exit;
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);
      _LDA(parA.add);
      _ORA(parB.add)
    end;
    stRegister, stRegistA: begin
      SetFunExpres(fun);
      //parB already in A
      _ORA(parA.add)
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto in [stRegister, stRegistA] then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = true then begin  //Special case.
        SetFunConst_bool(fun, true);
      end else begin  //Special case.
        SetFunExpres(fun);  //No needed do anything. Result already in A
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);
      //parA already in A
      _ORA(parB.add);
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_byte_div_byte(var fun: TGenOperand);
  var parA, parB: TGenOperand;
      AddrUndef: boolean;
      fdiv: TMirFunDec;

  procedure DivbyConst;
  begin
    case parB.val of
      1: ;
      2: begin
        _LSRa;
      end;
      3: begin
        _STA(H.addr);
        _LSRa;
        _ADCi(21);
        _LSRa;
        _ADC(H.addr);
        _RORa;
        _LSRa;
        _ADC(H.addr);
        _RORa;
        _LSRa;
        _ADC(H.addr);
        _RORa;
        _LSRa;
      end;
      4: begin
        _LSRa;
        _LSRa;
      end;
      7: begin
        _STA(H.addr);
        _LSRa;
        _LSRa;
        _LSRa;
        _ADC(H.addr);
        _RORa;
        _LSRa;
        _LSRa;
        _ADC(H.addr);
        _RORa;
        _LSRa;
        _LSRa;
      end;
      8: begin
        _LSRa;
        _LSRa;
        _LSRa;
      end;
      16: begin
        _LSRa;
        _LSRa;
        _LSRa;
        _LSRa;
      end;
      32: begin
        _LSRa;
        _LSRa;
        _LSRa;
        _LSRa;
        _LSRa;
      end;
      64: begin
        _LSRa;
        _LSRa;
        _LSRa;
        _LSRa;
        _LSRa;
        _LSRa;
      end;
      128: begin
        _ASLa;
        _LDAi(0);
        _ROLa
      end;
    else
        _LDXi(parB.val);
        functCall(fdiv, AddrUndef);
        _LDA(H.addr);   // Here we need only the div part
    end;
  end;

begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  fdiv := snfBytDivByt8;
  if compMod = cmConsEval then begin
    if (parA.Sto = stConst) and (parB.Sto = stConst) then
      SetFunConst_word(fun, parA.val div parB.val);
    exit;
  end;
    //Code generation
  case stoOperation(parA, parB) of
  stConst_Const:
    SetFunConst_word(fun, parA.val div parB.val);
  stRamFix_Const: begin
    SetFunExpres(fun);
    _LDA(parA.add);
    DivbyConst;
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.val);
    if parA.val > 0 then begin
        _LDX(parB.add);
        functCall(fdiv, AddrUndef);
        _LDA(H.addr);   // Here we need only the div part
    end;
  end;
  stRamFix_RamFix: begin
    SetFunExpres(fun);
        _LDA(parA.add);
        _LDX(parB.add);
        functCall(fdiv, AddrUndef);
        _LDA(H.addr);   // Here we need only the div part
  end;
  stRegist_RamFix: begin
    SetFunExpres(fun);
        //_LDA(parA.add);
        _LDX(parB.add);
        functCall(fdiv, AddrUndef);
        _LDA(H.addr);   // Here we need only the div part
  end;
  stRegist_Const: begin
    SetFunExpres(fun);
    DivbyConst;
  end;
  stConst_Regist: begin
    SetFunExpres(fun);
        _TAX;
        _LDAi(parA.val);
        functCall(fdiv, AddrUndef);
        _LDA(H.addr);   // Here we need only the div part
  end;
  stRamFix_Regist: begin
    SetFunExpres(fun);
        _TAX;
        _LDA(parA.add);
        functCall(fdiv, AddrUndef);
        _LDA(H.addr);   // Here we need only the div part
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_byte_mod_byte(var fun: TGenOperand);
  var parA, parB: TGenOperand;
      AddrUndef: boolean;
      fmod: TMirFunDec;

  procedure ModByConst;
  begin
    case parB.val of
        1: _LDAi(0);
        2: _ANDi(%1);
        4: _ANDi(%11);
        8: _ANDi(%111);
       16: _ANDi(%1111);
       32: _ANDi(%11111);
       64: _ANDi(%111111);
      128: _ANDi(%1111111);
    else
        _LDXi(parB.val);
        functCall(fmod, AddrUndef);
    end;
  end;

begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  fmod := snfBytDivByt8; // the same function as div
  if compMod = cmConsEval then begin
    if (parA.Sto = stConst) and (parB.Sto = stConst) then
      SetFunConst_word(fun, parA.val mod parB.val);
    exit;
  end;
    //Code generation
  case stoOperation(parA, parB) of
  stConst_Const:
    SetFunConst_word(fun, parA.val mod parB.val);
  stRamFix_Const: begin
    SetFunExpres(fun);
    _LDA(parA.add);
    ModByConst;
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.val);
    if parA.val > 0 then begin
        _LDX(parB.add);
        functCall(fmod, AddrUndef);
    end;
  end;
  stRamFix_RamFix: begin
    SetFunExpres(fun);
        _LDA(parA.add);
        _LDX(parB.add);
        functCall(fmod, AddrUndef);
  end;
  stRegist_RamFix: begin
    SetFunExpres(fun);
        //_LDA(parA.add);
        _LDX(parB.add);
        functCall(fmod, AddrUndef);
  end;
  stRegist_Const: begin
    SetFunExpres(fun);
    ModByConst;
  end;
  stConst_Regist: begin
    SetFunExpres(fun);
        _TAX;
        _LDAi(parA.val);
        functCall(fmod, AddrUndef);
  end;
  stRamFix_Regist: begin
    SetFunExpres(fun);
        _TAX;
        _LDA(parA.add);
        functCall(fmod, AddrUndef);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_bool_equal_bool(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_bool(fun, parA.value.valBool = parB.value.valBool);
    end;
    exit;
  end;
  //Code generation
  if parA.Sto = stConst then begin
     case parB.Sto of
     stConst: begin
       SetFunConst_bool(fun, parA.value.valBool = parB.value.valBool);
     end;
     stRamFix: begin
       if parA.value.valBool = false then begin  //Special case.
         SetFunExpres(fun);
         _LDA(parB.add);  // (A = false) is not A
         Invert_A_to_A;
       end else begin  //Special case: parA = True
         SetFunExpres(fun);
         _LDA(parB.add);  //if parB=0 then regA = 0
       end;
     end;
     stRegister, stRegistA: begin
       if parA.value.valBool = false then begin  //Special case.
         if not AcumStatInZ then _TAX;   //Update Z, if needed.
         SetFunExpres(fun);
         Invert_A_to_A;
       end else begin  //Special case: parA = True
         if not AcumStatInZ then _TAX;   //Update Z, if needed.
         SetFunExpres(fun);  //The same
       end;
     end;
     else
       GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
     end;
  end else if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = false then begin  //Special case.
        SetFunExpres(fun);
        _LDA(parA.add);  // (A = false) is not A
        Invert_A_to_A;
      end else begin  //Special case.
        SetFunExpres(fun);
        _LDA(parA.add);   // (A = true) is A
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);
      _LDA(parB.add);
      _EOR(parA.add);  //Compare OperA with OperB. Result in A, inverted.
      Invert_A_to_A;
    end;
    stRegister, stRegistA: begin
      { TODO : We should check "lastASMcode" in order to optimize. }
      SetFunExpres(fun);
      //parA in regA
      _EOR(parA.add);  //Compare OperA with OperB. Result in A, inverted.
      Invert_A_to_A;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto in [stRegister, stRegistA] then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = false then begin  //Special case.
        SetFunExpres(fun);
        if not AcumStatInZ then _TAX;   //Update Z, if needed.
        Invert_A_to_A;
      end else begin  //Special case.
        SetFunExpres(fun);
        if not AcumStatInZ then _TAX;   //Update Z, if needed.
      end;
    end;
    stRamFix: begin
        SetFunExpres(fun);
        //parA in regA
        _EOR(parB.add);  //Compare OperA with OperB
        Invert_A_to_A;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_bool_xor_bool(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_bool(fun, parA.value.valBool xor parB.value.valBool);
    end;
    exit;
  end;
  //Code generation
  if parA.Sto = stConst then begin
     case parB.Sto of
     stConst: begin
       SetFunConst_bool(fun, parA.value.valBool xor parB.value.valBool);
     end;
     stRamFix: begin
       if parA.value.valBool = false then begin  //Special case.
         SetFunExpres(fun);
         _LDA(parB.add);
       end else begin  //Special case: parA = True
         SetFunExpres(fun);
         _LDA(parB.add);
         Invert_A_to_A;
       end;
     end;
     stRegister, stRegistA: begin
       if parA.value.valBool = false then begin  //Special case.
         if not AcumStatInZ then _TAX;   //Update Z, if needed.
         SetFunExpres(fun);
       end else begin  //Special case: parA = True
         if not AcumStatInZ then _TAX;   //Update Z, if needed.
         SetFunExpres(fun);  //The same
         Invert_A_to_A;
       end;
     end;
     else
       GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
     end;
  end else if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = false then begin  //Special case.
        SetFunExpres(fun);
        _LDA(parA.add);
      end else begin  //Special case.
        SetFunExpres(fun);
        _LDA(parA.add);
        Invert_A_to_A;
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);
      _LDA(parB.add);
      _EOR(parA.add);
    end;
    stRegister, stRegistA: begin
      { TODO : We should check "lastASMcode" in order to optimize. }
      SetFunExpres(fun);
      //parA in regA
      _EOR(parA.add);  //Compare OperA with OperB. Result in A, inverted.
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto in [stRegister, stRegistA] then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = false then begin  //Special case.
        SetFunExpres(fun);
        if not AcumStatInZ then _TAX;   //Update Z, if needed.
      end else begin  //Special case.
        SetFunExpres(fun);
        if not AcumStatInZ then _TAX;   //Update Z, if needed.
        Invert_A_to_A;
      end;
    end;
    stRamFix: begin
        SetFunExpres(fun);
        //parA in regA
        _EOR(parB.add);  //Compare OperA with OperB
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
{%ENDREGION}
{%REGION Word operations}
procedure SIF_not_word(var fun: TGenOperand);
var
  par: TGenOperand;
begin
  par := (fun.elements[0]);  //Only one parameter
  requireA;
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    if par.Sto = stConst then SetFunConst_word(fun, (not par.value.valInt) and $FFFF);
    exit;
  end;
  //Code generation
  case par.Sto of
  stConst : begin
    SetFunConst_word(fun, (not par.value.valInt) and $FFFF);
  end;
  stRamFix: begin
    SetFunExpres(fun);
    _LDA(par.addH);
    _EORi($FF);
    _STA(H.addr);
    _LDA(par.addL);
    _EORi($FF);
  end;
//  stExpres: begin
//    SetUORResultExpres_byte;
//    //////
//  end;
  else
    genError('Not implemented: "%s"', [fun.name]);
  end;
end;
procedure SIF_word_asig_word(var fun: TGenOperand);
var
  idxVar: TAstVarDec;
  parA, parB: TGenOperand;
begin
  SetFunNull(fun);
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  //Validates parA.
  if parA.opType<>otVariab then begin //The only valid type.
    GenError('Only variables can be assigned.');
    exit;
  end;
  //Implements assignment
  if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst : begin
      if parB.valL = parB.valH then begin  //Lucky case
        _LDAi(parB.valL);
        _STA(parA.addL);
        _STA(parA.addH);
      end else begin  //General case
        //Caso general
        _LDAi(parB.valL);
        _STA(parA.addL);
        _LDAi(parB.valH);
        _STA(parA.addH);
      end;
    end;
    stRamFix: begin
      _LDA(parB.addL);
      _STA(parA.addL);
      _LDA(parB.addH);
      _STA(parA.addH);
    end;
    stRegister: begin   //se asume que se tiene en (H,A)
      _STA(parA.addL);
      _LDA(H.addr);
      _STA(parA.addH);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else if parA.Sto = stRegister then begin
    requireA;
    //Assignment to register H,A
    case parB.Sto of
    stConst : begin
      if parB.valL = parB.valH then begin  //Lucky case
        _LDAi(parB.valH);
        _STA(H.addr);  //No need to update A
      end else begin  //General case
        _LDAi(parB.valH);
        _STA(H.addr);
        _LDAi(parB.valL);
      end;
    end;
    stRamFix: begin
      _LDA(parB.addH);
      _STA(H.addr);
      _LDA(parB.addL);
    end;
    stRegister: begin  //Already in H,A
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure SIF_word_asig_byte(var fun: TGenOperand);
var
  idxVar: TAstVarDec;
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;   //We don't calculate constant here.
  end;
  if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst : begin
      SetFunExpres(fun);  //Realmente, el resultado no es importante
      if parB.valL = 0 then begin
        _LDAi(0);  //Load once
        _STA(parA.addL);
        _STA(parA.addH);
      end else begin
        _LDAi(parB.valL);
        _STA(parA.addL);
        _LDAi(0);
        _STA(parA.addH);
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);  //Realmente, el resultado no es importante
      _LDA(parB.addL);
      _STA(parA.addL);
      _LDAi(0);
      _STA(parA.addH);
    end;
    stRegister: begin   //se asume que está en A
      SetFunExpres(fun);  //Realmente, el resultado no es importante
      _STA(parA.addL);
      _LDAi(0);
      _STA(parA.addH);
    end;
    else
      genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure SIF_word_equal_word(var fun: TGenOperand);
var
  sale0: integer;
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_bool(fun, parA.val = parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_bool(fun, parA.val = parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.valL);
    _CMP(parB.addL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDAi(parA.valH);
    _CMP(parB.addH);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó p2 esta en A
    SetFunExpres(fun);
//    _LDAi(parA.valL);
    _CMPi(parA.valL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDAi(parA.valH);
    _CMP(H.addr);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _LDA(parA.addL);
    _CMPi(parB.valL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDA(parA.addH);
    _CMPi(parB.valH);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.addL);
    _CMP(parB.addL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDA(parA.addH);
    _CMP(parB.addH);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    //_LDA(parA.addL);
    _CMP(parA.addL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDA(parA.addH);
    _CMP(H.addr);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    //_LDA(parA.addL);
    _CMPi(parB.valL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDAi(parB.valH);
    _CMP(H.addr);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    //_LDA(parA.addL);
    _CMP(parB.addL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDA(parB.addH);
    _CMP(H.addr);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_word_equal_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
  stoo: TStoOperandsBSIF;
  sale0: integer;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_bool(fun, parA.val = parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  if parA.Sto = stConst then begin
    if parA.valH <> 0 then begin  //Always different
      SetFunConst_bool(fun, false);
      exit;
    end;
    //Compare like bytes
    case parB.Sto of
    stConst: begin  //compara constantes. Caso especial
      SetFunConst_bool(fun, parA.val = parB.val);
    end;
    stRamFix: begin
      SetFunExpres(fun);   //Se pide Z para el resultado
      if parA.val = 0 then begin  //caso especial
        _LDA(parB.add);
      end else begin
        _LDA(parB.add);
        _CMPi(parA.val);
      end;
      Copy_Z_to_A;
    end;
    stRegister, stRegistA: begin  //la expresión p2 se evaluó y esta en A
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      if parA.val = 0 then begin  //caso especial
        //Nothing
      end else begin
        _CMPi(parA.val);
      end;
      Copy_Z_to_A;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto = stRamFix then begin
    _LDA(parA.addH);
    _BNE_post(sale0);  //Jimp if <>zero (Z=0)
    //Need to compare low byte
    case parB.Sto of
    stConst: begin
      SetFunExpres(fun);
      if parB.val = 0 then begin  //caso especial
        _LDA(parA.addL);
      end else begin
        _LDA(parA.addL);
        _CMPi(parB.val);
      end;
    end;
    stRamFix:begin
      SetFunExpres(fun);
      _LDA(parB.add);
      _CMP(parA.addL);
    end;
    stRegister, stRegistA:begin   //parB evaluated in regA
      SetFunExpres(fun);
      _CMP(parA.addL);
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
      exit;
    end;
_LABEL_post(sale0);
    Copy_Z_to_A;
  end else if parA.Sto in [stRegister, stRegistA] then begin
    _LDX(H.addr);  //Load High byte
    _BNE(sale0);  //Jimp if <>zero (Z=0)
    case parB.Sto of
    stConst: begin   //la expresión p1 se evaluó y esta en A
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      if parB.val = 0 then begin  //caso especial
        //Nothing
      end else begin
        _CMPi(parB.val);
      end;
    end;
    stRamFix:begin  //parA evaluated in regA
      SetFunExpres(fun);
      _CMP(parB.add);
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
_LABEL_post(sale0);
    Copy_Z_to_A;
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_word_difer_word(var fun: TGenOperand);
begin
  SIF_word_equal_word(fun);
  if not Invert(fun) then begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_word_add_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
         SetFunConst_word(fun, parA.val + parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimize
    SetFunConst_word(fun, parA.val + parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _CLC;
    _LDAi(parA.valL);
    _ADC(parB.addL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
    //Form 2: (Very similar)
//    _LDA(parB.addH);  //parB.add->H
//    _STA(H.addr);
//    _CLC;
//    _LDAi(parA.valL);
//    _ADC(parB.addL);
//    _BCC_post(L2);
//    _INC(H.addr);
//_LABEL_post(L2);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en (A)
    SetFunExpres(fun);
    _CLC;
    _ADCi(parA.valL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _CLC;
    _LDA(parA.addL);
    _ADCi(parB.valL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _CLC;
    _LDA(parA.addL);
    _ADC(parB.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en (_H,A)
    SetFunExpres(fun);
    _CLC;
    _ADC(parA.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADCi(parB.valL);
    _TAX;  //Save
    _LDA(H.addr);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADC(parB.addL);
    _TAX;  //Save
    _LDA(H.addr);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_word_add_word(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_word(fun, parA.val + parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimize
    SetFunConst_word(fun, parA.val + parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _CLC;
    _LDAi(parA.valL);
    _ADC(parB.addL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _ADC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en (A)
    SetFunExpres(fun);
    _CLC;
    _ADCi(parA.valL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _ADC(H.addr);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stRamFix_Const: begin
    if parB.val = 0 then begin  //Special case
      SetFunVariab(fun, parA.vardec);
    end else if parB.valL = 0 then begin
      SetFunExpres(fun);
      _CLC;
      _LDA(parA.addH);
      _ADCi(parB.valH);
      _STA(H.addr);
      _LDA(parA.addL);
    end else begin
      SetFunExpres(fun);
      _CLC;
      _LDA(parA.addL);
      _ADCi(parB.valL);
      _TAX;  //Save
      _LDA(parA.addH);
      _ADCi(parB.valH);
      _STA(H.addr);
      _TXA;  //Restore A
    end;
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _CLC;
    _LDA(parA.addL);
    _ADC(parB.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stRamFix_Regist:begin  //La expresión B se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADC(parA.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADC(H.addr);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stRegist_Const: begin  //La expresión A se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADCi(parB.valL);
    _TAX;  //Save
    _LDA(H.addr);
    _ADCi(parB.valH);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stRegist_RamFix:begin  //La expresión A se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADC(parB.addL);
    _TAX;  //Save
    _LDA(H.addr);
    _ADC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_word_sub_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_word(fun, parA.val-parB.val);  //puede generar error
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const:begin  //suma de dos constantes. Caso especial
    SetFunConst_word(fun, parA.val-parB.val);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _SEC;
    _LDAi(parA.valL);
    _SBC(parB.addL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _SBCi(0);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
//  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
//    SetResultExpres(fun);
//      AddCallerTo(H);  //Declare using register
//    _STA(H);
//    _SEC;
//    _LDA(parA.val);
//    _SBC(H);
//  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.addL);
    _SBCi(parB.valL);
    _TAX;  //Save
    _LDA(parA.addH);
    _SBCi(0);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.addL);
    _SBC(parB.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _SBCi(0);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
//  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBC(parA.add);   //a - p1 -> a
//    //Invierte
//    _EORi($FF);
//    _CLC;
//    _ADCi(1);
//  end;
//  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBCi(parB.val);
//  end;
//  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBC(parB.add);
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_word_sub_word(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_word(fun, parA.val-parB.val);  //puede generar error
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const:begin  //suma de dos constantes. Caso especial
    SetFunConst_word(fun, parA.val-parB.val);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _SEC;
    _LDAi(parA.valL);
    _SBC(parB.addL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _SBC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
//  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
//    SetResultExpres(fun);
//      AddCallerTo(H);  //Declare using register
//    _STA(H);
//    _SEC;
//    _LDA(parA.val);
//    _SBC(H);
//  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.addL);
    _SBCi(parB.valL);
    _TAX;  //Save
    _LDA(parA.addH);
    _SBCi(parB.valH);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.addL);
    _SBC(parB.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _SBC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
//  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBC(parA.add);   //a - p1 -> a
//    //Invierte
//    _EORi($FF);
//    _CLC;
//    _ADCi(1);
//  end;
//  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBCi(parB.val);
//  end;
//  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBC(parB.add);
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_word_mul_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_word(fun, parA.val * parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    SetFunConst_word(fun, parA.val*parB.val);  //puede generar error
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_word_aadd_byte(var fun: TGenOperand);
var
  L1, L2: integer;
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  //Special assigment
  if parA.Sto = stRamFix then begin
    SetFunNull(fun);  //Formaly, an assigment doesn't return any value in Pascal
    //Asignación a una variable
    case parB.Sto of
    stConst : begin
      if parB.val=0 then begin
        //Caso especial. No hace nada
      end else if parB.val=1 then begin
        //Caso especial.
        _INC(parA.addL);
        _BNE_post(L1);
        _INC(parA.addH);
_LABEL_post(L1);
      end else begin
        _CLC;
        _LDA(parA.addL);
        _ADCi(parB.val);
        _STA(parA.addL);
        _BCC_post(L2);
        _INC(parA.addH);
_LABEL_post(L2);
      end;
    end;
    stRamFix: begin
      _CLC;
      _LDA(parA.addL);
      _ADC(parB.add);
      _STA(parA.addL);
      _BCC_post(L2);
      _INC(parA.addH);
_LABEL_post(L2);
    end;
    stRegister: begin  //ya está en A
      _CLC;
      _ADC(parA.addL);
      _STA(parA.addL);
      _BCC_post(L2);
      _INC(parA.addH);
_LABEL_post(L2);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure SIF_word_aadd_word(var fun: TGenOperand);
var
  L1, L2: integer;
  val2: DWord;
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  //Special assigment
  if parA.Sto = stRamFix then begin
    SetFunNull(fun);  //Formaly, an assigment doesn't return any value in Pascal
    //Asignación a una variable
    case parB.Sto of
    stConst : begin
      val2 := parB.val;
      if val2=0 then begin  //Special case
        //Do nothing
      end else if val2=1 then begin  //Special case
        _INC(parA.addL);
        _BNE_post(L1);
        _INC(parA.addH);
_LABEL_post(L1);
      end else if val2 < 256 then begin
        _CLC;
        _LDA(parA.addL);
        _ADCi(parB.val);
        _STA(parA.addL);
        _BCC_post(L2);
        _INC(parA.addH);
_LABEL_post(L2);
      end else if val2 = 256 then begin
        _INC(parA.addH);
      end else if val2 = 512 then begin
        _INC(parA.addH);
        _INC(parA.addH);
      end else if (val2 and $FF) = 0 then begin
        _CLC;
        _LDAi(parB.valH);
        _ADC(parA.addH);
        _STA(parA.addH);
      end else begin
        _CLC;
        _LDA(parA.addL);
        _ADCi(parB.valL);
        _STA(parA.addL);
        _LDA(parA.addH);
        _ADCi(parB.valH);
        _STA(parA.addH);
      end;
    end;
    stRamFix: begin
      _CLC;
      _LDA(parA.addL);
      _ADC(parB.addL);
      _STA(parA.addL);
      _LDA(parA.addH);
      _ADC(parB.addH);
      _STA(parA.addH);
    end;
    stRegister: begin  //ya está en H,A
      _CLC;
      //_LDA(parA.addL);
      _ADC(parA.addL);
      _STA(parA.addL);
      _LDA(H.addr);
      _ADC(parA.addH);
      _STA(parA.addH);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure SIF_word_asub_byte(var fun: TGenOperand);
var
  L1: integer;
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  //Caso especial de asignación
  if parA.Sto = stRamFix then begin
    SetFunNull(fun);  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case parB.Sto of
    stConst : begin
      if parB.val=0 then begin
        //Caso especial. No hace nada
      end else if parB.val=1 then begin
        //Caso especial.
        _LDA(parA.addL);
        _BNE_post(L1);
        _DEC(parA.addH);
_LABEL_post(L1);
        _DEC(parA.addL);
      end else begin
        _SEC;
        _LDA(parA.addL);
        _SBCi(parB.valL);
        _STA(parA.addL);
        _LDA(parA.addH);
        _SBCi(0);
        _STA(parA.addH);
      end;
    end;
    stRamFix: begin
      _SEC;
      _LDA(parA.add);
      _SBC(parB.add);
      _STA(parA.add);
    end;
    stRegister: begin  //ya está en A
      _SEC;
      _SBC(parA.add);   //a - p1 -> a
      //Invierte
      _EORi($ff);
      _CLC;
      _ADCi(1);
      //Devuelve
      _STA(parA.add);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
//  end else if parA.Sto = stExpRef then begin
//    {Este es un caso especial de asignación a un puntero a byte dereferenciado, pero
//    cuando el valor del puntero es una expresión. Algo así como (ptr + 1)^}
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case parB.Sto of
//    stConsta : begin
//      //Asignación normal
//      if parB.val=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        kMOVWF(FSR);  //direcciona
//        _SUBWF(0, toF);
//      end;
//    end;
//    stRamFix: begin
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(parB.add, toW);
//      _SUBWF(0, toF);
//    end;
//    stRegister: begin
//      //La dirección está en la pila y la expresión en A
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      rVar := GetVarByteFromStk;
//      kMOVF(rVar.adrByte0, toW);  //opera directamente al dato que había en la pila. Deja en A
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(aux, toW);
//      _SUBWF(0, toF);
//      aux.used := false;
//      exit;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
//  end else if parA.Sto = stVarRef then begin
//    //Asignación a una variable
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case parB.Sto of
//    stConsta : begin
//      //Asignación normal
//      if parB.val=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        //Caso especial de asignación a puntero dereferenciado: variable^
//        kMOVF(parA.add, toW);
//        kMOVWF(FSR);  //direcciona
//        _SUBWF(0, toF);
//      end;
//    end;
//    stRamFix: begin
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      kMOVF(parA.add, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(parB.add, toW);
//      _SUBWF(0, toF);
//    end;
//    stRegister: begin  //ya está en A
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      kMOVF(parA.add, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(aux, toW);
//      _SUBWF(0, toF);
//      aux.used := false;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure SIF_word_asub_word(var fun: TGenOperand);
  var
    parA, parB: TGenOperand;
    L1: integer;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
    //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  if parA.Sto <> stRamFix then begin
    GenError('Cannot assign to this Operand.');
    exit;
  end;

  SetFunNull(fun);
  case parB.Sto of
  stConst : begin
    if parB.val=0 then begin // do nothing
    end else if parB.val=1 then begin  // we can optimize by doing Dec
      _LDA(parA.addL);
      _BNE_post(L1);
      _DEC(parA.addH);
  _LABEL_post(L1);
      _DEC(parA.addL);
    end else begin
      _SEC;
      _LDA(parA.addL);
      _SBCi(parB.valL);
      _STA(parA.addL);
      _LDA(parA.addH);
      _SBCi(parB.valH);
      _STA(parA.addH);
    end;
  end;
  stRamFix : begin
      _SEC;
      _LDA(parA.addL);
      _SBCi(parB.addL);
      _STA(parA.addL);
      _LDA(parA.addH);
      _SBCi(parB.addH);
      _STA(parA.addH);
  end;
  stRegister: begin  //ya está en A
      SetFunExpres(fun);
      _STA(E.addr);
      _SEC;
      _LDA(parA.addL);
      _SBC(E.addr);
      _STA(parA.addL);
      _LDA(parA.addH);
      _SBC(H.addr);
      _STA(parA.addH);
  end
  else
    GenError(MSG_UNSUPPORTED); exit;
  end;
end;
procedure SIF_word_gequ_word(var fun: TGenOperand);
var
  L1B: integer;
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_bool(fun, parA.val >= parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_bool(fun, parA.val >= parB.val);
  end;
  stConst_RamFix: begin
    if parA.val = 65535 then begin
      //Always true
      SetFunConst_bool(fun, true);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetFunExpres(fun);
      //Compare MSB
      _LDAi(parA.valH);
      _CMP(parB.addH);
      _BNE_post(L1B);  //MSB1<>MSB2, quit with: C=1 -> var1>var2; C=0 -> var1<var2
      //MSB are equal,compare LSB
      _LDAi(parA.valL);
      _CMP(parB.addL);
  _LABEL_post(L1B);
      //Here if C=1, var>=var2; if C=0, var1<var
      Copy_C_to_A; //Copy C to A
    end;
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _STA(E.addr);  //Sava LSB2
    //Compare MSB
    _LDAi(parA.valH);
    _CMP(H.addr);
    _BNE_post(L1B);  //MSB1<>MSB2, quit with: C=1 -> var1>var2; C=0 -> var1<var2
    //MSB are equal,compare LSB
    _LDAi(parA.valL);
    _CMP(E.addr);
_LABEL_post(L1B);
    //Here if C=1, var>=var2; if C=0, var1<var
    Copy_C_to_A; //Copy C to A
  end;
  stRamFix_Const: begin
    if parB.val = 0 then begin
      //Alyway true
      SetFunConst_bool(fun, true);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetFunExpres(fun);
      //Compare MSB
      _LDA(parA.addH);
      _CMPi(parB.valH);
      _BNE_post(L1B);  //MSB1<>MSB2, quit with: C=1 -> var1>var2; C=0 -> var1<var2
      //MSB are equal,compare LSB
      _LDA(parA.addL);
      _CMPi(parB.valL);
  _LABEL_post(L1B);
      //Here if C=1, var>=var2; if C=0, var1<var
      Copy_C_to_A; //Copy C to A
    end;
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    //Compare MSB
    _LDA(parA.addH);
    _CMP(parB.addH);
    _BNE_post(L1B);  //MSB1<>MSB2, quit with: C=1 -> var1>var2; C=0 -> var1<var2
    //MSB are equal,compare LSB
    _LDA(parA.addL);
    _CMP(parB.addL);
_LABEL_post(L1B);
    //Here if C=1, var>=var2; if C=0, var1<var
    Copy_C_to_A; //Copy C to A
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _STA(E.addr);  //Sava LSB2
    //Compare MSB
    _LDA(parA.addH);
    _CMP(H.addr);
    _BNE_post(L1B);  //MSB1<>MSB2, quit with: C=1 -> var1>var2; C=0 -> var1<var2
    //MSB are equal,compare LSB
    _LDA(parA.addL);
    _CMP(E.addr);
_LABEL_post(L1B);
    //Here if C=1, var>=var2; if C=0, var1<var
    Copy_C_to_A; //Copy C to A
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    if parB.val = 0 then begin
      //Alyway true
      SetFunConst_bool(fun, true);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetFunExpres(fun);
      _STA(E.addr);  //Sava LSB1
      //Compare MSB
      _LDA(H.addr);
      _CMPi(parB.valH);
      _BNE_post(L1B);  //MSB1<>MSB2, quit with: C=1 -> var1>var2; C=0 -> var1<var2
      //MSB are equal,compare LSB
      _LDA(E.addr);
      _CMPi(parB.valL);
  _LABEL_post(L1B);
      //Here if C=1, var>=var2; if C=0, var1<var
      Copy_C_to_A; //Copy C to A
    end;
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _STA(E.addr);  //Sava LSB1
    //Compare MSB
    _LDA(H.addr);
    _CMP(parB.addH);
    _BNE_post(L1B);  //MSB1<>MSB2, quit with: C=1 -> var1>var2; C=0 -> var1<var2
    //MSB are equal,compare LSB
    _LDA(E.addr);
    _CMP(parB.addL);
_LABEL_post(L1B);
    //Here if C=1, var>=var2; if C=0, var1<var
    Copy_C_to_A; //Copy C to A
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_word_less_word(var fun: TGenOperand);
begin
  SIF_word_gequ_word(fun);
  if not Invert(fun) then begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_word_great_word(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  fun.Exchange(0,1);
  SIF_word_less_word(fun);
  fun.Exchange(0,1);
end;
procedure SIF_word_lequ_word(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  fun.Exchange(0,1);
  SIF_word_gequ_word(fun);
  fun.Exchange(0,1);
end;
procedure SIF_word_and_byte(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_byte(fun, parA.val and parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimiza
    SetFunConst_byte(fun, parA.val and parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.valL);
    _AND(parB.addL);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en (A)
    SetFunExpres(fun);
    _ANDi(parA.valL);      //Deja en A
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _LDA(parA.addL);
    _ANDi(parB.valL);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.addL);
    _AND(parB.addL);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en (_H,A)
    SetFunExpres(fun);
    _AND(parA.add);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _ANDi(parB.valL);
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _AND(parB.addL);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_word_and_word(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_word(fun, parA.val and parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimiza
    SetFunConst_word(fun, parA.val and parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.valH);
    _AND(parB.addH);
    _STA(H.addr);
    _LDAi(parA.valL);
    _AND(parB.addL);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en (A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _ANDi(parA.valL);
    _PHA;  //Save LSB result
    _LDAi(parA.valH);
    _AND(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _ANDi(parB.valH);
    _STA(H.addr);
    _LDA(parA.addL);
    _ANDi(parB.valL);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _AND(parB.addH);
    _STA(H.addr);
    _LDA(parA.addL);
    _AND(parB.addL);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en (_H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _AND(parA.addL);
    _PHA;  //Save LSB result
    _LDA(parA.addH);
    _AND(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _ANDi(parB.valL);
    _PHA;  //Save LSB result
    _LDAi(parB.valH);
    _AND(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _AND(parB.addL);
    _PHA;  //Save LSB result
    _LDA(parB.addH);
    _AND(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_word_or_word(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_word(fun, parA.val or parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimiza
    SetFunConst_word(fun, parA.val or parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.valH);
    _ORA(parB.addH);
    _STA(H.addr);
    _LDAi(parA.valL);
    _ORA(parB.addL);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en (A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _ORAi(parA.valL);
    _PHA;  //Save LSB result
    _LDAi(parA.valH);
    _ORA(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _ORAi(parB.valH);
    _STA(H.addr);
    _LDA(parA.addL);
    _ORAi(parB.valL);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _ORA(parB.addH);
    _STA(H.addr);
    _LDA(parA.addL);
    _ORA(parB.addL);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en (_H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _ORA(parA.addL);
    _PHA;  //Save LSB result
    _LDA(parA.addH);
    _ORA(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _ORAi(parB.valL);
    _PHA;  //Save LSB result
    _LDAi(parB.valH);
    _ORA(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _ORA(parB.addL);
    _PHA;  //Save LSB result
    _LDA(parB.addH);
    _ORA(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SNF_byt_div_byt_8(funEleExp: TAstFunBase);
{ Returns Div and Mod (byte, byte)
  Source: http://6502org.wikidot.com/software-math-intdiv
  Input:  A - numerator;
          X - denominator
  Output: _H - quotient (div)
          A  - remainder (mod)
}
  var L1, L2: integer;
begin
  PutLabel('__byt_div_byt_8');
    _STA(H.addr);
    _STX(E.addr);
    _LDAi(0);
    _LDXi(8);
    _ASL(H.addr);
_LABEL_pre(L1);
    _ROLa;
    _CMP(E.addr);
    _BCC_post(L2);
    _SBC(E.addr);
_LABEL_post(L2);
    _ROL(H.addr);
    _DEX;
    _BNE_pre(L1);
    _RTS;
end;
procedure SNF_wrd_div_wrd_16(fun: TAstFunBase);
{ Returns Div and Mod (word, word)
  Source: https://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
  Input:  Dividend  - numerator;
          Divisor   - denominator
  Output: Dividend  - quotient (div)
          Remainder - remainder (mod)
}
  var loop, skip: integer;
      Dividend, Divisor, Remainder: TAstVarDec;
begin
  Dividend  := fun.pars[0].vardec;
  Divisor   := fun.pars[1].vardec;
  Remainder := TAstVarDec(fun.elements[2]);
  PutLabel('__word_div_word');
    _LDAi(0);
    _STA(Remainder.addr);
    _STA(Remainder.addr + 1);
    _LDXi(16);
_LABEL_pre(loop);
    _ASL(Dividend.addr);
    _ROL(Dividend.addr + 1);
    _ROL(Remainder.addr);
    _ROL(Remainder.addr + 1);
    _LDA(Remainder.addr);
    _SEC;
    _SBC(Divisor.addr);
    _TAY;
    _LDA(Remainder.addr + 1);
    _SBC(Divisor.addr + 1);
    _BCC_post(skip);
    _STA(Remainder.addr + 1);
    _STY(Remainder.addr);
    _INC(Dividend.addr);
_LABEL_post(skip);
    _DEX;
    _BNE_pre(loop);
    _RTS;
end;
procedure SNF_word_shift_l(fun: TAstFunBase);
{Routine to left shift.
Input:
  (H,A) -> Value to be shifted
  register X -> Number of shift. Must be greater than zero.
Output:
  (H,A) -> Result}
var
  lbl1: integer;
begin
  PutLabel('__word_shift_l');
_LABEL_pre(lbl1);
  _ASLa;
  _ROL(H.addr);
  _DEX;
  _BNE_pre(lbl1);
  _RTS;
end;
procedure SIF_word_shl_byte(var fun: TGenOperand);
var
  i, L1, L2: Integer;
  AddrUndef: boolean;
  fInLine: boolean;
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  fInLine := false;
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_byte(fun, parA.val << parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimiza
    SetFunConst_word(fun, parA.val << parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.valH);
    _STA(H.addr);  //Load high byte
    _LDAi(parA.valL);
    //Loop
    _LDX(parB.add);
    _BEQ_post(L2);  //Protección to zero
//_LABEL_pre(L1);
//      _ASLa;
//      _ROL(H.addr);
//      _DEX;
//    _BNE_pre(L1);
    functCall(snfWordShift_l, AddrUndef);  //Use
_LABEL_post(L2);
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    if parB.val < 4 then begin
      _LDA(parA.addH);
      _STA(H.addr);  //Load high byte
      _LDA(parA.addL);
      for i:=1 to parB.val do begin
        _ASLa;
        _ROL(H.addr);
      end;
    end else begin
      _LDA(parA.addH);
      _STA(H.addr);  //Load high byte
      _LDA(parA.addL);
      //Loop
      _LDXi(parB.val);
      if fInLine then begin
_LABEL_pre(L1);
        _ASLa;
        _ROL(H.addr);
        _DEX;
        _BNE_pre(L1);
      end else begin
        functCall(snfWordShift_l, AddrUndef);  //Use
      end;
    end;
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _STA(H.addr);  //Load high byte
    _LDA(parA.addL);
    //Loop
    _LDX(parB.add);
    _BEQ_post(L2);  //Protección to zero
    if fInLine then begin
_LABEL_pre(L1);
      _ASLa;
      _ROL(H.addr);
      _DEX;
      _BNE_pre(L1);
    end else begin
      functCall(snfWordShift_l, AddrUndef);  //Use
    end;
_LABEL_post(L2);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta A
    SetFunExpres(fun);
    _TAX_opt;  //Counter
    _BEQ_post(L2);  //Protección to zero

    _LDA(parA.addH);
    _STA(H.addr);  //Load high byte
    _LDA(parA.addL);
    if fInLine then begin
_LABEL_pre(L1);
      _ASLa;
      _ROL(H.addr);
      _DEX;
      _BNE_pre(L1);
    end else begin
      functCall(snfWordShift_l, AddrUndef);  //Use
    end;
_LABEL_post(L2);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    if parB.val < 4 then begin
      for i:=1 to parB.val do begin
        _ASLa;
        _ROL(H.addr);
      end;
    end else begin
      _LDXi(parB.val);
      if fInLine then begin
  _LABEL_pre(L1);
        _ASLa;
        _ROL(H.addr);
        _DEX;
        _BNE_pre(L1);
      end else begin
        functCall(snfWordShift_l, AddrUndef);  //Use
      end;
    end;
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en (H,A)
    _LDXi(parB.add);
    if fInLine then begin
_LABEL_pre(L1);
      _ASLa;
      _ROL(H.addr);
      _DEX;
      _BNE_pre(L1);
    end else begin
      functCall(snfWordShift_l, AddrUndef);  //Use
    end;
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure SIF_word_shr_byte(var fun: TGenOperand);
var
  i: Integer;
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_byte(fun, parA.val >> parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimiza
    SetFunConst_byte(fun, parA.val >> parB.val);
  end;
//  stConst_RamFix: begin
//    SetResultExpres(fun);
//    _LDAi(parA.val);
//    _AND(parB.addL);
//  end;
//  stConst_Regist: begin  //la expresión p2 se evaluó y esta en (A)
//    SetResultExpres(fun);
//    _ANDi(parA.valL);      //Deja en A
//  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    if parB.val < 4 then begin
      _LDA(parA.addH);
      _STA(H.addr);  //Load high byte
      _LDA(parA.addL);
      for i:=1 to parB.val do begin
        _LSRa;
        _ROR(H.addr);
      end;
    end else begin
      genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end;
//  stRamFix_RamFix:begin
//  end;
//  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en (_H,A)
//  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    if parB.val < 4 then begin
      for i:=1 to parB.val do begin
        _LSRa;
        _ROR(H.addr);
      end;
    end else begin
      genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end;
//  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en (H,A)
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
{%ENDREGION}
procedure SIF_dword_asig_dword(var fun: TGenOperand);
var
  idxVar: TAstVarDec;
  parA, parB: TGenOperand;
begin
  SetFunNull(fun);
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  //Validates parA.
  if parA.opType<>otVariab then begin //The only valid type.
    GenError('Only variables can be assigned.');
    exit;
  end;
  //Implements assignment
  if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst : begin
      if (parB.val = 0) and (cpuMode = cpu65C02) then begin
        _STZ(parA.add);
        _STZ(parA.add+1);
        _STZ(parA.add+2);
        _STZ(parA.add+3);
      end else if (parB.valWlo = parB.valWhi) and (parB.valL = parB.valH) then begin
        //all byte parts are equal
        _LDAi(parB.valL);
        _STA(parA.add);
        _STA(parA.add+1);
        _STA(parA.add+2);
        _STA(parA.add+3);
      end else if parB.valWlo = parB.valWhi then begin
        //word parts are equal
        _LDAi(parB.valL);
        _STA(parA.add);
        _STA(parA.add+2);
        _LDAi(parB.valH);
        _STA(parA.add+1);
        _STA(parA.add+3);
      end else begin
        //General case
        _LDAi(parB.valL);
        _STA(parA.add);
        _LDAi(parB.valH);
        _STA(parA.add+1);
        _LDAi(parB.valE);
        _STA(parA.add+2);
        _LDAi(parB.valU);
        _STA(parA.add+3);
      end;
    end;
    stRamFix: begin      //stRamFix-stRamFix
      if parA.add = parB.add then begin
        //Maybe parB is the result of a SIF that identified an assignment target.
      end else begin
        _LDA(parB.add);
        _STA(parA.add);
        _LDA(parB.add+1);
        _STA(parA.add+1);
        _LDA(parB.add+2);
        _STA(parA.add+2);
        _LDA(parB.add+3);
        _STA(parA.add+3);
      end;
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure SIF_dword_asig_byte(var fun: TGenOperand);
  var parA, parB: TGenOperand;
begin
  SetFunNull(fun);
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B

  //Process special modes of the compiler.
  if compMod = cmConsEval then exit;  //We don't calculate constant here.
  //Validates parA.
  if (parA.opType<>otVariab) or (parA.Sto <> stRamFix) then begin //The only valid type.
    GenError('Only variables can be assigned.');
    exit;
  end;

  case parB.Sto of
  stConst:    //constant
    if (parB.val = 0) and (cpuMode = cpu65C02) then
      _STZ(parA.add)
    else begin
      _LDAi(parB.valL);
      _STA(parA.add);
     end;
  stRamFix:   //variable
    if parA.add = parB.add then begin
      //Maybe parB is the result of a SIF that identified an assignment target.
    end else begin
      _LDA(parB.add);
      _STA(parA.add);
    end;
  stRegister: //expression
      _STA(parA.add);
  end;
  if cpuMode = cpu65C02 then begin
    _STZ(parA.add+1);
    _STZ(parA.add+2);
    _STZ(parA.add+3);
  end else begin
    _LDAi(0);
    _STA(parA.add+1);
    _STA(parA.add+2);
    _STA(parA.add+3);
  end;
end;
procedure SIF_dword_asig_word(var fun: TGenOperand);
  var parA, parB: TGenOperand;
begin
  SetFunNull(fun);
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B

  //Process special modes of the compiler.
  if compMod = cmConsEval then exit;  //We don't calculate constant here.
  //Validates parA.
  if (parA.opType<>otVariab) or (parA.Sto <> stRamFix) then begin //The only valid type.
    GenError('Only variables can be assigned.');
    exit;
  end;

  case parB.Sto of
  stConst:    //constant
    if (parB.val = 0) and (cpuMode = cpu65C02) then begin
      _STZ(parA.add);
      _STZ(parA.add+1);
    end else begin
      if (parB.valL = 0) and (cpuMode = cpu65C02) then
        _STZ(parA.add)
      else begin
        _LDAi(parB.valL);
        _STA(parA.add);
      end;
      _LDAi(parB.valH);
      _STA(parA.add+1);
     end;
  stRamFix:   //variable
    if parA.add = parB.add then begin
      //Maybe parB is the result of a SIF that identified an assignment target.
    end else begin
      _LDA(parB.add);
      _STA(parA.add);
      _LDA(parB.add+1);
      _STA(parA.add+1);
    end;
  stRegister: begin //expression
      _STA(parA.add);
      _LDA(H.addr);
      _STA(parA.add+1);
    end;
  end;
  if cpuMode = cpu65C02 then begin
    _STZ(parA.add+2);
    _STZ(parA.add+3);
  end else begin
    _LDAi(0);
    _STA(parA.add+2);
    _STA(parA.add+3);
  end;
end;
procedure SIF_dword_add_dword(var fun: TGenOperand);
var
  parA, parB, target: TGenOperand;
  stoo: TStoOperandsBSIF;
  L1, L2, L3: integer;
begin
//  parA := (fun.elements[0]);  //Parameter A
//  parB := (fun.elements[1]);  //Parameter B
//  //Process special modes of the compiler.
//  if compMod = cmConsEval then begin
//    //Cases when result is constant
//    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
//      if parA.evaluated and parB.evaluated then begin
//        SetFunConst_dword(fun, parA.val + parB.val);
//      end;
//    end;
//    exit;
//  end;
//  //Code generation
//  if not GetAssignTarget(fun, target) then begin
//    genError('Internal error.', [BinOperationStr(fun)], fun.srcDec);
//    exit;
//  end;
//
//  stoo := stoOperation(parA, parB);
//  case stoo of
//  stConst_Const: begin
//    //Optimize
//    SetFunConst_dword(fun, parA.val + parB.val);
//  end;
//  stConst_RamFix, stRamFix_Const: begin
//    if stoo = stRamFix_Const then Exchange(parA, parB);
//    SetFunVariab(fun, target.add);  //stRamFix
//
//    if (parA.val = 0) and (parB.add = target.add) then exit
//    else if (parA.val = 0) and (parB.add <> target.add) then begin
//      _LDA(parB.add);
//      _STA(target.add);
//      _LDA(parB.add+1);
//      _STA(target.add+1);
//      _LDA(parB.add+2);
//      _STA(target.add+2);
//      _LDA(parB.add+3);
//      _STA(target.add+3);
//    end else if (parA.val = 1) and (parB.add = target.add) then begin
//      _INC(target.add);
//      _BNE_post(L1);
//      _INC(target.add+1);
//      _BNE_post(L2);
//      _INC(target.add+2);
//      _BNE_post(L3);
//      _INC(target.add+3);
//  _LABEL_post(L1);
//  _LABEL_post(L2);
//  _LABEL_post(L3);
//    end else begin
//      _CLC;
//      _LDAi(parA.valL);
//      _ADC(parB.add);
//      _STA(target.add);
//
//      _LDAi(parA.valH);
//      _ADC(parB.add+1);
//      _STA(target.add+1);
//
//      _LDAi(parA.valE);
//      _ADC(parB.add+2);
//      _STA(target.add+2);
//
//      _LDAi(parA.valU);
//      _ADC(parB.add+3);
//      _STA(target.add+3);
//    end;
//  end;
//  stRamFix_RamFix: begin
//    SetFunVariab(fun, target.add);  //stRamFix
//    _CLC;
//    _LDA(parA.add);
//    _ADC(parB.add);
//    _STA(target.add);
//
//    _LDA(parA.add+1);
//    _ADC(parB.add+1);
//    _STA(target.add+1);
//
//    _LDA(parA.add+2);
//    _ADC(parB.add+2);
//    _STA(target.add+2);
//
//    _LDA(parA.add+3);
//    _ADC(parB.add+3);
//    _STA(target.add+3);
//  end;
//  else
//    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
//  end;
end;
procedure SIF_dword_add_byte(var fun: TGenOperand);
  var parA, parB, target: TGenOperand;
      stoo: TStoOperandsBSIF;
      L1, L2, L3: integer;
begin
//  parA := (fun.elements[0]);  //Parameter A
//  parB := (fun.elements[1]);  //Parameter B
//
//  //Process special modes of the compiler.
//  if compMod = cmConsEval then begin
//    //Cases when result is constant
//    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
//      if parA.evaluated and parB.evaluated then begin
//        SetFunConst_dword(fun, parA.val + parB.val);
//      end;
//    end;
//    exit;
//  end;
//  //Code generation
//  if not GetAssignTarget(fun, target) then begin
//    genError('Internal error.', [BinOperationStr(fun)], fun.srcDec);
//    exit;
//  end;
//
//  stoo := stoOperation(parA, parB);
//  case stoo of
//  stConst_Const:
//    SetFunConst_dword(fun, parA.val + parB.val);
//  stRamFix_Const, stConst_RamFix: begin
//    if stoo = stConst_RamFix then Exchange(parA, parB);
//    SetFunVariab(fun, target.add);  //stRamFix
//
//    if (parB.val = 0) and (parA.add = target.add) then
//      exit
//    else if (parB.val = 0) and (parA.add <> target.add) then begin
//      _LDA(parA.add);
//      _STA(target.add);
//      _LDA(parA.add+1);
//      _STA(target.add+1);
//      _LDA(parA.add+2);
//      _STA(target.add+2);
//      _LDA(parA.add+3);
//      _STA(target.add+3);
//      exit;
//    end else if (parB.val = 1) and (parA.add = target.add) then begin
//      _INC(target.add);
//      _BNE_post(L1);
//      _INC(target.add+1);
//      _BNE_post(L2);
//      _INC(target.add+2);
//      _BNE_post(L3);
//      _INC(target.add+3);
//  _LABEL_post(L1);
//  _LABEL_post(L2);
//  _LABEL_post(L3);
//      exit;
//    end else begin
//      _CLC;
//      _LDA(parA.add);
//      _ADCi(parB.val);
//      _STA(target.add);
//    end;
//  end;
//  stRamFix_RamFix: begin
//    SetFunVariab(fun, target.add);  //stRamFix
//    _CLC;
//    _LDA(parA.add);
//    _ADC(parB.add);
//    _STA(target.add);
//  end;
//  else
//    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
//    exit;
//  end;
//
//  _LDA(parA.add+1);
//  _ADCi(0);
//  _STA(target.add+1);
//
//  _LDA(parA.add+2);
//  _ADCi(0);
//  _STA(target.add+2);
//
//  _LDA(parA.add+3);
//  _ADCi(0);
//  _STA(target.add+3);
end;
procedure SIF_dword_add_word(var fun: TGenOperand);
  var parA, parB, target: TGenOperand;
      stoo: TStoOperandsBSIF;
      L1, L2, L3: integer;
begin
//  parA := (fun.elements[0]);  //Parameter A
//  parB := (fun.elements[1]);  //Parameter B
//
//  //Process special modes of the compiler.
//  if compMod = cmConsEval then begin
//    //Cases when result is constant
//    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
//      if parA.evaluated and parB.evaluated then begin
//        SetFunConst_dword(fun, parA.val + parB.val);
//      end;
//    end;
//    exit;
//  end;
//  //Code generation
//  if not GetAssignTarget(fun, target) then begin
//    genError('Internal error.', [BinOperationStr(fun)], fun.srcDec);
//    exit;
//  end;
//
//  stoo := stoOperation(parA, parB);
//  case stoo of
//  stConst_Const:
//    SetFunConst_dword(fun, parA.val + parB.val);
//  stRamFix_Const, stConst_RamFix: begin
//    if stoo = stConst_RamFix then Exchange(parA, parB);
//    SetFunVariab(fun, target.add);  //stRamFix
//    // No optimizations for 0 and 1. Byte takes precedence so it will handle them
//    _CLC;
//    _LDA(parA.add);
//    _ADCi(parB.valL);
//    _STA(target.add);
//    _LDA(parA.add+1);
//    _ADCi(parB.valH);
//    _STA(target.add+1);
//  end;
//  stRamFix_RamFix: begin
//    SetFunVariab(fun, target.add);  //stRamFix
//    _CLC;
//    _LDA(parA.add);
//    _ADC(parB.add);
//    _STA(target.add);
//    _LDA(parA.add+1);
//    _ADC(parB.add+1);
//    _STA(target.add+1);
//  end;
//  else
//    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
//    exit;
//  end;
//
//  _LDA(parA.add+2);
//  _ADCi(0);
//  _STA(target.add+2);
//
//  _LDA(parA.add+3);
//  _ADCi(0);
//  _STA(target.add+3);
end;

{%REGION Char operations}
procedure SIF_char_asig_char(var fun: TGenOperand);
begin
  SIF_byte_asig_byte(fun);
end;
procedure SIF_char_asig_string(var fun: TGenOperand);
var
  parA, parB: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  parB := (fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;
  end;
  //Code generation
  //Solo se permite asignar constamtes cadenas de 1 caracter
  if parB.Sto <> stConst then begin
    GenError('Cannot assign to this Operand.'); exit;
    exit;
  end;
  if length(parB.value.ValStr) <> 1 then begin
    GenError('String must be 1 char size.'); exit;
    exit;
  end;
  parB.value.ValInt := ord(parB.value.ValStr[1]);  //transform
  SIF_byte_asig_byte(fun);
end;
procedure SIF_char_equal_char(var fun: TGenOperand);
begin
  SIF_byte_equal_byte(fun);  //es lo mismo
end;
procedure SIF_char_difer_char(var fun: TGenOperand);
begin
  SIF_byte_difer_byte(fun); //es lo mismo
end;
{%ENDREGION}

///////////// System INLINE function
procedure SIF_delay_ms(fun: TGenOperand);
var
  par: TGenOperand;
  elefun: TGenFunDec;
begin
  par := fun.elements[0];  //Only one parameter
  elefun := fun.fundec;  ////**** VErificar si es válido siempre.
  if par.Typ = typByte then begin
    //El parámetro byte, debe estar en A
    if fun.opType=otFunct then begin
      _LDAi(par.val);
      _JSR(elefun.adrr);
    end else begin
      GenError('Cannot get address of %s', [fun.name]);
    end;
  end else if par.Typ = typWord then begin
    //El parámetro word, debe estar en (H, A)
    if fun.opType=otFunct then begin
      _LDAi(par.valH);
      _STA(H.addr);
      _LDAi(par.valL);
      _JSR(elefun.adrr2);
    end else begin
      GenError('Cannot get address of %s', [fun.name]);
    end;
  end else begin
    GenError(MSG_INVAL_PARTYP, [par.Typ.name]);
    exit;
  end;
end;
procedure SIF_Inc(var fun: TGenOperand);
var
  LABEL1, L2: integer;
  par: TGenOperand;
begin
  par := (fun.elements[0]);  //Only one parameter
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  //Validations
  case par.opType of
  otConst: begin GenError('Cannot increase a constant.');exit; end;
  otFunct: begin GenError('Cannot increase a function/procedure or expression result.'); exit; end;
  otVariab: ; //The only valid type.
  else  //Not expected to happen
    GenError('Unimplemented.'); exit;
  end;
  //Code generation
  case par.Sto of
  stConst: begin
    GenError('Cannot increase a constant.'); exit;
  end;
  stRamFix: begin  //A common variable
    if (par.Typ = typByte) or (par.Typ = typChar) then begin
      _INC(par.vardec.addr);
    end else if par.Typ = typWord then begin
      _INC(par.vardec.addr);
      _BNE_post(LABEL1);  //label
      _INC(par.vardec.addr+1);
_LABEL_post(LABEL1);
    end else if par.Typ.catType = tctPointer then begin
      if par.Typ.ptrType.size = 1 then begin
        _INC(par.vardec.addr);
        _BNE_post(LABEL1);  //label
        _INC(par.vardec.addr+1);
_LABEL_post(LABEL1);
      end else if par.Typ.ptrType.size <256 then begin
        _CLC;
        _LDA(par.addL);
        _ADCi(par.Typ.ptrType.size);
        _STA(par.addL);
        _BCC_post(L2);
        _INC(par.addH);
_LABEL_post(L2);
      end else begin
        genError('Not implemented "%s()" for operands "%s".',
                 [fun.name, par.StoAsStr]);
      end;
    end else begin
      GenError(MSG_INVAL_PARTYP, [par.Typ.name]);
      exit;
    end;
  end;
  stRegister: begin
    if (par.Typ = typByte) or (par.Typ = typChar) then begin
      _CLC;
      _ADCi(1);
    end else if par.Typ = typWord then begin
      _CLC;
      _ADCi(1);
      _BNE_post(LABEL1);  //label
      _INC(H.addr);
_LABEL_post(LABEL1);
    end else begin
      GenError(MSG_INVAL_PARTYP, [par.Typ.name]);
      exit;
    end;
  end;
  stRegistA: begin
    _CLC;
    _ADCi(1);
  end;
  stRegistX: begin _INX; end;
  stRegistY: begin _INY; end;
  else
    genError('Not implemented "%s()" for operands "%s".',
             [fun.name, par.StoAsStr]);
  end;
end;
procedure SIF_Dec(var fun: TGenOperand);
var
  lbl1: integer;
  par: TGenOperand;
begin
  par := (fun.elements[0]);  //Only one parameter
  case par.opType of
  otConst: begin GenError('Cannot decrease a constant.');exit; end;
  otFunct: begin GenError('Cannot decrease a function/procedure or expression result.'); exit; end;
  otVariab: ; //The only valid type.
  else  //Not expected to happen
    GenError('Unimplemented.'); exit;
  end;
  //Code generation
  case par.Sto of
  stConst: begin
    GenError('Cannot decrease a constant.'); exit;
  end;
  stRamFix: begin  //A common variable
    if (par.Typ= typByte) or (par.Typ = typChar) then begin
      _DEC(par.vardec.addr);
    end else if par.Typ = typWord then begin
      _LDA(par.vardec.addr);
      _BNE_post(lbl1);
      _DEC(par.vardec.addr+1);
_LABEL_post(lbl1);
      _DEC(par.vardec.addr);
    end else if par.Typ.catType = tctPointer then begin
      if par.Typ.ptrType.size = 1 then begin
        _LDA(par.vardec.addr);
        _BNE_post(lbl1);
        _DEC(par.vardec.addr+1);
  _LABEL_post(lbl1);
        _DEC(par.vardec.addr);
      end else if par.Typ.ptrType.size <256 then begin
        _SEC;
        _LDA(par.addL);
        _SBCi(par.Typ.ptrType.size);
        _STA(par.addL);
        _LDA(par.addH);
        _SBCi(0);
        _STA(par.addH);
      end else begin
        genError('Not implemented "%s()" for operands "%s".',
                 [fun.name, par.StoAsStr]);
      end;
    end else begin
      GenError(MSG_INVAL_PARTYP, [par.Typ.name]);
      exit;
    end;
  end;
  //stRegister: begin  //To complete.
  //end;
  stRegistA: begin
    _SEC;
    _SBCi(1);
  end;
  stRegistX: begin _DEX; end;
  stRegistY: begin _DEY; end;
  else
    genError('Not implemented "%s()" for operands "%s".',
             [fun.name, par.StoAsStr]);
  end;
end;
procedure SIF_Ord(var fun: TGenOperand);
var
  par: TGenOperand;
begin
  par := (fun.elements[0]);  //Only one parameter
  case par.Sto of
  stConst : begin
    if par.Typ = typChar then begin
      SetFunConst(fun);
      fun.value.valInt := par.value.ValInt;
      fun.value.consType := ctLiteral;  //fun.evaluated := par.evaluated;
    end else if par.Typ = typBool then begin
      SetFunConst(fun);
      if par.value.ValBool then fun.value.ValInt := 0 else fun.value.ValInt := 1;
      fun.value.consType := ctLiteral;  //fun.evaluated := par.evaluated;
    end else begin
      GenError('Cannot get the ordinal of %s.', [par.Typ.name]); exit;
    end;
  end;
  stRamFix: begin
    if par.Typ = typChar then begin
      //Sigue siendo variable
      SetFunVariab(fun, par.add);  //Actualiza "par"
    end else begin
      SetFunExpres(fun);   //A default operand type
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  stRegister: begin  //se asume que ya está en (A)
    if par.Typ = typChar then begin
      //Es la misma expresión, solo que ahora es Byte.
      SetFunExpres(fun);
    end else begin
      SetFunExpres(fun); //Set a default operand type
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
end;
procedure SIF_Chr(var fun: TGenOperand);
var
  par: TGenOperand;
begin
  par := (fun.elements[0]);  //Only one parameter
  case par.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if par.Typ = typByte then begin
      SetFunConst(fun);
      fun.value.consType := ctLiteral; // fun.evaluated := par.evaluated;
      fun.value.valInt := par.value.ValInt;
    end else if par.Typ = typWord then begin
      SetFunConst(fun);
      fun.value.consType := ctLiteral; // fun.evaluated := par.evaluated;
      fun.value.valInt := par.value.valInt and $FF;
    end else begin
      GenError('Cannot convert this to char.'); exit;
    end;
  end;
  stRamFix: begin
    if par.Typ.IsByteSize then begin
      //Sigue siendo variable
      SetFunVariab(fun, par.add);
    end else if par.Typ = typWord then begin
      //Crea variable que apunte al byte bajo
      SetFunVariab(fun, par.add);
    end else begin
      SetFunExpres(fun);   //A default operand type
      GenError('Cannot convert to char.'); exit;
    end;
  end;
  stRegister: begin  //se asume que ya está en (A)
    if par.Typ = typByte then begin
      //Es la misma expresión, solo que ahora es Char.
      SetFunExpres(fun);
    end else if par.Typ = typWord then begin
      //Ya está en A el byte bajo
      SetFunExpres(fun);
    end else begin
      SetFunExpres(fun); //Set a default operand type
      GenError('Cannot convert this to char.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
end;
procedure SIF_Byte(var fun: TGenOperand);
var
  par: TGenOperand;
begin
  par := (fun.elements[0]);  //Only one parameter
  case par.Sto of
  stConst : begin
    if par.Typ = typByte then begin
      //ya es Byte
      SetFunConst(fun);  //It's already byte
      fun.value.valInt := par.value.ValInt;
    end else if par.Typ = typChar then begin
      SetFunConst(fun);  //It's already byte
      fun.value.valInt := par.value.ValInt;
    end else if par.Typ = typWord then begin
      SetFunConst(fun);  //It's already byte
      fun.value.valInt := par.value.valInt and $FF;
    end else begin
      GenError('Cannot convert this to byte.'); exit;
    end;
  end;
  stRamFix: begin
    if compMod = cmConsEval then exit;  //We don't generate constants in this case.
    if par.Typ.IsByteSize then begin
      //Es lo mismo.
      SetFunVariab(fun, par.add);  //Byte type
    end else if par.Typ = typWord then begin
      //Crea variable que apunte al byte bajo
      SetFunVariab(fun, par.add);
    end else begin
      SetFunExpres(fun);   //A default operand type
      GenError('Cannot convert to byte.'); exit;
    end;
  end;
  stRegister: begin  //se asume que ya está en (A)
    if compMod = cmConsEval then exit;  //We don't generate constants in this case.
    if par.Typ.IsByteSize then begin
      //Ya está en A y ya es Byte
      SetFunExpres(fun);
    end else if par.Typ = typWord then begin
      //Ya está en A el byte bajo
      SetFunExpres(fun);
    end else begin
      SetFunExpres(fun); //Set a default operand type
      GenError('Cannot convert this to byte.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
end;
procedure SIF_Word(var fun: TGenOperand);
var
  tmpVar: TAstVarDec;
  par: TGenOperand;
begin
  par := (fun.elements[0]);  //Only one parameter
  case par.Sto of  //El parámetro debe estar en "res"
  stConst : begin
    if par.Typ = typByte then begin
      SetFunConst(fun);
      fun.value.consType := ctLiteral; // fun.evaluated := par.evaluated;
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else if par.Typ = typChar then begin
      SetFunConst(fun);
      fun.value.consType := ctLiteral; // fun.evaluated := par.evaluated;
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else if par.Typ = typWord then begin
      //Already Word
      SetFunConst(fun);
      fun.value.consType := ctLiteral; // fun.evaluated := par.evaluated;
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else begin
      GenError('Cannot convert this constant to word.'); exit;
    end;
  end;
  stRamFix: begin
    if par.Typ.IsByteSize then begin
      SetFunExpres(fun);  //No podemos devolver variable. Pero sí expresión
      _LDAi(0);
      _STA(H.addr);
      _LDA(par.vardec.addr);
    end else if par.Typ = typWord then begin
      //ya es Word
      SetFunVariab(fun, par.add);
    end else if par.Typ.IsWordSize then begin
      //Has 2 bytes long, like pointers
      SetFunVariab(fun, par.add);
      {We could generate stRegister, but we prefer generate a variable, for simplicity
      and to have the possibility of assign: word(x) := ...}
    end else begin
      SetFunExpres(fun);   //A default operand type
      GenError('Cannot convert this variable to word.'); exit;
    end;
  end;
  stRegister: begin  //se asume que ya está en (A)
    if par.Typ = typByte then begin
      SetFunExpres(fun);
      //Ya está en A el byte bajo
      _LDXi(0);
      _STX(H.addr);
    end else if par.Typ = typChar then begin
      SetFunExpres(fun);
      //Ya está en A el byte bajo
      _LDXi(0);
      _STX(H.addr);
    end else if par.Typ = typWord then begin
//      Ya es word
    end else begin
      GenError('Cannot convert expression to word.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
end;
procedure SIF_DWord(var fun: TGenOperand);
var
  tmpVar: TAstVarDec;
  par: TGenOperand;
begin
  par := (fun.elements[0]);  //Only one parameter
  case par.Sto of  //El parámetro debe estar en "res"
  stConst : begin
    if par.Typ = typByte then begin
      SetFunConst(fun);
      fun.value.consType := ctLiteral; // fun.evaluated := par.evaluated;
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else if par.Typ = typChar then begin
      SetFunConst(fun);
      fun.value.consType := ctLiteral; // fun.evaluated := par.evaluated;
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else if par.Typ = typWord then begin
      //Already Word
      SetFunConst(fun);
      fun.value.consType := ctLiteral; // fun.evaluated := par.evaluated;
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else begin
      GenError('Cannot convert this constant to word.'); exit;
    end;
  end;
//  stRamFix: begin
//    if par.Typ.IsByteSize then begin
//      SetFunExpres(fun);  //No podemos devolver variable. Pero sí expresión
//      _LDAi(0);
//      _STA(H.addr);
//      _LDA(par.vardec.addr);
//    end else if par.Typ = typWord then begin
//      //ya es Word
//      SetFunVariab(fun, par.add);
//    end else if par.Typ.IsWordSize then begin
//      //Has 2 bytes long, like pointers
//      SetFunVariab(fun, par.add);
//      {We could generate stRegister, but we prefer generate a variable, for simplicity
//      and to have the possibility of assign: word(x) := ...}
//    end else begin
//      SetFunExpres(fun);   //A default operand type
//      GenError('Cannot convert this variable to word.'); exit;
//    end;
//  end;
//  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
end;
procedure SIF_Addr(var fun: TGenOperand);
{Returns the address of a datatype.}
var
  par: TGenOperand;
begin
  par := (fun.elements[0]);  //Only one parameter
  case par.opType of
  otVariab: begin
    //Es una variable simple. Una variable tiene dirección fija
    if par.Sto = stRamFix then begin
      {This is a special case where the result operand type, depends on if
      par is allocated.}
      if par.allocated then begin
        SetFunConst(fun);
        fun.value.valInt := par.add;
      end else begin
        {No allocated. We keep this as an expression in order to force the
        evaluation later, when the address must be defined.}
        SetFunExpres(fun);
      end;
    end else begin
      genError('Cannot obtain address for variable "%s"',[par.StoAsStr]);
      exit;
    end;
  end;
  otConst: begin
    SetFunConst(fun);
    if (par.typ = typByte) or (par.typ = typWord) then begin
      //For numeric constant, takes the value a as address,
      fun.value.valInt := par.Value.valInt;
    end else begin
      genError('Cannot obtain address this constant.');
      exit;
    end;
  end;
  //otFunct: begin
  { TODO : Faltaría implementar etso, después de que se defina el campo "coded" (o similar) en los TxpEleExpress para indciar cuando la función se ha implementado y codificado en memoria. }
    ////Should be a function call
    //if par.coded then begin
    //  xfun := par.fun;
    //  if xfun.codInline <> nil then begin
    //    //Inline Function
    //    genError('Cannot obtain address of a INLINE function.');
    //    exit;
    //  end else begin
    //    //Normal function
    //    SetResultConst(fun);  //Lo más cercano al POINTER de Pascal o al ADDRESS de Modula-2
    //    if xfun.coded then begin //We have a real address
    //      fun.value.valInt := xfun.adrr;
    //    end else begin
    //      //No tiene dirección. Debe ser forward (o declaración en INTERFACE).
    //      //Por ahora no se implementa. Debe ser algo como xfun.AddAddresPend(pic.iRam-2);
    //      genError('Cannot obtain address this operand.');
    //      exit;
    //    end;
    //
    //  end;
    //end;
  //end;
  else
    //Shouldn't happen
    genError('Design error.');
  end;
end;

//////////// Pointer operations
procedure SIF_pointer_add_byte(var fun: TGenOperand);
{Implementa la suma de un puntero (a cualquier tipo) y un byte.}
var
  ptrType: TGenTypeDec;
  parA: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  ptrType := parA.Typ;   //Se hace aquí porque después puede cambiar parA.
  //La suma de un puntero y un byte, se procesa, como una suma de word y byte.
  SIF_word_add_byte(fun);
  //Devuelve word, pero debe devolver el tipo puntero.
  fun.Typ := ptrType;
//  case fun.Sto of
//  stConst: res.SetAsConst(ptrType);  //Cambia el tipo a la constante
//  //stRamFix: res.SetAsVariab(res.rVar);
//  {Si devuelve variable, solo hay dos posibilidades:
//   1. Que sea la variable puntero, por lo que no hay nada que hacer, porque ya tiene
//      el tipo puntero.
//   2. Que sea la variable Word (y que la otra era constante puntero 0 = nil). En este
//      caso devolverá el tipo Word, lo cual tiene cierto sentido.}
//  stRegister: res.SetAsExpres(ptrType);  //Cambia tipo a la expresión
//  end;
end;
procedure SIF_pointer_add_word(var fun: TGenOperand);
{Implementa la suma de un puntero (a cualquier tipo) y un byte.}
var
  ptrType: TGenTypeDec;
  parA: TGenOperand;
begin
  parA := (fun.elements[0]);  //Parameter A
  ptrType := parA.Typ;   //Se hace aquí porque después puede cambiar parA.
  //La suma de un puntero y un word, se procesa, como una suma de words.
  SIF_word_add_word(fun);
  //Returns word. MUst returns pointer type.
  fun.Typ := ptrType;
//  case fun.Sto of
//  stConst: res.SetAsConst(ptrType);  //Cambia el tipo a la constante
//  //stRamFix: res.SetAsVariab(res.rVar);
//  {Si devuelve variable, solo hay dos posibilidades:
//   1. Que sea la variable puntero, por lo que no hay nada que hacer, porque ya tiene
//      el tipo puntero.
//   2. Que sea la variable Word (y que la otra era constante puntero 0 = nil). En este
//      caso devolverá el tipo Word, lo cual tiene cierto sentido.}
//  stRegister: res.SetAsExpres(ptrType);  //Cambia tipo a la expresión
//  end;
end;
procedure SIF_pointer_sub_byte(var fun: TGenOperand);
{Implementa la resta de un puntero (a cualquier tipo) y un byte.}
var
  ptrType: TGenTypeDec;
  parA: TGenOperand;
begin
  //Similar to SIF_pointer_add_byte
  parA := (fun.elements[0]);  //Parameter A
  ptrType := parA.Typ;
  SIF_word_sub_byte(fun);
  //Returns word. MUst returns pointer type.
  fun.Typ := ptrType;
//  case fun.Sto of
//  stConsta  : res.SetAsConst(ptrType);
//  stRegister: res.SetAsExpres(ptrType);
//  end;
end;
procedure SIF_pointer_sub_word(var fun: TGenOperand);
{Implementa la resta de un puntero (a cualquier tipo) y un byte.}
var
  ptrType: TGenTypeDec;
  parA: TGenOperand;
begin
  //Similar to SIF_pointer_add_word
  parA := (fun.elements[0]);  //Parameter A
  ptrType := parA.Typ;
  SIF_word_sub_word(fun);
  //Returns word. MUst returns pointer type.
  fun.Typ := ptrType;
//  case fun.Sto of
//  stConsta  : res.SetAsConst(ptrType);
//  stRegister: res.SetAsExpres(ptrType);
//  end;
end;
procedure SIF_derefPointer(fun: TAstExpress; SetRes: boolean);
{Implementa el operador de desreferencia "^", para Opr que se supone debe ser
 categoria "tctPointer", es decir, puntero a algún tipo de dato.}
begin
//  case parA.Sto of
//  stConsta : begin
//    //Caso especial. Cuando se tenga algo como: TPunteroAByte($FF)^
//    //Se asume que devuelve una variable de tipo Byte.
//    tmpVar := CreateTmpVar('', typByte);
//    tmpVar.addr := parA.val;  //Fija dirección de constante
//    SetUORResultVariab(fun, tmpVar);
//  end;
//  stRamFix: begin
//    //Caso común: ptrVar^
//    itemType := parA.Typ.ptrType;  //Type of pointed var
//    //By default we generate code as Setter
//    idxVar := CreateTmpVar('', typWord);
//    idxVar.addr := parA.addr;      //Var pointer as word
//    SetUORResultVarRef(idxVar, itemType);
//    //Here the Operand can be stVarRef or stExpRef
//    if OperMode = opmGetter then begin
//      //In mode Getter, we change this to stRegister, because ROP's don't like "strange" storages.
//      //Validation for WR availability, has been done before (It's suposed)
//      LoadToWR(res);  //Load to WR
//      if HayError then exit;
//      res.SetAsExpres(itemType);  //As operand is in WR, it's an expression
//    end;
//  end;
//  stRegister: begin
//    //La expresión Esta en WR, pero es una dirección, no un valor
//    SetUORResultExpRef(parA.Typ);
//  end;
//  else
////////////    genError('Not implemented: "%s"', [Opr.OperationString]);
//  end;
end;

procedure JUMP_IF_Z_pre(Invert, longJump: boolean; igoto: integer);
{Jump using the Z flag. Jump if Z is set.}
begin
  if longJump then begin          //Long jump
      if Invert then begin
        _BEQ(3);
        _JMP(igoto);
      end else begin
        _BNE(3);
        _JMP(igoto);
      end;
  end else begin
      if Invert then begin
        _BNE(igoto - _PC - 2);
      end else begin
        _BEQ(igoto - _PC - 2);
      end;
  end;
end;
procedure JUMP_IF_C_pre(Invert, longJump: boolean; igoto: integer);
{Jump using the C flag. Jump if C is set.}
begin
  if longJump then begin          //Long jump
      if Invert then begin
        _BCS(3);
        _JMP(igoto);
      end else begin
        _BCC(3);
        _JMP(igoto);
      end;
  end else begin
      if Invert then begin
        _BCC(igoto - _PC - 2);
      end else begin
        _BCS(igoto - _PC - 2);
      end;
  end;
end;
procedure JUMP_IF_Z_post(Invert, longJump: boolean; out curAddr: integer);
{Jump using the Z flag. Jump if Z is set.
If "longJump" is set it generates a long jump (more than 128 bytes). }
begin
  if longJump then begin          //Long jump
      if Invert then begin
        _BEQ(3);
        _JMP_post(curAddr);
      end else begin
        _BNE(3);
        _JMP_post(curAddr);
      end;
  end else begin
      if Invert then begin
        _BNE_post(curAddr);
      end else begin
        _BEQ_post(curAddr);
      end;
  end;
end;
procedure JUMP_IF_C_post(Invert, longJump: boolean; out curAddr: integer);
{Jump using the C flag. Jump if C is set.
If "longJump" is set it generates a long jump (more than 128 bytes). }
begin
  if longJump then begin          //Long jump
      if Invert then begin
        _BCS(3);
        _JMP_post(curAddr);
      end else begin
        _BCC(3);
        _JMP_post(curAddr);
      end;
  end else begin
      if Invert then begin
        _BCC_post(curAddr);
      end else begin
        _BCS_post(curAddr);
      end;
  end;
end;
procedure JUMP_IF_pre(OpRes: TAstExpress; boolVal, longJump: boolean;
                                 igoto: integer; out relatOver: boolean);
{Jump to a pre label, if the last operand "OpRes" returned a boolean result equal to
"boolVal".
If "longJump" is set it generates a long jump (more than 128 bytes). }
var
  offset: Integer;
begin
  if longJump then begin
    //In lonj jumps, we won't have overflow
    relatOver := false;
  end else begin
    //For short jumps, we need to verifiy the ffset
    offset := _PC-igoto + 2;
    if offset>127 then begin
      relatOver := true;
      exit;
    end;
  end;
  if OpRes.Sto = stRamFix then begin
    //Result in variable
    _LDA(OpRes.vardec.addr);
    JUMP_IF_Z_pre(boolVal, longJump, igoto);  //We cannot apply optimization
  end else if OpRes.Sto = stRegister then begin
    {We first evaluate the case when it could be done an optimization}
    if lastASMcode = lacCopyCtoA then begin
      //Expression result has been copied from C to A
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check C flag
      JUMP_IF_C_pre(not boolVal, longJump, igoto);
    end else if lastASMcode = lacInvCtoA then begin
      //Expression result has been copied from C to A inverted
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check C flag
      JUMP_IF_C_pre(boolVal, longJump, igoto);
    end else if lastASMcode = lacCopyZtoA then begin
      //Expression result has been copied from Z to A
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JUMP_IF_Z_pre(not boolVal, longJump, igoto);
    end else if lastASMcode = lacInvZtoA then begin
      //Expression result has been copied from Z to A inverted
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JUMP_IF_Z_pre(boolVal, longJump, igoto);
    end else if lastASMcode = lacInvAtoA then begin
      //Expression result has been copied from A to A inverted, and Z reflect the regA boolVal.
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JUMP_IF_Z_pre(not boolVal, longJump, igoto);
    end else begin
      {Cannot be (or should be) optimized }
      if AcumStatInZ then begin
        //Still we can use the optimizaction of testing Z flag
        JUMP_IF_Z_pre(boolVal, longJump, igoto);
      end else begin
        //Operand boolVal in A but not always in Z
        _TAX;  //To update Z
        JUMP_IF_Z_pre(boolVal, longJump, igoto);
      end;
    end;
  end else begin
    genError('Expression storage not supported.');
  end;
end;
procedure JUMP_IF_post(OpRes: TAstExpress; boolVal, longJump: boolean;
                                  out curAddr: integer);
{Jump to a post label, if the last operand "OpRes" returned a boolean result equal to
"boolVal".
If "longJump" is set it generates a long jump (more than 128 bytes). }
begin
  if OpRes.Sto = stRamFix then begin
    //Result in variable
    _LDA(OpRes.vardec.addr);
    JUMP_IF_Z_post(boolVal, longJump, curAddr);  //We cannot apply optimization
  end else if OpRes.Sto = stRegister then begin
    {We first evaluate the case when it could be done an optimization}
    if lastASMcode = lacCopyCtoA then begin
      //Expression result has been copied from C to A
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check C flag
      JUMP_IF_C_post(not boolVal, longJump, curAddr);
    end else if lastASMcode = lacInvCtoA then begin
      //Expression result has been copied from C to A inverted
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check C flag
      JUMP_IF_C_post(boolVal, longJump, curAddr);
    end else if lastASMcode = lacCopyZtoA then begin
      //Expression result has been copied from Z to A
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JUMP_IF_Z_post(not boolVal, longJump, curAddr);
    end else if lastASMcode = lacInvZtoA then begin
      //Expression result has been copied from Z to A inverted
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JUMP_IF_Z_post(boolVal, longJump, curAddr);
    end else if lastASMcode = lacInvAtoA then begin
      //Expression result has been copied from A to A inverted, and Z reflect the regA boolVal.
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JUMP_IF_Z_post(not boolVal, longJump, curAddr);
    end else begin
      {Cannot be (or should be) optimized }
      if AcumStatInZ then begin
        //Still we can use the optimizaction of testing Z flag
        JUMP_IF_Z_post(boolVal, longJump, curAddr);
      end else begin
        //Operand boolVal in A but not always in Z
        _TAX;  //To update Z
        JUMP_IF_Z_post(boolVal, longJump, curAddr);
      end;
    end;
  end else begin
    genError('Expression storage not supported.');
  end;
end;
procedure IF_TRUE(OpRes: TAstExpress; longJump: boolean; out info: TIfInfo);
{Conditional instruction. Test if last expression is TRUE. In this case, execute
the following block. The syntax is:

IF_TRUE(OpRes, info)
  <block of code>
IF_END(info)

This instruction require to call to IF_END() to define the End of the block.

The block of code can be one or more instructions.
}
begin
  JUMP_IF_post(OpRes, false, longJump, info.igoto);
end;
procedure IF_END(const info: TIfInfo; out relatOver: boolean);
{Define the End of the block, created with IF_TRUE().
Note the similarity with _LABEL_post().}
var
  offset, igoto: integer;
begin
  igoto := info.igoto;
  relatOver := false;
  if pic.ram[igoto].value = 0 then begin
    //Es salto absoluto
    pic.ram[igoto].value   := lo(_PC);
    pic.ram[igoto+1].value := hi(_PC);
  end else begin
    //Es salto relativo. Salto hacia adelante
    offset := _PC - igoto-1;
    if offset>127 then begin
      relatOver := true;
      exit;
    end;
    pic.ram[igoto].value := offset;
  end;
end;
procedure BRA2JMP(var info: TIfInfo);
{Change a relative jump (BEQ, BNE, BCS o BCC) of the form:
  BNE <offset>
To an absolute (long) jump:
  BEQ <+3>
  JMP <offset>
To generate the new code, the all jump instruction if overwritten and the pointer "iram"
is set to the next free position after the new junp instructions.
The IN/OUT parameter "info" give the addres (info.igoto) of the parameter of the relative
jump (BNE).
Field "info.igoto" will be set to the absolute jump after finishing this
procedure.}
var
  ramcell: TCPURamCellPtr;
begin
  ramcell := @pic.ram[info.igoto-1];  //Read Jump Opcode
  pic.iRam := info.igoto-1;    //Go to the start of the Opcode
  if (ramcell^.value = $F0) then begin  //BEQ
    _BNE(3);
    _JMP_post(info.igoto);
  end else if (ramcell^.value = $D0) then begin  //BNE
    _BEQ(3);
    _JMP_post(info.igoto);
  end else if (ramcell^.value = $90) then begin  //BCC
    _BCS(3);
    _JMP_post(info.igoto);
  end else if (ramcell^.value = $B0) then begin  //BCS
    _BCC(3);
    _JMP_post(info.igoto);
  end else begin
    GenError('Unsupported branch Opcode.');
  end;
end;

procedure GenCodLoadToA(fun: TAstExpress);
begin
  if fun.Typ.IsByteSize then begin
    case fun.Sto. of
    stConst: begin
      _LDAi(fun.value.valInt and $ff);
    end;
    stRamFix: begin
      _LDA(fun.vardec.addr);
    end;
    stRegister: begin
      //Already in A
    end
    else
      GenError('Cannot load this operand to register A.');
    end;
  end else begin
    GenError('Operand must be byte-size to fit in register A.');
  end;
end;
procedure GenCodLoadToX(fun: TAstExpress);
begin
  if fun.Typ.IsByteSize then begin
    case fun.Sto. of
    stConst: begin
      _LDXi(fun.value.valInt and $ff);
    end;
    stRamFix: begin
      _LDX(fun.vardec.addr);
    end;
    stRegister: begin
      _TAX_opt;
    end
    else
      GenError('Cannot load this operand to register X.');
    end;
  end else begin
    GenError('Operand must be byte-size to fit in register Y.');
  end;
end;
procedure GenCodLoadToY(fun: TAstExpress);
begin
  if fun.Typ.IsByteSize then begin
    case fun.Sto. of
    stConst: begin
      _LDYi(fun.value.valInt and $ff);
    end;
    stRamFix: begin
      _LDY(fun.vardec.addr);
    end;
    stRegister: begin
      _TAY;
    end
    else
      GenError('Cannot load this operand to register Y.');
    end;
  end else begin
    GenError('Operand must be byte-size to fit in register Y.');
  end;
end;
function CompilerName: string;
begin
  Result := 'P65Pas Compiler'
end;
function RAMmax: integer;
begin
   Result := high(pic.ram);
end;
procedure functCall(xfun: TMirFunDec; out AddrUndef: boolean);
{General routine to make the call to a Normal function.}
begin
  {*** Completar luego
  //////// Make the CALL
  AddrUndef := false;
  //In Code Generation (when executing this) it's supposed all functions are implemented.
  if xfun.coded then begin
    //We have a real address
    _JSR(xfun.adrr);  //It's a complete function
  end else begin
    //Function is not yet coded. We need to complete this call later.
    _JSR($0000);
    if not pic.disableCodegen then begin  //Verify if we are in mode no-code-generation.
      xfun.AddAddresPend(pic.iRam-2);  //Register the address to complete later
    end;
    AddrUndef := true;
  end;
  }
end;
procedure codRTS(isInterrupt: boolean);
{Encodes a RTS or RTI instruction.}
begin
  if isInterrupt then _RTI else _RTS;
end;
procedure GenCodeExpr(eleExp: TAstExpress);
{Generate code for a node expression. Some expression (like operation in constant) may
not generate code. This is a recursive procedure.
Nodes otConst, must be evaluated.}
var
  funcBase: TAstFunDec;
  AddrUndef, regsUsed: boolean;
  ele: TAstElement;
  parExpr: TAstExpress;
begin
//  if eleExp.opType = otFunct then begin
//    //It's an expression. There should be a function
//    funcBase := eleExp.fundec;
//    if funcBase.callType = ctSysInline then begin
//      //It's an INLINE function. It could be already implemented or not.
//      if funcBase.idClass = eleFuncDec then begin
//        //It's the implementation. No problem.
//        regsUsed := false;  //Set flag to indicate Registers are not used.
//        //Check first if it's needed to evaluate parameters.
//        for ele in eleExp.elements do begin
//          parExpr := TAstExpress(ele);
//          GenCodeExpr(parExpr);
//          if HayError then exit;
//          //Check availability of registers
//          if parExpr.opType = otFunct then begin
//            //An Expression result use registers.
//            if regsUsed = false then begin
//              regsUsed := true;  //Set to used.
//            end else begin
//              //Register are already used.
//              GenError('Too complex expression.', parExpr.srcDec);
//              exit;
//            end;
//          end;
//        end;
//        funcBase.codSysInline(eleExp);   //Process function
//        //Check if we can simplify
//        if eleExp.opType = otConst then begin
//          //Node resulted as a constant.
//          if eleExp.evaluated then begin
//            //We convert it to a simple constant (Constant fold).
//            eleExp.elements.Clear;  //Constants don't have childrens.
//          end;
//        end else if eleExp.opType = otVariab then begin
//          eleExp.elements.Clear;  //Variables don't have childrens.
//        end;
//      end else begin
//        { Los SIF no soportan implementación separada.}
//        GenError('No supported implementing System INLINE functions.');
//      end;
//    end else if funcBase.callType in [ctSysNormal, ctUsrNormal] then begin
//      //Should be a Normal subroutine. Generates the CALL instruction.
//      {Even though this is a Normal function, we can consider functCall() like the
//      INLINE routine callback to this function, like codInline.}
//      functCall(funcBase, AddrUndef);
//      SetFunExpres(eleExp);
//    end else begin
//      GenError('Unsupported.');
//    end;
//  end else if eleExp.opType = otConst then begin
//    //A constant expression. We have to evaluate it, if not already evaluated.
//    eleExp.Evaluate();  //Just in case
//    if not eleExp.evaluated then begin
//      //It's a simple constant
//      GenError('Constant not evaluated.', eleExp.srcDec);
//      exit;
//    end;
//  end else if eleExp.opType = otVariab then begin
//    //We don't need to generate code for this.
//  end else begin
//    GenError('Design error.');
//  end;
end;
procedure GenCodeASMline(asmInst: TAstAsmInstr);
{Generate code for an ASM instruction (element TAstAsmInstr).}
  function ReadOperandValueRef(paramRef: TAstElement): integer;
  {Read the value of a Operand when it's a reference to an element.}
  var
    xvar: TAstVarDec;
    xcon: TAstConsDec;
    xfun: TAstFunDec;
    instTarget: TAstAsmInstr;
  begin
    Result := 0;
    if paramRef.idClass = eleVarDec then begin
      xvar := TAstVarDec(paramRef);
      if not xvar.allocated then begin
        GenError('Variable not allocated.');
        exit;
      end;
      Result := xvar.addr;
    end else if paramRef.idClass = eleConsDec then begin
      xcon := TAstConsDec(paramRef);
      Result := xcon.value^.ValInt;
    end else if paramRef.idClass = eleFuncDec then begin
      xfun := TAstFunDec(paramRef);
      if not xfun.coded then begin
        GenError('Function not coded.');
        exit;
      end;
      Result := xfun.adrr;
    end else if paramRef.idClass = eleAsmInstr then begin
      //Referencia a una instrucción ASM. Tal vez una etiqueta o DB, DW
      instTarget := TAstAsmInstr(paramRef);  //Instrucción destino
      if instTarget.addr=-1 then begin
        //La etiqueta aún no ha sido mapeada en memoria
        {Define una posición tentativa, considerando que la etiqueta referenciada debe
        estar más adelante. Luego se completará cuando se defina la etiqueta.}
        Result := pic.iRam+3;
      end else begin
        Result := instTarget.addr;  //Toma su dirección.
      end;
    end else begin
      GenError('Invalid Opcode operand.');
      exit;
    end;
  end;
  procedure ApplyOperations(operRef: TAstElement; const operations: TAsmOperations; var operVal: integer);
  {Apply the operations to the parameter "operVal"}
  var
    i: Integer;
    operat: TAsmOperation;
  begin
    for i:=0 to high(operations) do begin
      operat := operations[i];
      case operat.oper of
      aopAddValue: begin
        operVal += operat.value;
      end;
      aopSubValue: begin
        operVal -= operat.value;
      end;
      aopSelByte: begin
        case operat.value of
        0:  //Low byte
          operVal := operVal and $ff;
        1:  //High byte
          if operRef = nil then  begin
            //No hay referencia a operando.
            operVal := (operVal and $ff00)>>8
          end else if operRef.idClass = eleConsDec then  begin
            //En constantes tomamos el byte alto
            operVal := (operVal and $ff00)>>8
          end else begin
            //Para variables o funciones, tomamos la siguiente dirección
            //operVal := operVal+1;
            operVal := (operVal and $ff00)>>8
          end;
        end;
      end;
      end;
    end;
  end;
  procedure ReadOperandValue(const asmOperand: TAsmOperand; out operVal: integer);
  {Read the value of an instruction Operand in "operVal".
  "operRef" returns the reference to the element when operand is an "element operand",
  otherwise returns NIL.}
  var
    elemRef: TAstElement;
  begin
    if (asmOperand.Val = -1) then begin
      //There is an expresion for the operand. We need to solve the parameter.
      elemRef := asmOperand.Ref;
      //Resolve operand value
      operVal := ReadOperandValueRef(elemRef);
      if HayError then exit;
    end else if (asmOperand.Val = -2) then begin
      //Operand is '$'
      elemRef := nil;
      operVal :=  pic.iRam;
    end else begin
      //Operand can be read directly
      elemRef := nil;
      operVal := asmOperand.Val;
    end;
    //Validates possible operations to the operand
    ApplyOperations(elemRef, asmOperand.operations, OperVal);
  end;
  procedure WriteInstruction(cpu_inst: TP6502Inst; cpu_amod: TP6502AddMode;
                             param, param2: integer);
  {Codifica la instrucción a partir de la posiicón actual de la RAM.
  Se debe haber ya definido: "param" }
  var
    addressModes: TP6502AddModes;
    offset: Integer;
  begin
    addressModes := PIC16InstName[cpu_inst].addressModes;
    //debugln('iRam = ' + IntToStr(pic.iRam));
    if cpu_amod = aRelative then begin  //Instrucciones de salto relativo
      offset := param-pic.iRam-2;
      { TODO : Validar si el salto es mayor a 127 o menor a -128 }
      pic.codAsm(cpu_inst, aRelative, word(offset));
    end else if cpu_amod = aZeroPRel then begin
      offset := param2-pic.iRam-2;
      { TODO : Validar si el salto es mayor a 127 o menor a -128 }
      pic.codAsm(cpu_inst, aZeroPRel, param, word(offset));
    end else if (param<256) then begin
      //It could be expressed as zero-page instruction
      if (cpu_amod = aAbsolute) and (aZeroPage in addressModes) then begin
        pic.codAsm(cpu_inst, aZeroPage, param);
      end else if (cpu_amod = aAbsolutX) and (aZeroPagX in addressModes) then begin
        pic.codAsm(cpu_inst, aZeroPagX, param);
      end else if (cpu_amod = aAbsolutY) and (aZeroPagY in addressModes) then begin
        pic.codAsm(cpu_inst, aZeroPagY, param);
      end else if (cpu_amod = aIndirect) and (aIndirecZP in addressModes) then begin
        pic.codAsm(cpu_inst, aIndirecZP, param);
      end else begin
        pic.codAsm(cpu_inst, cpu_amod, param);
      end;
    end else begin
      pic.codAsm(cpu_inst, cpu_amod, param);
    end;
  end;
var
  cpu_inst  : TP6502Inst;
  cpu_amod  : TP6502AddMode;
  operandVal, operandVal2: Integer;
begin
  if asmInst.iType = itOpcode then begin   //Instrucción normal.
    pic.MsjError := '';
    //Calculate the final Opcode operand parameter.
    if asmInst.operand.used then ReadOperandValue(asmInst.operand, operandVal);
    if asmInst.operand2.used then ReadOperandValue(asmInst.operand2, operandVal2);
    //Write the instruction
    asmInst.addr := pic.iRam;   //Set address
    cpu_inst := TP6502Inst(asmInst.opcode);
    cpu_amod := TP6502AddMode(asmInst.addMode);
    WriteInstruction(cpu_inst, cpu_amod, operandVal, operandVal2);
    if pic.MsjError <> '' then begin
      GenError(pic.MsjError);
      exit;
    end;
    lastASMLabel := '';
  end else if asmInst.iType = itLabel then begin  //Instrucción etiqueta.
    asmInst.addr := pic.iRam;   //Actualiza dirección actual
    lastASMLabel := asmInst.name;
  end else if asmInst.iType = itOrgDir then begin  //Instrucción ORG.
    //Calculate the final Opcode operand parameter.
    ReadOperandValue(asmInst.operand, operandVal);
    pic.iRam := operandVal;   //Actualiza dirección actual
    lastASMLabel := '';
  end else if asmInst.iType = itDefByte then begin  //Instrucción DB.
    //Calculate the final Opcode operand parameter.
    ReadOperandValue(asmInst.operand, operandVal);
    pic.codByte(operandVal and $ff, ruData, lastASMLabel);
    lastASMLabel := '';
  end else if asmInst.iType = itDefWord then begin  //Instrucción DW.
    //Calculate the final Opcode operand parameter.
    ReadOperandValue(asmInst.operand, operandVal);
    pic.codByte(operandVal and $ff, ruData, lastASMLabel);
    pic.codByte((operandVal >> 8) and $ff, ruData, '');
    lastASMLabel := '';
  end else begin
    //It's not an instruction
    GenError('Inalid ASM instruction.');
    exit;
  end;
end;
function GenCodeCodition(cond: TAstElement): TAstExpress;
{Generates code for a condition Block.
Returns the boolean expression inside the condition.
If an Error occurs returns FALSE.
There should be at least one Expression in "cond" and "cond" must be a TEleCondit
element. We won't check here.}
var
  expSet: TAstExpress;
  ele: TAstElement;
begin
  //The last expression should be the boolean condition
  Result := TAstExpress(cond.elements[cond.elements.Count-1]);
  //Boolean type has been checked in Analysis.
  for ele in cond.elements do begin
    expSet := TAstExpress(ele);  //Takes assigment function or the last expression.
    GenCodeExpr(expSet);
    if HayError then exit;
  end;
end;
procedure GenCodeSentences(sentList: TAstElements);
{Generate code for a list of sentences.}
var
  eleSen, ele: TAstElement;
  sen: TAstSentence;
  expSet: TAstExpress;
  inst: TAstAsmInstr;
  asmBlock: TAstAsmBlock;
  idCtx, rowCtx, tmp: Integer;
  srcLin: String;
  blk: TAstBlock;
begin
  { *** Completar luego
//  ShowContexts;
//  ShowCurContInformat;
  for eleSen in sentList do begin
    if eleSen.idClass = eleSenten then begin
      //Generates code to the sentence.
      sen := TAstSentence(eleSen);
      if asmIncComm then begin
        {Genera los comentarios por instrucción, accediendo al contenido del
        código fuente a través del contexto al que apunta cada instrucción. }
        idCtx  := sen.srcDec.idCtx;
        rowCtx := sen.srcDec.row-1;
        srcLin := ctxList[idCtx].curLines[rowCtx];  {Podría fallar si el contenido del
         archivo no se encuentra en "curLines". El scanner podría usar otro almacenamiento.
         Habría que analizar mejor cuál es el acceso correcto al contenido fuente.}
        pic.addTopComm('    ;' + trim(srcLin));
        //MsgBox(srcLin);
      end;
      //Identifica a la sentencia
      case sen.sntType of
      sntAssign: begin  //Assignment
        for ele in sen.elements do begin
          expSet := TAstExpress(ele);  //Takes assigment function.
          GenCodeExpr(expSet);
          if HayError then exit;
        end;
      end;
      sntProcCal: begin  //Call to function or method
        for ele in sen.elements do begin
          expSet := TAstExpress(ele);  //Takes assigment function.
          GenCodeExpr(expSet);
        end;
      end;
      sntAsmBlock: begin
        asmBlock := TAstAsmBlock(sen.elements[0]);  //Takes root node.
        for ele in asmBlock.elements do begin
          inst := TAstAsmInstr(ele);
          GenCodeASMline(inst);
        end;
        //Remains to complete uncomplete instructions
        tmp := pic.iRam;  //Save
        for inst in asmBlock.undefInstrucs do begin
          pic.iRam := inst.addr;   //Set at its original RAM position
          GenCodeASMline(inst);    //Overwrite the code to complete
          { TODO : Sería mejor analizar si podría darse el caso de que la nueva instrucción
          tenga un tamaño diferente a la grabada anteriormente. De ser así, habría
          un grave error. }
        end;
        pic.iRam := tmp;   //Restore
      end;
      sntIF: begin
        GenCondeIF(sen);
      end;
      sntWHILE: begin
        GenCodeWHILE(sen);
      end;
      sntFOR: begin
        GenCodeFOR(sen);
      end;
      sntREPEAT: begin
        GenCodeREPEAT(sen);
      end;
      sntExit: begin
        GenCodeExit(sen);
      end;
      else
        GenError('Unknown sentence type.');
        exit;
      end;
      if HayError then exit;
    end else if eleSen.idClass = eleBlock then begin
      blk := TAstBlock(eleSen);
      GenCodeBlock(blk);
    end else begin
      GenError('Sentence expected.');
      exit;
    end;
  end;
  }
end;
procedure GenCodeSentences2(sentList: TMirElements);
{Generate code for a list of sentences.}
var
  eleSen, ele: TMirElement;
  expSet: TAstExpress;
  inst: TAstAsmInstr;
  asmBlock: TAstAsmBlock;
  idCtx, rowCtx, tmp: Integer;
  srcLin: String;
  blk: TAstBlock;
  misAsg: TMirAssign;
begin
//  ShowContexts;
//  ShowCurContInformat;
  for eleSen in sentList do begin
    //Generates code to the sentence.
//    if asmIncComm then begin
//      {Genera los comentarios por instrucción, accediendo al contenido del
//      código fuente a través del contexto al que apunta cada instrucción. }
//      idCtx  := sen.srcDec.idCtx;
//      rowCtx := sen.srcDec.row-1;
//      srcLin := ctxList[idCtx].curLines[rowCtx];  {Podría fallar si el contenido del
//       archivo no se encuentra en "curLines". El scanner podría usar otro almacenamiento.
//       Habría que analizar mejor cuál es el acceso correcto al contenido fuente.}
//      pic.addTopComm('    ;' + trim(srcLin));
//      //MsgBox(srcLin);
//    end;
    //Identifica a la sentencia
    case eleSen.mirType  of
    mtyAssign: begin  //Assignment
      misAsg := TMirAssign(eleSen);
//      if misAsg.dest.opType = otFunct;
//        if HayError then exit;
//      end;
    end;
//    sntProcCal: begin  //Call to function or method
//      for ele in sen.elements do begin
//        expSet := TAstExpress(ele);  //Takes assigment function.
//        GenCodeExpr(expSet);
//      end;
//    end;
//    sntAsmBlock: begin
//      asmBlock := TAstAsmBlock(sen.elements[0]);  //Takes root node.
//      for ele in asmBlock.elements do begin
//        inst := TAstAsmInstr(ele);
//        GenCodeASMline(inst);
//      end;
//      //Remains to complete uncomplete instructions
//      tmp := pic.iRam;  //Save
//      for inst in asmBlock.undefInstrucs do begin
//        pic.iRam := inst.addr;   //Set at its original RAM position
//        GenCodeASMline(inst);    //Overwrite the code to complete
//        { TODO : Sería mejor analizar si podría darse el caso de que la nueva instrucción
//        tenga un tamaño diferente a la grabada anteriormente. De ser así, habría
//        un grave error. }
//      end;
//      pic.iRam := tmp;   //Restore
//    end;
//    sntIF: begin
//      GenCondeIF(sen);
//    end;
//    sntWHILE: begin
//      GenCodeWHILE(sen);
//    end;
//    sntFOR: begin
//      GenCodeFOR(sen);
//    end;
//    sntREPEAT: begin
//      GenCodeREPEAT(sen);
//    end;
//    sntExit: begin
//      GenCodeExit(sen);
//    end;
    else
      GenError('Unknown sentence type.');
      exit;
    end;
    if HayError then exit;
  end;
end;

procedure GenCodeBlock(block: TAstBlock);
{Do code generation for the body element specified. }
begin
  {*** Completar luego
  if block.idClass <> eleBlock then begin
    GenError('Internal error. Block expected.');
    exit;
  end;
  TreeElems.OpenElement(block);
  GenCodeSentences(TreeElems.curNode.elements);
  }
end;

procedure GenCondeIF(sen: TAstSentence);
var
  expBool: TAstExpress;
  i: Integer;
  lbl1: TIfInfo;
  //Variables for jumps completion.
  njumps: integer;
  jumps: array of integer;
  relatOver: boolean;
begin
  njumps := 0;
  SetLength(jumps, njumps);
  i:=0;
  while i<sen.elements.Count do begin
    //Takes condition
    expBool := GenCodeCodition(sen.elements[i]);
    if HayError then exit;
    if (expBool.opType = otConst) then begin
      //Constant conditions have special behaviour.
      if (expBool.value.ValBool=false) then begin
        i+=2;  //Not processed
      end else begin
        //True expressions are the last executed.
        GenCodeBlock(TAstBlock(sen.elements[i+1]));
        break;  //No more is executed.
      end;
    end else begin
      //Not constant expressions.
      //Check if we need to create space for a jump.
      if i+2<sen.elements.Count then begin  //There are more conditions.
        //We creates space for a JMP instruction
        inc(njumps);
        SetLength(jumps, njumps);  //Create new jump address
      end;
      //Creates the tentative conditional using short jumps
      IF_TRUE(expBool, false, lbl1);
      GenCodeBlock(TAstBlock(sen.elements[i+1]));
      if i+2<sen.elements.Count then begin  //There are more conditions.
        //We need to include a jump to the end
        _JMP_post(jumps[njumps-1]); //New jump to complete later
      end;
      IF_END(lbl1, relatOver);
      if relatOver then begin
        //GenError('Block to long.', sen.srcDec);
        //Block to long for short jump. We recompile using a long block.
        BRA2JMP(lbl1);  //We cannot use IF_TRUE() again because probably IF_TRUE() has made some optimization (delete Opcodes) before of generate the jump instruction.
        GenCodeBlock(TAstBlock(sen.elements[i+1]));
        if i+2<sen.elements.Count then begin  //There are more conditions.
          //We need to include a jump to the end
          _JMP_post(jumps[njumps-1]); //New jump to complete later
        end;
        IF_END(lbl1, relatOver);
      end;
      i+=2;
    end;
  end;
  //Complete jumps
  for i:=0 to high(jumps) do begin
    _LABEL_post(jumps[i]);
  end;
end;
procedure GenCodeWHILE(sen: TAstSentence);
{Compila una extructura WHILE}
var
  lbl1: Word;
  expBool: TAstExpress;
  lbl2: TIfInfo;
  relatOver: boolean;
begin
  lbl1 := _PC;        //guarda dirección de inicio
  expBool := GenCodeCodition(sen.elements[0]);
  if HayError then exit;
  //Aquí debe estar el cuerpo del "while"
  if (expBool.opType = otConst) then begin
    if (expBool.value.ValBool=false) then begin
      //We don't need to process body.
    end else begin
      //Infinite loop
      GenCodeBlock(TAstBlock(sen.elements[1]));
      _JMP(lbl1);
    end;
  end else begin  //otVariab. otFunct
    IF_TRUE(expBool, false, lbl2);
    GenCodeBlock(TAstBlock(sen.elements[1]));
    _JMP(lbl1);   //salta a evaluar la condición
    IF_END(lbl2, relatOver);
    if relatOver then begin
      //GenError('Block to long.');
      BRA2JMP(lbl2);
      GenCodeBlock(TAstBlock(sen.elements[1]));
      _JMP(lbl1);   //salta a evaluar la condición
      IF_END(lbl2, relatOver);
    end;
    //ya se tiene el destino del salto
    //_LABEL_post(dg);   //Termina de codificar el salto
  end;
end;
procedure GenCodeFOR(sen: TAstSentence);
var
  assign, ele: TAstElement;
  expSet, expBool: TAstExpress;
  lbl1: Word;
  lbl2: TIfInfo;
  relatOver: boolean;
begin
  //Generate code for the assigment
  assign := sen.elements[0];
  for ele in assign.elements do begin
    expSet := TAstExpress(ele);  //Takes assigment function.
    GenCodeExpr(expSet);
  end;
  //Condition
  lbl1 := _PC;        //guarda dirección de inicio
  expBool := GenCodeCodition(sen.elements[1]);
  if HayError then exit;
  //Aquí debe estar el cuerpo del "for"
  if (expBool.opType = otConst) then begin
    if (expBool.value.ValBool=false) then begin
      //We don't need to process body.
    end else begin
      //Infinite loop
      GenCodeBlock(TAstBlock(sen.elements[2]));
      _JMP(lbl1);
    end;
  end else begin  //otVariab, otFunct
    IF_TRUE(expBool, false, lbl2);
    GenCodeBlock(TAstBlock(sen.elements[2]));
    _JMP(lbl1);   //salta a evaluar la condición
    IF_END(lbl2, relatOver);
    if relatOver then begin
      //GenError('Block to long.');
      BRA2JMP(lbl2);
      GenCodeBlock(TAstBlock(sen.elements[2]));
      _JMP(lbl1);   //salta a evaluar la condición
      IF_END(lbl2, relatOver);
    end;
  end;

end;
procedure GenCodeREPEAT(sen: TAstSentence);
var
  lbl1: Word;
  expBool: TAstExpress;
  relatOver: boolean;
begin
  lbl1 := pic.iRam;        //guarda dirección de inicio
  //Compile Body
  GenCodeBlock(TAstBlock(sen.elements[0]));
  //Compile condiiton
  expBool := GenCodeCodition(sen.elements[1]);
  if HayError then exit;
  if (expBool.opType = otConst) then begin
    if (expBool.value.ValBool=true) then begin
      //A common block.
    end else begin
      //Infinite loop
      _JMP(lbl1);
    end;
  end else begin  //otVariab. otFunct
    JUMP_IF_pre(expBool, false, false, lbl1, relatOver);
    if relatOver then begin
      //Let's use long jumps
//      GenError('Block too long.', sen.srcDec);
//      exit;
      pic.iRam := lbl1; //Lets to the begin to compile loop again.
      //Compile Body
      GenCodeBlock(TAstBlock(sen.elements[0]));
      //Compile condition
      expBool := GenCodeCodition(sen.elements[1]);
      JUMP_IF_pre(expBool, false, true, lbl1, relatOver);
    end;
  end;
end;
procedure GenCodeExit(sen: TAstSentence);
{Se debe dejar en los registros de trabajo, el valor del parámetro indicado.}
var
  curFun: TAstFunImp;
  par, expSet: TAstExpress;
  parentNod: TAstElement;
  ele: TAstElement;
begin
//  //TreeElems.curNode, debe ser de tipo "Body".
//  if sen.elements.Count=0 then begin
//    //There isn't an expression.
//    _RTS;
//  end else begin
//    //There is an expression.
//    //It's supposed to be a function. We don't validate here. It's been done in Analyze.
//    parentNod := TreeElems.curCodCont.Parent;
//    if parentNod.idClass <> eleFuncImp then begin  //Shouldn't happen
//      GenError('Design error.');
//      exit;
//    end;
//    curFun := TAstFunImp(parentNod);
//    //Generate code for evaluating all possible expressions
//    for ele in sen.elements do begin
//      expSet := TAstExpress(ele);  //Takes assigment function or the last expression.
//      GenCodeExpr(expSet);
//      if HayError then exit;
//    end;
//    //par := TAstExpress(sen.elements[0]);  //Only one parameter
//    par := TAstExpress(sen.elements[sen.elements.Count-1]);  //The last expression
//    //El resultado de la expresión está en "par".
//    LoadToWR(par);  //Carga expresión en WR y genera RTS
//    _RTS;
//  end;
end;

procedure Invert_A_to_A;
{Invert all the bits of A register (as boolean expression) .
If A=$00 => A = $FF
If A=$FF => A = $00
}
begin
  lastASMcode := lacInvAtoA;  //Activates flag
  lastASMaddr := _PC;  //Get current address.
  _EORi($FF); //Invert bits
end;
procedure Copy_Z_to_A;
{Copy the logic value of Z flag to A register (as boolean expression) .
If Z=0 => A = $00
If Z=1 => A = $FF
}
begin
  //Result in Z. Move to A.
  lastASMcode := lacCopyZtoA;  //Activates flag
  lastASMaddr := _PC;  //Get current address.
  _BEQ(2);  //If Z=1: regA = 0
  _LDAi($FF);
  _EORi($FF);  // Invert A
end;
procedure Invert_Z_to_A;
{Copy the logic value of Z flag (inverted) to A register (as boolean expression) .
If Z=1 => A = $00
If Z=0 => A = $FF
}
begin
  //Result in Z. Move to A.
  lastASMcode := lacInvZtoA;  //Activates flag
  lastASMaddr := _PC;  //Get current address.
  _BEQ(2);    //If Z=1: regA = 0
  _LDAi($FF);
end;
procedure Copy_C_to_A;
{Copy the logic value of C flag to A register (as boolean expression).
If C=0 => A = $00
If C=1 => A = $FF
}
begin
  lastASMcode := lacCopyCtoA;  //Activates flag
  lastASMaddr := _PC;  //Get current address.
//  _PHP;
//  _PLA;
//  _ANDi($01);
//  _ASLa;  //Leaves in bit 1.
  _LDAi($FF); //Doesn't change bit C
  _BCS(2);  //If C=1
  _EORi($00);
end;
procedure Invert_C_to_A;
{Copy the logic value of C flag (inverted) to A register (as boolean expression).
If C=0 => A = $FF
If C=1 => A = $00
}
begin
  lastASMcode := lacInvCtoA;  //Activates flag
  lastASMaddr := _PC;  //Get current address.
  _LDAi($00); //Doesn't change bit C
  _BCS(2);  //If C=1
  _EORi($FF);
end;
function Invert(fun: TGenOperand): boolean;
{Convert a boolean operand in the negative form, changing its constant value (if it's a
constant operand) or modifying the generated code (if it's a register operand).
If cannot invert the operand, returns FALSE.
}
begin
  if fun.Sto = stConst then begin
    //In constants, we can change the value.
    fun.SetCon_Literal(not fun.value.valBool);
  end else if fun.Sto = stRegister then begin
    if lastASMcode = lacCopyZtoA then begin
      pic.iRam := lastASMaddr;   //Delete last instructions
      Invert_Z_to_A;
    end else if lastASMcode = lacCopyCtoA then begin
      pic.iRam := lastASMaddr;   //Delete last instructions
      Invert_C_to_A;
    end else if lastASMcode = lacInvCtoA then begin
      pic.iRam := lastASMaddr;   //Delete last instructions
      Copy_C_to_A;
    end else if lastASMcode = lacInvAtoA then begin
      pic.iRam := lastASMaddr;   //Delete last instructions
      lastASMcode := lacNone;
    end else begin
      //We could add here other types or negations.
      exit(false);
    end;
  end else begin
    exit(false);
  end;
  exit(true);
end;

procedure AddLocVar(var pars: TAstParamArray; parName: string; const srcPos: TSrcPos;
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
  pars[n].isLocVar := true;
end;

procedure DoGenerateCode;
{Generates the final binary code using information from the AST as input.
Must be called after DoOptimize().}
begin
{*** Completar luego
  procedure GenCodeMainBody(body: TAstBody);
  {Generates code for a Main Body element.}
  begin
    //It's the main program
    PutLabel('__main__');
    //Process body
    TreeElems.OpenElement(body); //Locate in the Body. Formally this won't be necessary if we are not going to solve identifiers.
    GenCodeSentences(TreeElems.curNode.elements);
    TreeElems.CloseElement;              //Close the Body.
    //Ending label
    PutLabel('__end__');
    //{ TODO : Considerar incluir este código de verificación. }
    //  if pic.MsjError<>'' then begin //Puede ser error al escribir la última instrucción
    //    GenError(pic.MsjError);
    //    exit;
    //  end;
  end;
  procedure GenCodeFunction(body: TAstBody);
  {Generates code for a function element.}
  var
    isInt: boolean;
    funcPar: TAstFunBase;
  begin
    PutLabel('__' + body.Parent.name);
    funcPar := TAstFunBase(body.Parent);  //Parent function
    isInt := funcPar.IsInterrupt;  //Update flag
    //Process body
    TreeElems.OpenElement(body); //Locate in the Body.
    TreeElems.curCodCont := body;  //Needed because TreeElems.OpenElement() doesn't do it.
    GenCodeSentences(TreeElems.curNode.elements);
    TreeElems.CloseElement;              //Close the Body.
    //Includes the final RTS
    if OptRetProc then begin  //Optimize
      //Verifica es que ya se ha incluido exit().
      if funcPar.firstObligExit<>nil then begin
        //Ya tiene un exit() obligatorio y en el final (al menos eso se espera)
        //No es necesario incluir el RTS().
      end else begin
        //No hay un exit(), seguro
        codRTS(isInt);  //RTS instruction
      end;
    end else begin  //Always include
      codRTS(isInt);  //RTS instruction
    end;
  end;
  procedure GenBootloader(out add1, add2: word);
  {Generates the bootloader. Returns in "add1" and "add2" the start address and the end
  address of the bootloader;}
  var
    i: Integer;
  begin
    add1 := pic.iRam;
    if          bootloader = bldNone then begin
      //No bootloader
    end else if bootloader = bldJMP then begin
      pic.codByte(76, ruCodeOp);  //Opcode JMP
      pic.codByte(0, ruData, 'COD_HL');   //To complete later
      pic.codByte(0, ruData);             //To complete later
    end else if bootloader = bldC64 then begin
      //GenBootloaderC64;    //Commodore 64 bootloader.
      PutTopComm(';BASIC starter code: 10 SYS __main__');
      pic.codByte($0C, ruData);  //Dirección de siguiente línea
      pic.codByte($08, ruData);
      pic.codByte($0A, ruData);  //Número de línea
      pic.codByte($00, ruData);
      pic.codByte($9e, ruData);  //Token de instrucción SYS
      pic.codByte(0, ruData, 'COD_4A'); //To complete later
      pic.codByte(0, ruData);           //To complete later
      pic.codByte(0, ruData);           //To complete later
      pic.codByte(0, ruData);           //To complete later
      pic.codByte($00, ruData);  //Fin de instrucción
      pic.codByte($00, ruData);  //Sgte línea BASIC
      pic.codByte($00, ruData);  //Sgte línea BASIC
    end else if bootloader = bldCustom then begin
      PutTopComm(';Custom Bootloader.');
      for i:=0 to high(loaderBytes) do begin
        if loaderBytes[i]=-76 then begin
          pic.codByte(76, ruCodeOp);  //Opcode JMP
        end else if loaderBytes[i]=-1001 then begin  //2 bytes address for entry point.
          pic.codByte(0, ruData, 'COD_HL');  //To complete later
          pic.codByte(0, ruData);            //To complete later
        end else if loaderBytes[i]=-1002 then begin  //5 bytes ASCII address for entry point.
          pic.codByte(0, ruData, 'COD_5A');  //To complete later
          pic.codByte(0, ruData);            //To complete later
          pic.codByte(0, ruData);            //To complete later
          pic.codByte(0, ruData);            //To complete later
          pic.codByte(0, ruData);            //To complete later
        end else if loaderBytes[i]=-1003 then begin  //4 bytes ASCII address for entry point.
          pic.codByte(0, ruData, 'COD_4A');  //To complete later
          pic.codByte(0, ruData);            //To complete later
          pic.codByte(0, ruData);            //To complete later
          pic.codByte(0, ruData);            //To complete later
        end else begin  //Common byte
          pic.codByte(loaderBytes[i], true);
        end;
      end;
    end;
    add2 := pic.iRam;
  end;
  procedure CompleteBootloader(add1, add2: word; cod_entrypoint: word);
  {Complete the sections of the bootloader that need to be completed.
  "cod_entrypoint" is the address for the entry point of the compiled code.}
  var
    i: Word;
    tmp: string;
  begin
    for i:= add1 to add2 do begin
      if pic.ram[i].name = 'COD_HL' then begin
        pic.ram[i].value := lo(cod_entrypoint);
        pic.ram[i+1].value := hi(cod_entrypoint);
      end else if pic.ram[i].name = 'COD_5A' then begin
        tmp := RightStr('0000' + IntToStr(cod_entrypoint), 5);
        pic.ram[i  ].value := ord(tmp[1]);
        pic.ram[i+1].value := ord(tmp[2]);
        pic.ram[i+2].value := ord(tmp[3]);
        pic.ram[i+3].value := ord(tmp[4]);
        pic.ram[i+4].value := ord(tmp[5]);
      end else if pic.ram[i].name = 'COD_4A' then begin
        tmp := RightStr('000' + IntToStr(cod_entrypoint), 4);
        pic.ram[i  ].value := ord(tmp[1]);
        pic.ram[i+1].value := ord(tmp[2]);
        pic.ram[i+2].value := ord(tmp[3]);
        pic.ram[i+3].value := ord(tmp[4]);
      end;
    end;
  end;
var
  add , addr: word;
  add1, add2: word;
  fun    : TAstFunDec;
  i      : Integer;
  bod    : TAstBody;
  elem   : TAstElement;
begin
  if IsUnit then exit;
  //Verifica las constantes usadas. Solo en el nodo principal, para no sobrecargar mensajes.
  for elem in TreeElems.main.elements do if elem.idClass = eleConsDec then begin
    if elem.nCalled = 0 then begin
      GenWarn(WA_UNUSED_CON_, [elem.name], elem.srcDec);
    end;
  end;
  //Inicio de generación de código.
  pic.iRam := GeneralORG;  //Inicia puntero a RAM
  compMod := cmGenCode;    //Generates code.
  pic.disableCodegen := false;  //Enable the code generation
  //Create Bootloader
  addBootldr := pic.iRam;  //Save position.
  GenBootloader(add1, add2);
  //Asigna memoria a registros
  //Asigna memoria para las variables, buscando memoria libre a partir de "GeneralORG".
  addVariab  := pic.iRam;   //Save position.
  CreateVarsAndPars;  //Primero a las variables locales (y parámetros) de las funciones
  //Find the next free RAM location, to write functions.
  pic.freeStart := GeneralORG;  //Start of program block
  pic.dataAddr1   := -1; {Disable. It has been already used for allocatig variables, but
                          now we just want to find a free RAM location in the program block}
  pic.GetFreeByte(addr);
  pic.iRam := addr;
  addFuncts  := pic.iRam;  //Save position.
  //Codifica la función INTERRUPT, si existe
  if interruptFunct<>nil then begin;
    { TODO : Revisar }
    //fun := interruptFunct;
    ////Compila la función en la dirección 0x04
    //pic.iRam := $04;
    //fun.adrr := pic.iRam;    //Actualiza la dirección final
    //fun.retType.DefineRegister;    //Asegura que se dispondrá de los WR necesarios
    //SetCtxState(fun.posCtx);  //Posiciona escáner
    //PutLabel('__'+fun.name);
    //TreeElems.OpenElement(fun.BodyNode); //Ubica el espacio de nombres, de forma similar a la pre-compilación
    //CompileSentence;
    //TreeElems.CloseElement;  //cierra el body
    //TreeElems.CloseElement;  //cierra la función
    //if HayError then exit;     //Puede haber error
  end;
  //Codifica las subrutinas usadas
  for fun in usedFuncs do begin
    if fun.IsInterrupt then continue;
    //debugln('---Función usada: ' + fun.name);
    case fun.callType of
    ctUsrNormal: begin  //Función normal de usuario
      //Compile used function in the current address.
      fun.adrr := pic.iRam;     //Actualiza la dirección final
      //Is a common function with body.
      GenCodeFunction(fun.bodyImplem);
      if HayError then exit;   //Puede haber error
      fun.coded := true;       //Marca como ya codficada en memoria.
      //Verifica si hace falta completar llamadas
      if fun.nAddresPend>0 then begin
          //Hay llamadas pendientes que completar a esta función
          for i:=0 to fun.nAddresPend -1 do begin
            debugln('Completando lllamadas pendientes a %s en %d', [fun.name, fun.addrsPend[i]]);
            //Completa la instrucción JSR $0000
            add := fun.addrsPend[i];
            pic.ram[add].value   := fun.adrr and $ff;
            pic.ram[add+1].value := (fun.adrr >> 8) and $ff;
          end;
      end;
    end;
    ctSysNormal: begin  //Función normal del sistema.
      //Compile used function in the current address.
      fun.adrr := pic.iRam;    //Actualiza la dirección final
      fun.codSysNormal(fun);   //Rutina para generar código
      if HayError then exit;   //Puede haber error
      fun.coded := true;       //Marca como ya codficada en memoria.
      { TODO : ¿Hace falta completar llamadas? }
    end;
    end;
  end;
  for fun in unusedFuncs do begin
    //Esta función no se usa.
    if fun.Parent = TreeElems.main then begin
      //Genera mensaje solo para funciones del programa principal.
      GenWarn(WA_UNUSED_PRO_, [fun.name], fun.srcDec);
    end;
  end;
  //Compila cuerpo del programa principal
  CompleteBootloader(add1, add2, pic.iRam);  //Complete bootloader
  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
  if bod = nil then begin
    GenError('Body program not found.');
    exit;
  end;
  bod.adrr := pic.iRam;  //guarda la dirección de codificación
  GenCodeMainBody(bod);
  if HayError then exit;     //Puede haber error
  //Clean extra RAM firstly used and later not used by optimization.
  for add := pic.iRam to pic.iRam +3 do begin
    pic.ram[add].used := ruUnused;
  end;
}

end;


{ TGenConsValue }

function TGenConsValue.evaluated(typ: TGenTypeDec): Boolean;
var
  itemExp: TGenConsValue;
begin
  if typ.catType = tctAtomic then begin
    //Simple type
    case consType of
    ctLiteral: exit(true);
    ctConsRef: exit(consRef.evaluated);
    ctVarAddr: exit(addrVar.allocated);
    ctFunAddr: exit(addrFun.coded);
    end;
  end else if typ.catType = tctArray then begin;
    //Constant array. Let's evaluate by items
    for itemExp in items do begin
      if not itemExp.evaluated(typ.itmType) then exit(false);
    end;
    exit(true);
  end else begin
    //Not implemented other types
    exit(false);
  end;
end;
procedure TGenConsValue.evaluate(typ: TGenTypeDec);
var
  itemExp: TGenConsValue;
begin
  if Typ.catType = tctAtomic then begin
    //Simple type
    case consType of
    ctLiteral: exit;   //No need to convert.
    ctConsRef: ValInt := consRef.value.ValInt;
    ctVarAddr: ValInt := addrVar.addr;
    ctFunAddr: ValInt := addrFun.adrr;
    end;
  end else if Typ.catType = tctArray then begin
    //Constant array. Let's evaluate by items
    for itemExp in items do begin
      itemExp.Evaluate(typ.itmType);
    end;
  end else begin
    //Object or pointer.
    //Pointers are not allowed because constant pointers generate variables: ($123)^
    //Object are not implemented.
    debugln('Not implemented');
  end;
end;
procedure TGenConsValue.InitItems;
begin
  nItems := 0;
  curSize := CONS_ITEM_BLOCK;   //Block size
  setlength(items, curSize);  //initial size
end;
procedure TGenConsValue.AddConsItem(const c: TGenConsValue);
begin
  items[nItems] := c;
  inc(nItems);
  if nItems >= curSize then begin
    curSize += CONS_ITEM_BLOCK;   //Increase size by block
    setlength(items, curSize);  //make space
  end;
end;
procedure TGenConsValue.CloseItems;
begin
  setlength(items, nItems);
end;
function TGenConsValue.LByte: byte;
begin
  Result := LO(word(valInt));
end;
function TGenConsValue.HByte: byte;
begin
  Result := HI(word(valInt));
end;
function TGenConsValue.EByte: byte;
begin
  Result := (valInt >> 16) and $FF;
end;
function TGenConsValue.UByte: byte;
begin
  Result := (valInt >> 24) and $FF;
end;
function TGenConsValue.valuesAsString: string;
{Returns a string containing the abstract of values stored.}
var
  tmp: Char;
begin
  If ValBool then tmp := 'T' else tmp := 'F';
  Result := 'int=' + IntToStr(ValInt) + ',bool=' + tmp;
end;

{ TGenOperand }

function TGenOperand.StoAsStr: string;
begin
  WriteStr(Result, Sto);
end;
function TGenOperand.FunCallText: string;
//Returns the function call in text.
//Only works when "opType" is otFunc.
var
  i: Integer;
begin
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  Result := Text + '(';
  if (funDec=Nil) or  //Is System
     (funDec.astFunDec.callType in [ctUsrInline, ctSysInline]) then begin
      //Only in this case, shows parameters
      for i:=0 to High(elements) do begin
        //Agrega nombre de parámetro
          if i=0 then Result += elements[i].Text
          else        Result += ',' + elements[i].Text;
      end;
      Result += ')';
  end;
  {$ENDIF}
end;
procedure TGenOperand.SetParAsVar(i: Integer; vardec0: TMirVarDec);
{Set a parameter like a variable}
var
  par: ^TMirOperand;
begin
  par := @elements[i];
  //Convert "par1" to the temporal variable
  par^.Text   := vardec0.text;
  par^.SetVar_RamFix(vardec0);
  par^.astOperand := nil;
end;
function TGenOperand.val: dword;
begin
  Result := value.ValInt;
end;
function TGenOperand.valL: word;
begin
  Result := LO(word(value.ValInt));
end;
function TGenOperand.valH: word;
begin
  Result := HI(word(value.ValInt));
end;
function TGenOperand.valU: word;
begin
  Result := (value.valInt >> 24) and $FF;
end;
function TGenOperand.valE: word;
begin
  Result := (value.valInt >> 16) and $FF;
end;
function TGenOperand.valWlo: word;
begin
  Result := word(value.ValInt);
end;
function TGenOperand.valWhi: word;
begin
  Result := (value.valInt >> 16) and $FFFF;
end;
procedure TGenOperand.SetCon_Literal(valBool: Boolean);
{Set the value of a Constant boolean expression.}
begin
  opType := otConst;
  value.consType := ctLiteral;   //Only set the atomic constant type
  value.valBool := valBool;    //Tal vez no sea necesario si usamos solo "value.ValInt"
  //Como en algunos casos se usa el valor numérico, lo fijamos también.
  if valBool then begin
    value.ValInt := 255;
  end else begin
    value.ValInt := 0;
  end;
end;
procedure TGenOperand.SetCon_Literal(valInt: Int64);
begin
  opType := otConst;
  value.consType := ctLiteral;   //Only set the atomic constant type
  value.ValInt := valInt;
end;
procedure TGenOperand.SetCon_ConstRef(cons0: TMirConDec);
begin
  opType := otConst;
  value.consType := ctConsRef;   //Only set the atomic constant type
  value.consRef := cons0;  //Keep reference
  ToLiteral;
end;
procedure TGenOperand.SetCon_VarAddr(var0: TMirVarDec);
begin
  opType := otConst;
  value.consType := ctVarAddr;   //Only set the atomic constant type
  value.addrVar := var0;  //Keep reference
  ToLiteral;
end;
procedure TGenOperand.SetCon_FunAddr(fun0: TMirFunDec);
begin
  opType := otConst;
  value.consType := ctFunAddr;
  value.addrFun := fun0;  //Keep reference
  ToLiteral;
end;
procedure TGenOperand.ToLiteral;
{Evaluate constant values to literal values in "value". Must be called after testing
with evaluated().}
begin
  value.evaluate(typ);
end;
function TGenOperand.evaluated: boolean;
{Indicates if the constant value is evaluated. It means if its literal value can be read
from "value" field.}
begin
  exit(value.evaluated(typ));
end;
function TGenOperand.allocated: boolean;
{Indicates if the variable is allocated ein memory.}
begin
  if Sto = stRamFix then begin
    //Allocations depends on constant value.
    exit(value.evaluated(typ));
  end else begin
    //In all other cases (indexed, addressed by constant )
    exit(true);
  end;
end;
function TGenOperand.vardec: TMirVarDec;
{Give the reference to a variable declaration when it exists. Otherwise returns NIL.}
begin
  if Sto= stRamFix then begin
    if value.consType = ctVarAddr then exit(value.addrVar)
    else exit(nil);
  end else begin
    exit(nil);
  end;
end;
procedure TGenOperand.SetVar_RamFix(vardec0: TMirVarDec);
begin
  opType    := otVariab;
  Sto       := stRamFix;
  //Add the Constant offset in "value".
  value.consType := ctVarAddr;
  value.addrVar  := vardec0;  //Keep reference
end;
procedure TGenOperand.SetVar_RamFix(addr: word);
begin
  opType    := otVariab;
  Sto       := stRamFix;
  //Add the Constant offset in "value".
  value.consType := ctLiteral;
  value.ValInt := addr;
end;
procedure TGenOperand.SetVar_RamVarOf(vardec0: TMirVarDec; idxVar0: TMirVarDec);
begin
  opType    := otVariab;
  Sto       := stRamVarOf;
  //Add the Constant offset in "value".
  value.consType := ctVarAddr;
  value.addrVar  := vardec0;  //Keep reference
  //Add the index
  idxVar := idxVar0;
end;
function TGenOperand.offs: integer;
begin
  exit(value.ValInt);
end;
function TGenOperand.add: word;
begin
  exit(value.ValInt);
end;
function TGenOperand.addL: word;
begin
  exit(value.ValInt);
end;
function TGenOperand.addH: word;
begin
  exit(value.ValInt+1);
end;
function TGenOperand.name: String;
begin
  Exit(text);
end;
function TGenOperand.srcDec: TSrcPos;
var
  vd: TMirVarDec;
begin
  exit(astOperand.srcDec);
end;
procedure TGenOperand.Exchange(i1, i2: integer);
var
  tmp: TGenOperand;
begin
  tmp := elements[i1];
  elements[i1] := elements[i2];
  elements[i2] := tmp;
end;

{ TGenFunDec }
function TGenFunDec.HasImplem: boolean;
{Indica si la declaración tiene implementación separada.}
begin
  exit(implem<>nil);
end;
function TGenFunDec.nLocalVars: integer;
{Returns the numbers of local variables for this function.}
var
  elem : TAstElement;
begin
  Result := 0;
  for elem in elements do begin
    if elem.idClass = eleVarDec then inc(Result);
  end;
end;
procedure TGenFunDec.AddAddresPend(ad: word);
{Add a pending address to the function to be completed later.}
begin
  addrsPend[nAddresPend] := ad;
  inc(nAddresPend);
  if nAddresPend > curSize then begin
    curSize += CONS_ITEM_BLOCK;   //Increase size by block
    setlength(addrsPend, curSize);  //make space
  end;
end;
constructor TGenFunDec.Create;
begin
  inherited Create;
  { *** Por definir
  idClass := eleFuncDec;
  //Init addrsPend[]
  nAddresPend := 0;
  curSize := CONS_ITEM_BLOCK;   //Block size
  setlength(addrsPend, curSize);  //initial size
  declar := Self;
  //By default, we assume this is a declaration and implementation.
  elemImplem := elements;}
end;

{ TGenVarDec }
function TGenVarDec.addrL: word;
{Dirección absoluta de la variable de menor pero, cuando es de tipo WORD.}
begin
  Result := addr;
end;
function TGenVarDec.addrH: word;
{Dirección absoluta de la variable de mayor pero, cuando es de tipo WORD.}
begin
  Result := addr + 1;
end;
function TGenVarDec.addrE: word;
begin
  Result := addr + 2;
end;
function TGenVarDec.addrU: word;
begin
  Result := addr + 3;
end;
function TGenVarDec.AddrString: string;
{Devuelve una cadena, que representa a la dirección física.}
begin
  if vardec.typ.IsByteSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else if vardec.typ.IsWordSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else if vardec.typ.IsDWordSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else begin
    Result := '';   //Error
  end;
end;
procedure TGenVarDec.ResetAddress;
begin
  addr := 0;
end;
function TGenVarDec.stoStr: string;
begin
  WriteStr(Result, storage);
end;
constructor TGenVarDec.Create;
begin
  mirType := mtyVarDec;
end;

procedure DoGenerateHexFile(hexFile: string);
begin
  pic.GenHex(hexFile, opt.GeneralORG);
end;


{ TGenTypeDec }
function TGenTypeDec.getSize: word;
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
procedure TGenTypeDec.setSize(AValue: word);
begin
  fSize := AValue;
end;
function TGenTypeDec.groupStr: string;
begin
  WriteStr(Result, group);
end;
function TGenTypeDec.catTypeStr: string;
begin
  WriteStr(Result, catType);
end;
function TGenTypeDec.nItems: integer;
begin
  if copyOf<>nil then begin
    exit(copyOf.consNitm.value^.ValInt)
  end else begin
    exit(consNitm.value^.ValInt)
  end;
end;
function TGenTypeDec.IsByteSize: boolean;
{Indica si el tipo, tiene 1 byte de tamaño}
begin
//  if copyOf<>nil then exit(copyOf.IsByteSize);  //verifica
  Result := size = 1;
end;
function TGenTypeDec.IsWordSize: boolean;
{Indica si el tipo, tiene 2 bytes de tamaño}
begin
//  if copyOf<>nil then exit(copyOf.IsWordSize);  //verifica
  Result := size = 2;
end;
function TGenTypeDec.IsDWordSize: boolean;
{Indica si el tipo, tiene 4 bytes de tamaño}
begin
//  if copyOf<>nil then exit(copyOf.IsDWordSize);  //verifica
  Result := size = 4;
end;
function TGenTypeDec.IsArrayOf(itTyp: TGenTypeDec; numIt: integer): boolean;
{Indicates if this type is an array of the specified type and with the specified
number of elements.}
begin
  if catType <> tctArray then exit(false);
  //I'm an array
//  debugln('Buscando arreglo en: ' + self.name);
  if consNitm = nil then exit(false);  //Not yet set the size.
  exit( (nItems = numIt) and itmType.IsEquivalent(itTyp) );
end;
function TGenTypeDec.IsPointerTo(ptTyp: TGenTypeDec): boolean;
begin
  exit( (catType = tctPointer) and ptrType.IsEquivalent(ptTyp) );
end;
function TGenTypeDec.IsEquivalent(typ: TGenTypeDec): boolean;
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
constructor TGenTypeDec.Create;
begin
  inherited;
  idClass:=eleTypeDec;
  //Ceeate list
  internalTypes:= TAstTypeDecs.Create(true);
end;
destructor TGenTypeDec.Destroy;
begin
  internalTypes.Destroy;
  inherited;
end;

initialization
  OnError := Nil;
  HayError := False;
  pic := TP6502.Create;
  picCore := pic;   //Referencia picCore

finalization
  pic.Destroy;

end.

