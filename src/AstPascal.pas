unit AstPascal;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, Classes, fgl, alexiaLex;
type  //Tipos de nodos
  TASTNodeType = (
    //Nodos de expresiones
    ntVariableRef,   //Referencia a variable: x, valor, ...
    ntNumberLiteral, //Literal numérico: 123, 456.
    ntBooleanLiteral,//Literal booleano: true, false.
    ntStringLiteral, //Literal de cadena: 'Hola'.
    ntArrayLiteral,  //Literal de arreglo: (10, 20, 30)
    ntFieldInitializer,//Inicializador de campo: nombre: 'Juan'
    ntRecordLiteral, //Literal de registro: (nombre: 'Juan'; edad: 30)
    ntPointerLiteral,//Literal de puntero: Nil o $100
    ntBinaryOp,      //Operación binaria. Ej. En "a+b", la operación binaria es el "+".
    ntUnaryOp,       //Operación unaria (un operando). Ej. -x, not a.
    ntProcFunctCall, //Llamada a procedimiento o función: max(a, b). Cuando es un procedimiento, sería un nodo de sentencia.
    ntFieldAccess,   //Acceso a campo (persona.nombre).
    ntPointerDeref,  //Acceso a dirección de puntero (p^).
    ntArrayRef,      //Acceso a arreglo (variable[index]).
    //Nodos de sentencias
    ntAssignment,     //Asignación de valor a variable.
    ntIfStatement,    //Condicional IF-THEN-ELSE.
    ntWhileLoop,      //Bucle WHILE-DO.
    ntRepeatUntil,    //Bucle REPEAT-UNTIL.
    ntForLoop,        //Bucle FOR-TO/DOWNTO-DO.
    ntCaseStatement,  //Estructura CASE.
    ntCaseBranch,     //Rama individual de un CASE.
    ntWithStatement,  //Estructura WITH ... DO
    ntExitStatement,  //Instrucción EXIT
    ntAsmBlock,       //Bloque asm ... end;
    ntAsmInstruction, //Instrucción dentro de un bloque ASM
    //Nodos de declaraciones
    ntVarDecl,        //Declaración de variable: var x: byte;
    ntConstDecl,      //Declaración de constantes: const PI=3;
    ntProcFunctDecl,  //Declaración de procedimiento o función.
    ntForwardDecl,    //Declaración FORWARD
    ntTypeDecl,       //Declaración de tipo
    //Nodos auxiliares para declaraciones de tipos
    ntArrayRange,     //Rango de arreglo (1..10)
    ntVariantBranch,  //Una rama de los casos RECORD con variantes.
    //Nodos de definiciones de tipos
    ntSimpleTypeDef,  //Tipo simple, ya predefinido por el sistema.
    ntAliasTypeDef,   //Alias
    ntSubranTypeDef,  //Subrango
    ntEnumTypeDef,    //Enumerado
    ntArrayTypeDef,   //Tipo arreglo
    ntRecordTypeDef,  //Tipo RECORD
    ntPointerTypeDef, //Puntero
    ntProcedTypeDef,  //Tipos procedurales: = proocedure(a: integer; b: integer);
    //Nodos estructurales
    ntUnitRef,        //Referencia a unidades: USES unit1, unit2, ...
    ntProgram,        //Nodo raíz del programa completo: program MiPrograma;
    ntUnit,           //Nodo raiz de una unidad
    ntBlock           //Bloque de instrucciones (begin...end)
  );
type  //Declaraciones y clases base para el AST
  // Dirección del bucle FOR
  TForDirection = (
    fdUpTo,    // to (ascendente)
    fdDownTo   // downto (descendente)
  );

  // Declaración forward de clases
  TASTNode = class;
  TVarDecl = class;
  TVariableRef = class;
  TBlock = class;
  TExpression = class;
  TNumberLiteral = class;
  TBooleanLiteral = class;
  TStringLiteral = class;
  TBinaryOp = class;
  TUnaryOp = class;
  TFunctionCall = class;
  TCaseBranch = class;
  TProcFunctDecl = class;
  TTypeDef = class;
  TTypeDecl = class;
  TRecordTypeDef = class;

  // Listas genéricas especializadas
  TASTNodeList = specialize TFPGObjectList<TASTNode>;
  TVarDeclList = specialize TFPGObjectList<TVarDecl>;
  TProcDeclList = specialize TFPGObjectList<TProcFunctDecl>;
  TExpressionList = specialize TFPGObjectList<TExpression>;

  // Nodo base (clase abstracta)
  TASTNode = class
  private
    FNodeType: TASTNodeType;  //Tipo de nodo.
    FSrcPos: TSrcPos;         //Ubicación en el texto.
    FParent: TASTNode;        //Referencia al nodo padre.
  public
    property NodeType: TASTNodeType read FNodeType;
    property SrcPos: TSrcPos read FSrcPos write FSrcPos;
    property Parent: TASTNode read FParent write FParent;
  public  //Inicialización y depuración
    constructor Create(ANodeType: TASTNodeType; const ASrcPos: TSrcPos);
    function ToString: string; override;
  end;

  // Expresión (clase abstracta)
  TExpression = class(TASTNode)
  private
    //Campos de formato, como ":2:3" o ":10" que se usan en las instrucciones write() y
    //writeln() para espaciar .
    FFormatWidth: Integer;     // -1 = sin formato
    FFormatDecimals: Integer;  // -1 = sin decimales
    {Referencia (no se es propietario) a la definición del tipo de la expresión.
    Se actualiza en el análisis semántico.}
    FExpTypeDef: TTypeDef;
  public
    property FormatWidth: Integer read FFormatWidth write FFormatWidth;
    property FormatDecimals: Integer read FFormatDecimals write FFormatDecimals;
    property ExpTypeDef: TTypeDef read FExpTypeDef write FExpTypeDef;
  public  //Información sobre la expresión
    function HasFormat: Boolean;
    function HasDecimals: Boolean;
    function ValueStr: String;
  public  //Inicialización
    constructor Create(ANodeType: TASTNodeType; const ASrcPos: TSrcPos);
  end;

  {Clase abstracta base para contenedores de código (procedimientos, funciones y programa
  principal)}
  TCodeContainer = class(TASTNode)
  private
    FName: string;
    FDeclarations: TASTNodeList;
    FBody: TBlock;
    FIsForward: Boolean;  //True si es declaración FORWARD
    FIsAssembler: Boolean; //Indica si el procedimiento o función es ASSEMBLER.
  public
    //Lista de parámetros. Si no hay parámetros contiene NIL. Realmente debería ser
    //"TVarDeclList" pero se usa "TASTNodeList" para reutilizar código.
    Parameters: TASTNodeList;
    property Name: string read FName write FName;
    property Declarations: TASTNodeList read FDeclarations;
    property Body: TBlock read FBody write FBody;
    procedure AddParameter(Param: TVarDecl);
    property IsForward: Boolean read FIsForward;
    property IsAssembler: Boolean read FIsAssembler write FIsAssembler;
  public  //Inicialización y depuración
    constructor Create(ANodeType: TASTNodeType; AIsForward: Boolean);
    destructor Destroy; override;
  end;

type  //Nodos de expresiones
  // Literal numérico
  TNumberKind = (
    nkInteger,   // Número entero
    nkFloat      // Número de coma flotante
  );
  TNumberLiteral = class(TExpression)
  private
    FKind: TNumberKind;
    FIntValue: Int64;           // Valor entero
    FFloatValue: Double;        // Valor flotante
  public
    // Constructores
    constructor Create(AValue: Int64; const ASrcPos: TSrcPos); overload;
    constructor Create(AValue: Double; const ASrcPos: TSrcPos); overload;

    // Propiedades
    property Kind: TNumberKind read FKind;
    property IntValue: Int64 read FIntValue;
    property FloatValue: Double read FFloatValue;

    // Métodos de conveniencia
    function IsInteger: Boolean;
    function IsFloat: Boolean;
    function AsString: string;
    function AsVariant: Variant;

    function ToString: string; override;
  end;
  // Literal booleano
  TBooleanLiteral = class(TExpression)
  private
    FValue: Boolean;
  public
    property Value: Boolean read FValue;

    constructor Create(AValue: Boolean; const ASrcPos: TSrcPos);
    function ToString: string; override;
  end;
  // Literal string
  TStringLiteral = class(TExpression)
  private
    FValue: string;
  public
    property Value: string read FValue;
  public  //Inicialización y depuración
    constructor Create(const AValue: string; const ASrcPos: TSrcPos);
    function ToString: string; override;
  end;
  // Literal de arreglos
  TArrayLiteral = class(TExpression)
  private
    FValues: TExpressionList;  // Lista de valores (pueden ser anidados)
  public
    procedure AddValue(Value: TExpression);
    property Values: TExpressionList read FValues;
    function IsMultiDimensional: Boolean;
  public  //Inicialización y depuración
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Inicializador de campo para literales RECORD: nombre: 'Juan'
  TFieldInitializer = class(TASTNode)
  private
    FFieldName: string;
    FValue: TExpression;
  public
    constructor Create(const AFieldName: string; AValue: TExpression;
                       const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property FieldName: string read FFieldName;
    property Value: TExpression read FValue;

    function ToString: string; override;
  end;
  TFieldInitializerList = specialize TFPGObjectList<TFieldInitializer>;
  // Literal de RECORD: (nombre: 'Juan'; edad: 30)
  TRecordLiteral = class(TExpression)
  private
    FFieldInitializers: TFieldInitializerList;  // Lista de TFieldInitializer
  public
    procedure AddInitializer(Init: TFieldInitializer);
    property FieldInitializers: TFieldInitializerList read FFieldInitializers;
  public  //Inicialización y depuración
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Literales de puntero: nil o $100.
  {De momento solo se soportará NIL porque, formalmente, un literal de puntero implica un
  literal para refererirse a direcciones de memoria, y aunque puede usarse un simple
  número como $100, no es semánticamente lo mismo. Además, este formato es dependiente
  del hardware. En algunas CPU puede ser un valor de 8 bits, en otros casos será de 10, 16
  o 32 bits, o peor aún, puede requerir de un <segmento:desplazamiento> o
  <página:Desplazamiento>}
  TPointerLiteral = class(TExpression)
  private
    FAddress: Integer;  // -1 = nil, >= 0 = dirección literal
  public
    property Address: Integer read FAddress;
    function IsNil: Boolean;
    function IsAddress: Boolean;
  public  //Inicialización y depuración
    constructor Create(const ASrcPos: TSrcPos); overload;  // nil
    constructor Create(AAddress: Integer; const ASrcPos: TSrcPos); overload;  // dirección literal
    function ToString: string; override;
  end;
  // Referencia a variable
  TVariableRef = class(TExpression)
  private
    FName: string;
    FDeclaration: TVarDecl;
  public
    property Name: string read FName;
    property Declaration: TVarDecl read FDeclaration write FDeclaration;
  public  //Inicialización y depuración
    constructor Create(const AName: string; const ASrcPos: TSrcPos);
    function ToString: string; override;
  end;
  // Expresión binaria
  TBinaryOp = class(TExpression)
  private
    FOp: string;
    FLeft: TExpression;
    FRight: TExpression;
  public
    property Op: string read FOp;
    property Left: TExpression read FLeft;
    property Right: TExpression read FRight;
  public  //Inicialización y depuración
    constructor Create(const AOp: string; ALeft, ARight: TExpression;
                       const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Expresión unaria
  TUnaryOp = class(TExpression)
  private
    FOp: string;
    FOperand: TExpression;
  public
    property Op: string read FOp;
    property Operand: TExpression read FOperand;

    function ToString: string; override;
    constructor Create(const AOp: string; AOperand: TExpression;
                       const ASrcPos: TSrcPos);
    destructor Destroy; override;
  end;
  // Llamada a función
  TFunctionCall = class(TExpression)
  private
    FName: string;
    FArguments: TExpressionList;
    FDeclaration: TProcFunctDecl;    //Enlace a la declaración
    FIsProcedure: Boolean;      //"True" si es llamada a procedimiento
    FIsIntrinsic: Boolean;      //Indica si es una llamada a proc/función.
  public
    procedure AddArgument(Arg: TExpression);
    property Name: string read FName;
    property Arguments: TExpressionList read FArguments;
    property Declaration: TProcFunctDecl read FDeclaration write FDeclaration;
    property IsProcedure: Boolean read FIsProcedure write FIsProcedure;
    property IsIntrinsic: Boolean read FIsIntrinsic write FIsIntrinsic;
  public  //Inicialización y depuración
    constructor Create(const AName: string; const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Acceso a campo: persona.edad
  TFieldAccess = class(TExpression)
  private
    FFieldName: string;        //Nombre del campo (después del punto)
    FRecordVar: TExpression;   //Variable registro (antes del punto)
  public
    property FieldName: string read FFieldName;
    property RecordVar: TExpression read FRecordVar;
  public  //Inicialización y depuración
    constructor Create(ARecordVar: TExpression; const AFieldName: string;
                       const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Dereferencia de puntero: p^
  TPointerDeref = class(TExpression)
  private
    FPointer: TExpression;  // La expresión que es un puntero
  public
    property Pointer: TExpression read FPointer;
    constructor Create(APointer: TExpression; const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Acceso a arreglo: variable[index]
  TArrayRef = class(TExpression)
  private
    FArrayVar: TExpression;     //La variable arreglo o expresión
    FIndices: TExpressionList;  //Lista de índices (multidimensional)
  public
    procedure AddIndex(Index: TExpression);
    property ArrayVar: TExpression read FArrayVar;
    property Indices: TExpressionList read FIndices;
  public  //Inicialización y depuración
    constructor Create(AArrayVar: TExpression; const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;

type  //Nodos de sentencias
  { TAssignment }
  // Asignación
  TAssignment = class(TASTNode)
  private
    FTarget: TExpression;
    FValue: TExpression;
  public
    constructor Create(ATarget: TExpression; AValue: TExpression;
                       const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property Target: TExpression read FTarget;
    property Value: TExpression read FValue;

    function ToString: string; override;
  end;
  // Sentencia IF
  TIfStatement = class(TASTNode)
  private
    FCondition: TExpression;
    FThenBranch: TBlock;
    FElseBranch: TBlock;
  public
    constructor Create(ACondition: TExpression; AThenBranch: TBlock;
                       AElseBranch: TBlock; const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property Condition: TExpression read FCondition;
    property ThenBranch: TBlock read FThenBranch;
    property ElseBranch: TBlock read FElseBranch;

    function ToString: string; override;
  end;
  // Sentencia WHILE
  TWhileLoop = class(TASTNode)
  private
    FCondition: TExpression;
    FBody: TBlock;
  public
    constructor Create(ACondition: TExpression; ABody: TBlock;
                       const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property Condition: TExpression read FCondition;
    property Body: TBlock read FBody;

    function ToString: string; override;
  end;
  // Sentencia REPEAT...UNTIL
  TRepeatUntil = class(TASTNode)
  private
    FBody: TBlock;
    FCondition: TExpression;
  public
    constructor Create(ABody: TBlock; ACondition: TExpression;
                       const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property Body: TBlock read FBody;
    property Condition: TExpression read FCondition;

    function ToString: string; override;
  end;
  // Sentencia FOR
  TForLoop = class(TASTNode)
  private
    FControlVar: TVariableRef;
    FDirection: TForDirection;
    FStartExpr: TExpression;
    FEndExpr: TExpression;
    FBody: TBlock;
  public
    property ControlVar: TVariableRef read FControlVar;
    property Direction: TForDirection read FDirection;
    property StartExpr: TExpression read FStartExpr;
    property EndExpr: TExpression read FEndExpr;
    property Body: TBlock read FBody;
  public  //Inicialización y depuración
    constructor Create(AControlVar: TVariableRef; ADirection: TForDirection;
                       AStartExpr, AEndExpr: TExpression; ABody: TBlock;
                       const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Rama de CASE (constante: instrucción)
  TCaseBranch = class(TASTNode)
  private
    FConstants: TExpressionList;  // Lista de constantes
    FStatement: TBlock;           // Instrucción a ejecutar
  public
    procedure AddConstant(ConstExpr: TExpression);
    property Constants: TExpressionList read FConstants;
    property Statement: TBlock read FStatement write FStatement;
  public  //Inicialización y depuración
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  TCaseBranchList = specialize TFPGObjectList<TCaseBranch>;
  // Sentencia CASE
  TCaseStatement = class(TASTNode)
  private
    FSelector: TExpression;
    FBranches: TCaseBranchList;
    FElseBranch: TBlock;
  public

    procedure AddBranch(Branch: TCaseBranch);

    property Selector: TExpression read FSelector;
    property Branches: TCaseBranchList read FBranches;
    property ElseBranch: TBlock read FElseBranch write FElseBranch;
  public  //Inicialización y depuración
    constructor Create(ASelector: TExpression; const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Nodo para WITH
  TWithStatement = class(TASTNode)
  private
    FRecordVar: TExpression;  // La variable registro (puede ser campo o arreglo)
    FBody: TBlock;            // El cuerpo del WITH
  public
    property RecordVar: TExpression read FRecordVar;
    property Body: TBlock read FBody;
  public  //Inicialización y depuración
    constructor Create(ARecordVar: TExpression; ABody: TBlock;
                       const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Nodo para la instrucción EXIT
  TExitStatement = class(TASTNode)
  private
    FReturnValue: TExpression;  // Valor de retorno (opcional, solo para funciones)
  public
    property ReturnValue: TExpression read FReturnValue;
    function HasReturnValue: Boolean;
  public  //Inicialización y depuración
    constructor Create(const ASrcPos: TSrcPos); overload;
    constructor Create(AReturnValue: TExpression; const ASrcPos: TSrcPos); overload;
    destructor Destroy; override;
    function ToString: string; override;
  end;
  //Tipos para soporte a instrucciones ASM
  //ASM instruction type
  TAsmInsType = (
    itOpcode,     //Common instruction with an Opcode and Operand.
    itLabel,      //An ASM label.
    itLocLabel,   //An ASM local label.
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
                      should be read in "ref".}
    ref: TASTNode;   {Reference to element when operand refers to some Pascal or
                      ASM element.}
    nam: string;     {Operand name. Used when operand is an unsolved reference}
    used: boolean;   //Indicates if operand is used or not.
    //Operations
    operations: TAsmOperations;    //Operations applied on Operand
    procedure ClearOperations;
    procedure AddOperation(oper: TAsmOperator; value: word);
  end;
  // Nodo para una instrucción ASM
  TAsmInstruction = class(TASTNode)
  private
    Fname    : string;   //Element name
    Funame   : string;   //Upper case name. Used to acelerate searchings.
    procedure Setname(AValue: string);
  public
    addr   : integer;  //Starting Address. Used only in code generation.
    iType  : TAsmInsType;   //ASM instruction type
    //Fields to generate instructions, using TP6502.codAsm() or similar.
    opcode : word;     {Formally should be TP6502Inst or similar. Defined as word
                        because we don't want to depend on unit P6502Utils here. }
    addMode: byte;     {Formally should be TP6502AddMode or similar. Defined as byte
                        because we don't want to depend on unit P6502Utils here. }
    operand: TAsmOperand;  //Operand for ASM instruction.
    operand2: TAsmOperand; //Second operand, used when it's needed.
    property name: string read Fname write Setname;
    property uname: string read Funame;
  public  //Inicialización y depuración
    constructor Create(const ASrcPos: TSrcPos);
    function ToString: string; override;
  end;
  TAsmInstructionList = specialize TFPGObjectList<TAsmInstruction>;
  // Nodo para un bloque ASM
  TAsmBlock = class(TASTNode)
  private
    FInstructions: TAsmInstructionList; // Lista de instrucciones ASM
    FRegisters: TStringList; // Registros modificados (para la cláusula ['EAX','EBX'])
  public
    undefInstrucs: TAsmInstructionList; //List of instruction with operands undefined
    procedure AddInstruction(Inst: TAsmInstruction);
    procedure AddRegister(const Reg: string);
    property Instructions: TAsmInstructionList read FInstructions;
    property Registers: TStringList read FRegisters;
  public  //Inicialización y depuración
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
const //Tipos de declaraciones de variables
  DEC_NONE   = 0;  //Normal declaration. Will be mapped in RAM according compiler decision.
  DEC_ABSOL  = 1;  //Mapped in ABSOLUTE address
  {Queda abierta la definición de otros tipos de declaración si el compilador los soporta}

type  //Nodos de declaraciones
  //Tipo de parámetro
  TParamType = (
    ptyNone,   //Parámetro por valor
    ptyVar,    //Parámetro de tipo VAR
    ptyConst,  //Parámetro de tipo CONST
    ptyOut     //Parámetro de tipo OUT
  );
  // Declaraciones de variables
  TVarDecl = class(TASTNode)
  private
    FIsParameter: Boolean;    //Indica que esta declaración es de un parámetro.
    FParamType: TParamType;   //Tipo del parámetro.
    FTypeDef  : TTypeDef;     //Referencia al tipo de la variable declarada.
    {Bandera para indicar que este nodo es propietario del tipo "FTypeDef" y, en
    consecuencia, debe responsabilizarse de destruirlo. Esta variable es necesaria porque
    las declaraciones de la forma: VAR a,b,c: <tipo estructurado>
    comparten un mismo objeto "TTypeDef" y solo uno debe destruirlo.}
    FTypeOwner: boolean;
  public
    Name      : string;       //Nombre de la variable
    hasAdic   : Byte;         {Valor que define el tipo de parámetro adicional. Por
                               defecto, toma el valor DEC_NONE. Se maneja como número en
                               lugar de un enumerado fijo porque es dependiente del
                               hardware}
    initVal   : TExpression;  //La expresión que define el valor inicial
    absAddr   : TExpression;  {Reference to the AST expression that returns the absolute
                               address where the variable should be located.}
    property TypeOwner: boolean read FTypeOwner write FTypeOwner;
  public   //Campos para el tipo
    property TypeDef: TTypeDef read FTypeDef write FTypeDef;
  public  //Manejo de parámetros
    property IsParameter: Boolean read FIsParameter write FIsParameter;
    property ParamType: TParamType read FParamType write FParamType;
  public  //Inicialización y depuración
    constructor Create(const AName: string; const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Declaraciones de constantes
  TConstDecl = class(TASTNode)
  private
    FName     : string;       //Nombre de la constante
    FTypeDef  : TTypeDef;     //Referencia al tipo de la constante.
    FValue    : TExpression;  //La expresión que define el valor
  public
    property Name: string read FName;
    property TypeDef: TTypeDef read FTypeDef write FTypeDef;
    property Value: TExpression read FValue;
    function HasType: Boolean;
  public  //Inicialización y depuración
    constructor Create(const AName: string; AValue: TExpression; const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Declaración de procedimientos o funciones
  TProcFunctDecl = class(TCodeContainer)
  private
    FReturnTypeDef: TTypeDef;        //Referencia al tipo de retorno. También se usa como
                                     //bandera para identificar a las funciones.
    FIsMethod     : Boolean;         //Bandera para indicar que es método.
    FRecordType   : TRecordTypeDef; //Referencia al tipo RECORD, cuando es un método.
  public
    property ReturnTypeDef: TTypeDef read FReturnTypeDef write FReturnTypeDef;
    function IsFunction: Boolean; inline;
    property IsMethod: Boolean read FIsMethod write FIsMethod;
    property RecordType: TRecordTypeDef read FRecordType write FRecordType;
  public  //Inicialización y depuración
    constructor Create(const AName: string; const ASrcPos: TSrcPos; AIsForward: Boolean);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  //Clase que implementa la declaración de tipos: <Name> = <Definit>
  TTypeDecl = class(TASTNode)
  private
    //Nombre del tipo: TYPE <nombre_del_tipo> = ...
    FName: string;
    {Definición del tipo. Se espera que la declaración de un tipo contenga siempre una
    definición, aún cuando se trate de un Alias}
    FDefinition: TTypeDef;
  public
    property Name: string read FName write FName;
    property Definition: TTypeDef read FDefinition write FDefinition;
    constructor Create(const ATypeName: string; const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
type  //Definiciones previas para declaraciones de tipos

  //Clase base para todas las definiciones de tipo (alias o INLINE).
  {Una definición de tipo se encuentra en:
    - Las declaraciones de tipo:  "= array[1..12] of char"
    - Declaraciones de variables: ": array[1..12] of char"
    - Declaración de parámetros: ": mitipo"
    - Valor devuelto de una función:  ": byte"
    - Campos de un RECORD: ": tipodecampo"
  }

  { TTypeDef }

  TTypeDef = class(TASTnode)
    private
      {Nombre del tipo.
      Normalmente, no se debería usar un nombre, pues una definición de tipo, es anónima
      en su concepción:
        array[1..5] of char
        integer
      Solo en las declaraciones de tipo tienen un nombre:
        TArreglo5 = array[1..5] of char;
        mitipo = integer;
      Pero este nombre se guarda en el campo Name del TTypeDecl asociado.
      Sin embargo, para mantener toda la información de la declaración de un tipo
      (TTypeDecl) en la definición (excepto su SrcPos), se guarda una copia del nombre en
      este campo (cuando hay un nombre). Así se simplifica la implementación. En cualquier
      caso, se puede acceder al nombre del tipo, a traves del nodo padre.}
      FTypeName: string;
    public
      property TypeName: string read FTypeName write FTypeName;
      function IsNamed: Boolean; inline;
      function IsInline: Boolean; inline;
      function GetFinalDef: TTypeDef; inline;
    public  //Inicialización y depuración
      constructor Create(ANodeType: TASTNodeType; const ATypeName: string;
          const ASrcPos: TSrcPos);
      function ToString: string; override;
    end;

  // Rango de arreglo (1..10, 'a'..'z', etc.)
  TArrayRange = class(TASTNode)
  private
    FLowExpr: TExpression;   // Límite inferior
    FHighExpr: TExpression;  // Límite superior
  public
    property LowExpr: TExpression read FLowExpr;
    property HighExpr: TExpression read FHighExpr;
  public  //Inicialización y depuración
    constructor Create(ALowExpr, AHighExpr: TExpression; const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  TArrayRangeList = specialize TFPGObjectList<TArrayRange>;
  //Definición de campo de RECORD. No se usa. Se está usando TVarDecl para los campos.
  //TFieldDef = class(TASTNode)
  //end;
  //TFieldDefList = specialize TFPGObjectList<TFieldDef>;

  // Nodo para una variante (rama de un CASE dentro de un RECORD)
  TVariantBranch = class(TASTNode)
  private
    //Lista de valores que activan esta variante.
    {Por lo general será un solo valor constante como 0 o 1, pero se soportan listas}
    FSelectorValues: TExpressionList;
    //Lista de campos que se usarán en esta variante.
    {Estos campos incluyen sus variables y tipos. Similar a TRecordTypeDef.Fields.}
    FFields: TASTNodeList;
  public
    procedure AddSelectorValue(Value: TExpression);
    procedure AddField(Field: TASTNode);
    property SelectorValues: TExpressionList read FSelectorValues;
    property Fields: TASTNodeList read FFields;
  public  //Inicialización y depuración
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  TVariantBranchList = specialize TFPGObjectList<TVariantBranch>;
type  //Nodos de definiciones de tipos
  //Definición de tipos pedefinidos (integer, byte, boolean, etc.)
  TSimpleTypeDef = class(TTypeDef)
    {Este nodo representa a una supuesta definición de los tipos básicos, que se supone ya
    están definidos. No se pueden crear desde Pascal.}
  public  //Inicialización y depuración
    constructor Create(const ATypeName: string; const ASrcPos: TSrcPos);
    function ToString: string; override;
  end;
  //Alias (type TEdad = integer)
  TAliasTypeDef = class(TTypeDef)
  private
    {Referencia al tipo base.
    Este es el nombre del tipo referenciado directamente: <tipo_alias> = <tipo_base>.
    Solo es necesario una cadena, porque esta definición es de tipo alias}
    FBaseType: String;   //Nombre del tipo base (ej: 'integer', 'TPersona').
    {Referencia a la declaración del tipo.
    Se asigna en el análisis semántico.
    Tosos los tipos indicados en FBaseType (mitipo, TCadena, ..), salvo los del sistema
    (byte, integer, string, ...) deben tener una declaración.
    Debe ser solo una referencia. No se es propietario de este nodo.}
    FDeclaration: TTypeDecl;
    {Referencia a la definición fundamental del tipo.
    Se resuelve en el análisis semántico.
    La definición final de un tipo, se obtiene resolviendo los alias encadenados, hasta
    llegar a la definición final. Solo puede ser un tipo del sistema (byte, integer,
    string, ...) o un tipo estructurado (array ... of ..., record ... end).
    Debe ser solo una referencia. No se es propietario de este nodo.}
    FFinalDef: TTypeDef;
  public
    property BaseType: String read FBaseType write FBaseType;
    property Declaration: TTypeDecl read FDeclaration write FDeclaration;
    property FinalDef: TTypeDef read FFinalDef write FFinalDef;
  public  //Inicialización y depuración
    constructor Create(ABaseType: String; const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  //Subrango (1..10, 'a'..'z')
  TSubranTypeDef = class(TTypeDef)
  private
    FLowExpr: TExpression;    //Límite inferior del rango
    FHighExpr: TExpression;   //Límite superior del rango
    FBaseType: TTypeDef;      //Tipo base (integer, char, etc.)
    FBaseTypeName: string;    //Nombre del tipo base
  public
    property LowExpr: TExpression read FLowExpr;
    property HighExpr: TExpression read FHighExpr;
    property BaseType: TTypeDef read FBaseType write FBaseType;
    property BaseTypeName: string read FBaseTypeName write FBaseTypeName;
  public  //Inicialización y depuración
    constructor Create(ALowExpr, AHighExpr: TExpression; const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  //Enumerado (Rojo, Verde, Azul)
  TEnumTypeDef = class(TTypeDef)
  private
    FValues: TStringList;  // Lista de nombres de valores
  public
    procedure AddValue(const Value: string);
    property Values: TStringList read FValues;
  public  //Inicialización y depuración
    function ToString: string; override;
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;
  end;
  //Arreglo (array[1..10] of TPersona)
  TArrayTypeDef = class(TTypeDef)
  private
    FIndexRanges: TArrayRangeList;  //Dimensiones del arreglo
    FElemTypeDef: TTypeDef;  //Referencia al tipo de elemento del arreglo.
  public
    procedure AddRange(Range: TArrayRange);
    property IndexRanges: TArrayRangeList read FIndexRanges;
    property ElemTypeDef: TTypeDef read FElemTypeDef write FElemTypeDef;
  public  //Inicialización y depuración
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Registro (record ... end)
  TRecordTypeDef = class(TTypeDef)
  private
  public
    Fields: TASTNodeList;         //Declaraciones de variables.
    //Campos para manejar los casos de "Variant Record"
    VarSelector: TVarDecl;        //El campo selector (Si no es variante, está en NIL)
    Branches: TVariantBranchList; //Ramas de las variantes
  public  //Inicialización y depuración
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Puntero (^TLista, ^integer)
  // ============================================================
  TPointerTypeDef = class(TTypeDef)
  private
    FTargetTypeDef: TTypeDef;  // Para tipos definidos inline
  public
    property TargetTypeDef: TTypeDef read FTargetTypeDef write FTargetTypeDef;
  public  //Inicialización y depuración
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Tipo procedural: procedure(a: integer; b: integer);
  TProcedTypeDef = class(TTypeDef)
  private
    FIsFunction: Boolean;        // True = función, False = procedimiento
  public
    ReturnTypeName: string;     // Tipo de retorno (solo para funciones)
    ReturnTypeDef: TTypeDef;    // Definición del tipo de retorno
    Parameters: TASTNodeList;   // Lista de parámetros. Realmente debería ser "TVarDeclList" pero se usa "TASTNodeList" para reutilizar código.
    procedure AddParameter(Param: TVarDecl);
    property IsFunction: Boolean read FIsFunction write FIsFunction;
  public  //Inicialización y depuración
    constructor Create(AIsFunction: Boolean; const ASrcPos: TSrcPos); overload;
    destructor Destroy; override;
    function ToString: string; override;
  end;
type  //Nodos estructurales
  // Nodo para la sección USES. ***** ¿No bastaría con un simple string?
  TUnitRef = class(TASTNode)
  private
    FUnitName: string;
    FUnitPath: string;  // Ruta completa (resuelta en análisis semántico)  ***¿Se usa?
  public
    property UnitName: string read FUnitName;
    property UnitPath: string read FUnitPath write FUnitPath;
  public  //Inicialización y depuración
    constructor Create(const AUnitName: string; const ASrcPos: TSrcPos);
    function ToString: string; override;
  end;
  TUnitRefList = specialize TFPGObjectList<TUnitRef>;
  // Bloque (lista de instrucciones)
  TBlock = class(TASTNode)
  private
    FStatements: TASTNodeList;
  public
    procedure AddStatement(Statement: TASTNode);
    property Statements: TASTNodeList read FStatements;
  public  //Inicialización y depuración
    constructor Create;
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Programa prinicpal
  TProgram = class(TCodeContainer)
  private
    FUsedUnits: TUnitRefList;  //Lista de unidades usadas
  public
    property UsedUnits: TUnitRefList read FUsedUnits;
  public  //Inicialización y depuración
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    function ToString: string; override;
  end;
  // Unidad
  TUnit = class(TASTNode)
  private
    FUnitName: string;
    FInterfaceUses: TUnitRefList;       //USES en interface
    FImplementationUses: TUnitRefList;  //USES en implementation
    FInterfaceDecls: TASTNodeList;      //Declaraciones públicas (interface)
    FImplementationDecls: TASTNodeList; //Declaraciones privadas (implementation)
    FInitializationBlock: TBlock;       //Bloque de inicialización
    FFinalizationBlock: TBlock;         //Bloque de finalización
  public
    Name: String;
    property UnitName: string read FUnitName;
    property InterfaceUses: TUnitRefList read FInterfaceUses;
    property ImplementationUses: TUnitRefList read FImplementationUses;
    property InterfaceDecls: TASTNodeList read FInterfaceDecls;
    property ImplementationDecls: TASTNodeList read FImplementationDecls;
    property InitializationBlock: TBlock read FInitializationBlock write FInitializationBlock;
    property FinalizationBlock: TBlock read FFinalizationBlock write FFinalizationBlock;
  public  //Inicialización y depuración
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    function ToString: string; override;
  end;
  TUnitList = specialize TFPGObjectList<TUnit>;

  // Funciones auxiliares
function ForDirectionToString(Direction: TForDirection): string;
implementation
function ForDirectionToString(Direction: TForDirection): string;
begin
  case Direction of
    fdUpTo:    Result := 'to';
    fdDownTo:  Result := 'downto';
    else       Result := 'unknown';
  end;
end;
{$region "Clases base para el AST"}
// TASTNode
constructor TASTNode.Create(ANodeType: TASTNodeType; const ASrcPos: TSrcPos);
begin
  FNodeType := ANodeType;
  FSrcPos := ASrcPos;
end;
function TASTNode.ToString: string;
begin
  Result := Format('Node(%d)', [Ord(FNodeType)]);
end;
// TExpression
function TExpression.ValueStr: String;
{Devuelve una cadena conteniendo el valor de la expresión, cuando es un literal.}
begin
  if self.NodeType = ntNumberLiteral then begin
    exit(TNumberLiteral(self).AsString);
  end else if self.NodeType = ntBooleanLiteral then begin
    if TBooleanLiteral(self).Value then Exit('true') else Exit('false');
  end else if self.NodeType = ntStringLiteral then begin
    exit(TStringLiteral(self).Value);
  end else if self.NodeType = ntArrayLiteral then begin
    exit('lit_array[]');
  end else if self.NodeType = ntRecordLiteral then begin
    exit('lit_record');
  end else begin
    exit('<expres>');
  end;
end;
function TExpression.HasFormat: Boolean;
begin
  Result := FFormatWidth >= 0;
end;
function TExpression.HasDecimals: Boolean;
begin
  Result := FFormatDecimals >= 0;
end;
constructor TExpression.Create(ANodeType: TASTNodeType; const ASrcPos: TSrcPos);
begin
  inherited Create(ANodeType, ASrcPos);
  FFormatWidth := -1;
  FFormatDecimals := -1;
end;
// TCodeContainer
procedure TCodeContainer.AddParameter(Param: TVarDecl);
begin
  Param.IsParameter := True;
  Parameters.Add(Param);
end;
procedure TProgram.Clear;
{Limpia al árbol de sintaxis del programa o subprograma, y lo deja listo para iniciar el
llenado}
begin
  FUsedUnits.Clear;
  //Limpiar declaraciones (eliminar todos los elementos)
  FDeclarations.Clear;
  //Limpiar cuerpo principal (eliminar todas las instrucciones)
  FBody.Statements.Clear;
end;
constructor TCodeContainer.Create(ANodeType: TASTNodeType; AIsForward: Boolean);
begin
  {Notar que no se indica el "SrcPos" de este objeto, ni de FDeclarations y FBody porque
  se actualizarán después.}
  FNodeType := ANodeType; //Identifica al nodo

  FIsAssembler := False;  //Por defecto no es ASSEMBLER.
  FIsForward := AIsForward;
  //Crea los elementos fijos del programa.
  if FIsForward then begin
    //En declaraciones FORWARD no es necesario crear las declaraciones y el cuerpo.
    FDeclarations := Nil;  //Marca para que no intenten destruirla.
    FBody := Nil;          //Marca para que no intenten destruirla.
  end else begin
    FDeclarations := TASTNodeList.Create(True);
    FBody := TBlock.Create;
  end;
  {No creamos la lista de parámetros aquí, por los siguientes motivos:
   - Para no usar memoria dinámica si el procedimiento/función no usa parámetros.
   - Para permitir que la lista de parámetros se cree previamente a la creación del
     procedimiento/función, y así facilitar el análisis sintáctico de procedimientos,
     funciones o declaraciones FORWARD.}
  Parameters := nil;
end;
destructor TCodeContainer.Destroy;
begin
  Parameters.Free;     //Destruye si se ha creado.
  FBody.Free;          //Destruye si se ha creado.
  FDeclarations.Free;  //Destruye si se ha creado.
  inherited;
end;
{$endregion}
{$region "Nodos de expresiones"}
// TVariableRef
constructor TVariableRef.Create(const AName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntVariableRef, ASrcPos);
  FName := AName;
  FDeclaration := nil;
end;
function TVariableRef.ToString: string;
begin
  Result := Format('TVariableRef: %s', [FName]);
  if FDeclaration <> nil then
    Result += LineEnding + 'TypeDef:' + LineEnding + FDeclaration.TypeDef.ToString;
end;
// TNumberLiteral
function TNumberLiteral.IsInteger: Boolean;
begin
  Result := FKind = nkInteger;
end;
function TNumberLiteral.IsFloat: Boolean;
begin
  Result := FKind = nkFloat;
end;
function TNumberLiteral.AsString: string;
begin
  case FKind of
    nkInteger:
      Result := IntToStr(FIntValue);
    nkFloat:
      Result := FloatToStr(FFloatValue);
    else
      Result := '';
  end;
end;
function TNumberLiteral.AsVariant: Variant;
begin
  case FKind of
    nkInteger:
      Result := FIntValue;
    nkFloat:
      Result := FFloatValue;
    else
      Result := Null;
  end;
end;
constructor TNumberLiteral.Create(AValue: Int64; const ASrcPos: TSrcPos);
// Constructor para enteros
begin
  inherited Create(ntNumberLiteral, ASrcPos);
  FKind := nkInteger;
  FIntValue := AValue;
end;
constructor TNumberLiteral.Create(AValue: Double; const ASrcPos: TSrcPos);
// Constructor para flotantes
begin
  inherited Create(ntNumberLiteral, ASrcPos);
  FKind := nkFloat;
  FFloatValue := AValue;
end;
function TNumberLiteral.ToString: string;
begin
  case FKind of
    nkInteger:
      Result := Format('NumberLiteral: %d', [FIntValue]);
    nkFloat:
      Result := Format('NumberLiteral: %g', [FFloatValue]);
    else
      Result := 'NumberLiteral: (unknown)';
  end;
end;
// TBooleanLiteral
constructor TBooleanLiteral.Create(AValue: Boolean; const ASrcPos: TSrcPos);
begin
  inherited Create(ntBooleanLiteral, ASrcPos);
  FValue := AValue;
end;
function TBooleanLiteral.ToString: string;
begin
  Result := Format('BooleanLiteral: %s', [BoolToStr(FValue, True)]);
end;
// TStringLiteral
constructor TStringLiteral.Create(const AValue: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntStringLiteral, ASrcPos);
  FValue := AValue;
end;
function TStringLiteral.ToString: string;
begin
  Result := Format('StringLiteral: "%s"', [FValue]);
end;
// TArrayLiteral
procedure TArrayLiteral.AddValue(Value: TExpression);
begin
  FValues.Add(Value);
  Value.Parent := Self;
end;
function TArrayLiteral.IsMultiDimensional: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FValues.Count - 1 do begin
    if FValues[i].NodeType = ntArrayLiteral then begin
      Result := True;
      Break;
    end;
  end;
end;
function TArrayLiteral.ToString: string;
begin
  Result := Format('ArrayLiteral: %d values', [FValues.Count]);
  if IsMultiDimensional then
    Result := Result + ' [multidimensional]';
end;
constructor TArrayLiteral.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntArrayLiteral, ASrcPos);
  FValues := TExpressionList.Create(True);
end;
destructor TArrayLiteral.Destroy;
begin
  FValues.Free;
  inherited;
end;
// TFieldInitializer
constructor TFieldInitializer.Create(const AFieldName: string; AValue: TExpression;
                                     const ASrcPos: TSrcPos);
begin
  inherited Create(ntFieldInitializer, ASrcPos);
  FFieldName := AFieldName;
  FValue := AValue;
  if FValue<>nil then FValue.Parent := Self;
end;
destructor TFieldInitializer.Destroy;
begin
  FValue.Free;
  inherited;
end;
function TFieldInitializer.ToString: string;
begin
  Result := Format('FieldInitializer: %s = %s',
                   [FFieldName, FValue.ToString]);
end;
// TRecordLiteral
constructor TRecordLiteral.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntRecordLiteral, ASrcPos);
  FFieldInitializers := TFieldInitializerList.Create(True);
end;
destructor TRecordLiteral.Destroy;
begin
  FFieldInitializers.Free;
  inherited;
end;
procedure TRecordLiteral.AddInitializer(Init: TFieldInitializer);
begin
  FFieldInitializers.Add(Init);
end;
function TRecordLiteral.ToString: string;
begin
  Result := Format('RecordLiteral: %d fields', [FFieldInitializers.Count]);
end;
// TPointerLiteral
function TPointerLiteral.IsNil: Boolean;
begin
  Result := FAddress = -1;
end;
function TPointerLiteral.IsAddress: Boolean;
begin
  Result := FAddress >= 0;
end;
function TPointerLiteral.ToString: string;
begin
  if IsNil then
    Result := 'PointerLiteral: nil'
  else
    Result := Format('PointerLiteral: $%4.4x', [FAddress]);
end;
constructor TPointerLiteral.Create(const ASrcPos: TSrcPos);
// Constructor para nil
begin
  inherited Create(ntPointerLiteral, ASrcPos);
  FAddress := -1;
end;
constructor TPointerLiteral.Create(AAddress: Integer; const ASrcPos: TSrcPos);
// Constructor para dirección literal
begin
  inherited Create(ntPointerLiteral, ASrcPos);
  FAddress := AAddress;
end;
// TBinaryOp
constructor TBinaryOp.Create(const AOp: string; ALeft, ARight: TExpression;
                             const ASrcPos: TSrcPos);
begin
  inherited Create(ntBinaryOp, ASrcPos);
  FOp := AOp;
  FLeft := ALeft;
  FLeft.Parent := Self;
  FRight := ARight;
  FRight.Parent := Self;
end;
destructor TBinaryOp.Destroy;
begin
  FreeAndNil(FLeft);
  FreeAndNil(FRight);
  inherited;
end;
function TBinaryOp.ToString: string;
begin
  Result := Format('BinaryOp: %s', [FOp]);
end;
// TUnaryOp
constructor TUnaryOp.Create(const AOp: string; AOperand: TExpression;
                             const ASrcPos: TSrcPos);
begin
  inherited Create(ntUnaryOp, ASrcPos);
  FOp := AOp;
  FOperand := AOperand;
end;
destructor TUnaryOp.Destroy;
begin
  FreeAndNil(FOperand);
  inherited;
end;
function TUnaryOp.ToString: string;
begin
  Result := Format('UnaryOp: %s', [FOp]);
end;
// TFunctionCall
constructor TFunctionCall.Create(const AName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntProcFunctCall, ASrcPos);
  FName := AName;
  FArguments := TExpressionList.Create(True);
  FDeclaration := nil;
  FIsProcedure := False;
end;
destructor TFunctionCall.Destroy;
begin
  FArguments.Free;
  inherited;
end;
procedure TFunctionCall.AddArgument(Arg: TExpression);
begin
  Arg.Parent := Self;
  FArguments.Add(Arg);
end;
function TFunctionCall.ToString: string;
begin
  Result := Format('FunctionCall: %s (%d args)', [FName, FArguments.Count]);
end;
// TFieldAccess
constructor TFieldAccess.Create(ARecordVar: TExpression; const AFieldName: string;
                                const ASrcPos: TSrcPos);
begin
  inherited Create(ntFieldAccess, ASrcPos);
  FRecordVar := ARecordVar;
  FFieldName := AFieldName;
end;
destructor TFieldAccess.Destroy;
begin
  FRecordVar.Free;
  inherited;
end;
function TFieldAccess.ToString: string;
begin
  Result := Format('FieldAccess: %s.%s', [FRecordVar.ToString, FFieldName]);
end;
// TPointerDeref
constructor TPointerDeref.Create(APointer: TExpression; const ASrcPos: TSrcPos);
begin
  inherited Create(ntPointerDeref, ASrcPos);
  FPointer := APointer;
end;
destructor TPointerDeref.Destroy;
begin
  FPointer.Free;
  inherited;
end;
function TPointerDeref.ToString: string;
begin
  Result := Format('PointerDeref: %s^', [FPointer.ToString]);
end;
// TArrayRef
procedure TArrayRef.AddIndex(Index: TExpression);
begin
  FIndices.Add(Index);
  Index.Parent := Self;
end;
constructor TArrayRef.Create(AArrayVar: TExpression; const ASrcPos: TSrcPos);
begin
  inherited Create(ntArrayRef, ASrcPos);
  FArrayVar := AArrayVar;
  FArrayVar.Parent := Self;
  FIndices := TExpressionList.Create(True);
end;
destructor TArrayRef.Destroy;
begin
  FArrayVar.Free;
  FIndices.Free;
  inherited Destroy;
end;
function TArrayRef.ToString: string;
begin
  Result := 'TArrayRef: <_item()>';
  //Result += LineEnding + 'TypeDef:' + LineEnding + FArrayVar.???;
end;
{$endregion}
{$region "Nodos de sentencias"}
// TAssignment
constructor TAssignment.Create(ATarget: TExpression; AValue: TExpression;
  const ASrcPos: TSrcPos);
begin
  inherited Create(ntAssignment, ASrcPos);
  FTarget := ATarget;
  FTarget.Parent := Self;
  FValue := AValue;
end;
destructor TAssignment.Destroy;
begin
  FreeAndNil(FTarget);
  FreeAndNil(FValue);
  inherited;
end;
function TAssignment.ToString: string;
var
  TargetStr: string;
begin
  case FTarget.NodeType of
    ntVariableRef:
      TargetStr := TVariableRef(FTarget).Name;
    ntArrayRef:
      TargetStr := TArrayRef(FTarget).ArrayVar.ToString + '[...]';
    ntFieldAccess:
      TargetStr := TFieldAccess(FTarget).RecordVar.ToString + '.' +
                   TFieldAccess(FTarget).FieldName;
    else
      TargetStr := '<Expression>';
  end;
  Result := Format('Assignment: %s := ...', [TargetStr]);
end;
// TIfStatement
constructor TIfStatement.Create(ACondition: TExpression; AThenBranch: TBlock;
                               AElseBranch: TBlock; const ASrcPos: TSrcPos);
begin
  inherited Create(ntIfStatement, ASrcPos);
  FCondition := ACondition;
  FThenBranch := AThenBranch;
  FElseBranch := AElseBranch;
end;
destructor TIfStatement.Destroy;
begin
  FreeAndNil(FCondition);
  FreeAndNil(FThenBranch);
  FreeAndNil(FElseBranch);
  inherited;
end;
function TIfStatement.ToString: string;
begin
  Result := 'IfStatement';
end;
// TWhileLoop
constructor TWhileLoop.Create(ACondition: TExpression; ABody: TBlock;
                              const ASrcPos: TSrcPos);
begin
  inherited Create(ntWhileLoop, ASrcPos);
  FCondition := ACondition;
  FBody := ABody;
end;
destructor TWhileLoop.Destroy;
begin
  FreeAndNil(FCondition);
  FreeAndNil(FBody);
  inherited;
end;
function TWhileLoop.ToString: string;
begin
  Result := 'WhileLoop';
end;
// TRepeatUntil
constructor TRepeatUntil.Create(ABody: TBlock; ACondition: TExpression;
                                const ASrcPos: TSrcPos);
begin
  inherited Create(ntRepeatUntil, ASrcPos);
  FBody := ABody;
  FCondition := ACondition;
end;
destructor TRepeatUntil.Destroy;
begin
  FreeAndNil(FBody);
  FreeAndNil(FCondition);
  inherited;
end;
function TRepeatUntil.ToString: string;
begin
  Result := 'RepeatUntil';
end;
// TForLoop
constructor TForLoop.Create(AControlVar: TVariableRef; ADirection: TForDirection;
                           AStartExpr, AEndExpr: TExpression; ABody: TBlock;
                           const ASrcPos: TSrcPos);
begin
  inherited Create(ntForLoop, ASrcPos);
  FControlVar := AControlVar;
  FDirection := ADirection;
  FStartExpr := AStartExpr;
  FEndExpr := AEndExpr;
  FBody := ABody;
end;
destructor TForLoop.Destroy;
begin
  FreeAndNil(FControlVar);
  FreeAndNil(FStartExpr);
  FreeAndNil(FEndExpr);
  FreeAndNil(FBody);
  inherited;
end;
function TForLoop.ToString: string;
begin
  Result := Format('ForLoop: %s %s %s',
                   [FControlVar.Name, ForDirectionToString(FDirection), '...']);
end;
// TCaseStatement
procedure TCaseStatement.AddBranch(Branch: TCaseBranch);
begin
  FBranches.Add(Branch);
end;
constructor TCaseStatement.Create(ASelector: TExpression; const ASrcPos: TSrcPos);
begin
  inherited Create(ntCaseStatement, ASrcPos);
  FSelector := ASelector;
  FSelector.Parent := Self;
  FBranches := TCaseBranchList.Create(True);
  FElseBranch := nil;
end;
destructor TCaseStatement.Destroy;
begin
  FreeAndNil(FSelector);
  FreeAndNil(FElseBranch);
  FBranches.Free;
  inherited;
end;
function TCaseStatement.ToString: string;
begin
  Result := Format('CaseStatement (%d branches)', [FBranches.Count]);
  if FElseBranch <> nil then
    Result := Result + ' (with else)';
end;
// TCaseBranch
constructor TCaseBranch.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntCaseBranch, ASrcPos);
  FConstants := TExpressionList.Create(True);
  FStatement := nil;
end;
destructor TCaseBranch.Destroy;
begin
  FConstants.Free;
  FreeAndNil(FStatement);
  inherited;
end;
procedure TCaseBranch.AddConstant(ConstExpr: TExpression);
begin
  FConstants.Add(ConstExpr);
end;
function TCaseBranch.ToString: string;
begin
  Result := Format('CaseBranch (%d constants)', [FConstants.Count]);
end;
// TWithStatement
constructor TWithStatement.Create(ARecordVar: TExpression; ABody: TBlock;
                                  const ASrcPos: TSrcPos);
begin
  inherited Create(ntWithStatement, ASrcPos);
  FRecordVar := ARecordVar;
  FBody := ABody;
end;
destructor TWithStatement.Destroy;
begin
  FRecordVar.Free;
  FBody.Free;
  inherited;
end;
function TWithStatement.ToString: string;
begin
  Result := Format('WithStatement: %s', [FRecordVar.ToString]);
end;
// TExitStatement
function TExitStatement.HasReturnValue: Boolean;
begin
  Result := FReturnValue <> nil;
end;
constructor TExitStatement.Create(const ASrcPos: TSrcPos);
// Constructor sin valor de retorno (procedimiento)
begin
  inherited Create(ntExitStatement, ASrcPos);
  FReturnValue := nil;
end;
constructor TExitStatement.Create(AReturnValue: TExpression; const ASrcPos: TSrcPos);
// Constructor con valor de retorno (función)
begin
  inherited Create(ntExitStatement, ASrcPos);
  FReturnValue := AReturnValue;
end;
destructor TExitStatement.Destroy;
begin
  FReturnValue.Free;
  inherited;
end;
function TExitStatement.ToString: string;
begin
  if HasReturnValue then
    Result := Format('ExitStatement: return %s', [FReturnValue.ToString])
  else
    Result := 'ExitStatement';
end;
// TAsmOperand
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
// TAsmInstruction
procedure TAsmInstruction.Setname(AValue: string);
begin
  if Fname = AValue then Exit;
  Fname    := AValue;
  Funame   := Upcase(AValue);
end;
constructor TAsmInstruction.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntAsmInstruction, ASrcPos);
end;
function TAsmInstruction.ToString: string;
begin
  Result := 'AsmInstruction';
end;
// TAsmBlock
constructor TAsmBlock.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntAsmBlock, ASrcPos);
  FInstructions := TAsmInstructionList.Create(True);
  FRegisters := TStringList.Create;
  undefInstrucs := TAsmInstructionList.Create(False);  //Solo guardará referencias
end;
destructor TAsmBlock.Destroy;
begin
  undefInstrucs.Destroy;
  FRegisters.Destroy;
  FInstructions.Destroy;
  inherited;
end;
procedure TAsmBlock.AddInstruction(Inst: TAsmInstruction);
begin
  FInstructions.Add(Inst);
end;
procedure TAsmBlock.AddRegister(const Reg: string);
begin
  FRegisters.Add(Reg);
end;
function TAsmBlock.ToString: string;
begin
  Result := Format('AsmBlock (%d instructions)', [FInstructions.Count]);
  if FRegisters.Count > 0 then
    Result := Result + Format(' modifies [%s]', [FRegisters.CommaText]);
end;
{$endregion}
{$region "Nodos de declaraciones"}
// TVarDecl
constructor TVarDecl.Create(const AName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntVarDecl, ASrcPos);
  Name       := AName;
  {El campo FTypeDef se creará e inicializará en el Parser}
  //FTypeDef := nil;
  hasAdic    := DEC_NONE;  //Indica que no hay parñametros adicionales en la declaración
  FParamType := ptyNone;
  //FIsParameter := False;  //No es necesario
  //initVal = Nil;    //No es necesario
  //absAddr = Nil;    //No es necesario
  //La información de tipo debe completarse después
  //TypeOwner := False;
end;
destructor TVarDecl.Destroy;
begin
  absAddr.Free;     //Destruye si se ha usado
  initVal.Free;     //Destruye si se ha usado
  if FTypeOwner then begin     //Es porpietario del tipo
    FTypeDef.Free;
  end;
  inherited Destroy;
end;
function TVarDecl.ToString: string;
begin
  Result := 'VarDecl: Name=' + Name +
  ', TypeName=' + FTypeDef.TypeName + ', TypeDef=' ;
  if TypeDef=Nil then Result += '<Nil>' else Result += TypeDef.FTypeName;
  if FIsParameter then begin
    Result := Result + ' (parameter';
    if FParamType = ptyVar then
      Result := Result + ', var';
    Result := Result + ')';
  end;
end;
// TConstDecl
function TConstDecl.HasType: Boolean;
begin
  //Se usa "FTypeDef" como bandera.
  Result := FTypeDef<>nil;
end;
constructor TConstDecl.Create(const AName: string; AValue: TExpression;
                              const ASrcPos: TSrcPos);
begin
  inherited Create(ntConstDecl, ASrcPos);
  FName := AName;
  //FTypeDef := nil;
  FValue := AValue;
end;
destructor TConstDecl.Destroy;
begin
  FValue.Free;
  FTypeDef.Free;    //Destruye si se ha creado.
  inherited;
end;
function TConstDecl.ToString: string;
begin
  Result := Format('ConstDecl: %s', [FName]);
  if HasType then
    Result := Result + Format(': %s', [FTypeDef.ToString]);
  Result := Result + Format(' = %s', [FValue.ToString]);
end;
// TProcFunctDecl
function TProcFunctDecl.IsFunction: Boolean;
{Indica si este nodo es una función.}
begin
  //Cuando es función, debe tener creado su FReturnTypeDef.
  Exit(FReturnTypeDef <> Nil);
end;
constructor TProcFunctDecl.Create(const AName: string; const ASrcPos: TSrcPos; AIsForward: Boolean);
begin
  inherited Create(ntProcFunctDecl, AIsForward);
  FSrcPos := ASrcPos;
  FName := AName;
  {El campo FReturnTypeDef se creará e inicializará en el Parser, cuando se determine que
  este nodo es una función}
  //FReturnTypeDef := nil;
  //FIsMethod := False;
  //FRecordType := nil;
end;
destructor TProcFunctDecl.Destroy;
begin
  FReturnTypeDef.Free;   //Lo destruye, solo si se ha creado.
  inherited Destroy;
end;
function TProcFunctDecl.ToString: string;
begin
  Result := Format('Procedure: %s', [FName]);
  if IsFunction then
    Result := Result + Format(' returns %s', [FReturnTypeDef.TypeName]);
  if Parameters.Count > 0 then
    Result := Result + Format(' (%d params)', [Parameters.Count]);
  if FDeclarations <> nil then
    Result := Result + Format(' (%d locals)', [FDeclarations.Count]);
  if IsForward then
    Result := Result + ' FORWARD';
  if IsAssembler then
    Result := Result + ' ASSEMBLER';
end;
{$endregion}
{$region "Definiciones previas para declaraciones de tipos"}
// TtypeDef
function TTypeDef.IsNamed: Boolean;
{Indica si la definición de tipo es una solo un identificador que referencia a una
declaración externa de tipo. Algo como "integer" o "mitipo".}
begin
  Exit(NodeType = ntAliasTypeDef);
end;
function TTypeDef.IsInline: Boolean;
{Indica si la definición de tipo es estructruada o compleja. Algo como: "array of ... " o
"record ... end" }
begin
  Exit(NodeType <> ntAliasTypeDef);
end;
function TTypeDef.GetFinalDef: TTypeDef;
{Devuelve la definición final del tipo.}
begin
  if NodeType = ntAliasTypeDef then begin
    //Es un tipo alias.
    //Devolvemos "FinalDef", que debe haberse resuelto en el análisis semántico.
    Exit(TAliasTypeDef(Self).FinalDef);
  end else begin
    //Los tipos INLINE son su misma definición final
    Exit(Self);
  end;
end;
constructor TTypeDef.Create(ANodeType: TASTNodeType; const ATypeName: string;
                            const ASrcPos: TSrcPos);
begin
  inherited Create(ANodeType, ASrcPos);
  FTypeName := ATypeName;
end;
function TTypeDef.ToString: string;
begin
  Result := Format('TypeDef(%d): %s', [Ord(NodeType), FTypeName]);
end;
{ TTypeDecl }
constructor TTypeDecl.Create(const ATypeName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntTypeDecl, ASrcPos);
  FName := ATypeName;
end;
destructor TTypeDecl.Destroy;
begin
  FDefinition.Free;    //Destruye si se ha creado (que es lo normal)
  inherited Destroy;
end;
function TTypeDecl.ToString: string;
begin
  Result := 'TTypeDecl: Name=' + FName + LineEnding + 'Definition: ' + LineEnding;
  if FDefinition = Nil then Result += '<Nil>'
  else Result += FDefinition.ToString;
end;
// TArrayRange
constructor TArrayRange.Create(ALowExpr, AHighExpr: TExpression;
  const ASrcPos: TSrcPos);
begin
  inherited Create(ntArrayRange, ASrcPos);
  FLowExpr := ALowExpr;
  FHighExpr := AHighExpr;
end;
destructor TArrayRange.Destroy;
begin
  FLowExpr.Free;   //Libera si se ha creado
  FHighExpr.Free;  //Libera si se ha creado
  inherited;
end;
function TArrayRange.ToString: string;
begin
  Result := 'ArrayRange';
end;
// TVariantBranch
constructor TVariantBranch.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntVariantBranch, ASrcPos);
  FSelectorValues := TExpressionList.Create(True);
  FFields := TASTNodeList.Create(True);
end;
destructor TVariantBranch.Destroy;
begin
  FSelectorValues.Free;
  FFields.Free;
  inherited;
end;
procedure TVariantBranch.AddSelectorValue(Value: TExpression);
begin
  FSelectorValues.Add(Value);
end;
procedure TVariantBranch.AddField(Field: TASTNode);
begin
  FFields.Add(Field);
end;
function TVariantBranch.ToString: string;
begin
  Result := Format('VariantBranch: %d selectors, %d fields',
                   [FSelectorValues.Count, FFields.Count]);
end;
{$endregion}
{$region "Nodos de declaraciones de tipos"}
// TSimpleTypeDef
constructor TSimpleTypeDef.Create(const ATypeName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntSimpleTypeDef, ATypeName, ASrcPos);
end;
function TSimpleTypeDef.ToString: string;
begin
  Result := Format('SimpleType: %s', [FTypeName]);
end;
// TAliasTypeDef
constructor TAliasTypeDef.Create(ABaseType: String; const ASrcPos: TSrcPos);
begin
  inherited Create(ntAliasTypeDef, '', ASrcPos);
  FBaseType := ABaseType;
end;
destructor TAliasTypeDef.Destroy;
begin
  inherited Destroy;
end;
function TAliasTypeDef.ToString: string;
begin
  Result := 'TAliasTypeDef:' + LineEnding +
  Format('%s = %s', [FTypeName, FBaseType]) + LineEnding;
  if Declaration=Nil then
    Result += 'Declaration: <Nil>'+ LineEnding
  else
    Result += 'Declaration:' + Declaration.ToString + LineEnding;
  if FinalDef=Nil then
    Result += 'FinalDef: <Nil>' + LineEnding
  else
    Result += 'FinalDef:' + FinalDef.ToString + LineEnding;
end;
// TSubranTypeDef
constructor TSubranTypeDef.Create(ALowExpr, AHighExpr: TExpression;
  const ASrcPos: TSrcPos);
begin
  inherited Create(ntSubranTypeDef, '', ASrcPos);
  FLowExpr := ALowExpr;
  FHighExpr := AHighExpr;
end;
destructor TSubranTypeDef.Destroy;
begin
  FLowExpr.Free;
  FHighExpr.Free;
  inherited;
end;
function TSubranTypeDef.ToString: string;
begin
  Result := Format('Subrange: %s..%s',
                   [FLowExpr.ToString, FHighExpr.ToString]);
  if FBaseTypeName <> '' then
    Result := Result + Format(' base %s', [FBaseTypeName])
  else if FBaseType <> nil then
    Result := Result + Format(' base %s', [FBaseType.FTypeName]);
end;
// TEnumTypeDef
procedure TEnumTypeDef.AddValue(const Value: string);
begin
  FValues.Add(Value);
end;
function TEnumTypeDef.ToString: string;
begin
  Result := Format('Enum: (%s)', [FValues.CommaText]);
end;
constructor TEnumTypeDef.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntEnumTypeDef, '', ASrcPos);
  FValues := TStringList.Create;
end;
destructor TEnumTypeDef.Destroy;
begin
  FValues.Free;
  inherited;
end;
// TArrayTypeDef
procedure TArrayTypeDef.AddRange(Range: TArrayRange);
begin
  FIndexRanges.Add(Range);
end;
constructor TArrayTypeDef.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntArrayTypeDef, '', ASrcPos);
  FIndexRanges := TArrayRangeList.Create(True);
  {El campo FElemTypeDef se creará e inicializará en el Parser, cuando se determine que
  este nodo es una función}
  //FElemTypeDef := nil;
end;
destructor TArrayTypeDef.Destroy;
begin
  FIndexRanges.Free;
  FElemTypeDef.Free;   //Lo destruye, si se ha creado.
  inherited;
end;
function TArrayTypeDef.ToString: string;
var
  typName: String;
begin
  Result := Format('ArrayType: [%d dims] of %s',
                   [FIndexRanges.Count, FElemTypeDef.ToString]);
end;
// TRecordTypeDef
constructor TRecordTypeDef.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntRecordTypeDef, '', ASrcPos);
  Fields := TASTNodeList.Create(True);
  VarSelector := Nil;   //No se usa por defecto
  Branches := Nil;      //No se usa por defecto
end;
destructor TRecordTypeDef.Destroy;
begin
  Branches.Free;     //Destruye si se ha usado.
  VarSelector.Free;  //Destruye si se ha usado.
  Fields.Destroy;
  inherited;
end;
function TRecordTypeDef.ToString: string;
begin
  Result := Format('Record: %d fields', [Fields.Count]);
end;
// TPointerTypeDef
constructor TPointerTypeDef.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntPointerTypeDef, '', ASrcPos);
  //FTargetTypeDef := nil;
end;
destructor TPointerTypeDef.Destroy;
begin
  FTargetTypeDef.Free;
  inherited;
end;
function TPointerTypeDef.ToString: string;
begin
  Result := Format('Pointer: ^%s', [FTargetTypeDef.ToString]);
end;
// TProcedTypeDef
constructor TProcedTypeDef.Create(AIsFunction: Boolean; const ASrcPos: TSrcPos);
begin
  inherited Create(ntProcedTypeDef, '', ASrcPos);
  FIsFunction := AIsFunction;
  //Parameters := TVarDeclList.Create(True);
  Parameters := Nil;   //Se crea a demanda
  ReturnTypeName := '';
  ReturnTypeDef := nil;
end;
destructor TProcedTypeDef.Destroy;
begin
  Parameters.Free;
  ReturnTypeDef.Free;   //Destruye si existe
  inherited;
end;
procedure TProcedTypeDef.AddParameter(Param: TVarDecl);
begin
  Param.IsParameter := True;
  Parameters.Add(Param);
end;
function TProcedTypeDef.ToString: string;
begin
  if FIsFunction then
    Result := Format('FunctionType: (%d params) returns %s',
                     [Parameters.Count, ReturnTypeName])
  else
    Result := Format('ProcedureType: (%d params)', [Parameters.Count]);
end;
{$endregion}
{$region "Nodos estructurales"}
// TUnitRef
constructor TUnitRef.Create(const AUnitName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntUnitRef, ASrcPos);
  FUnitName := AUnitName;
  FUnitPath := '';
end;
function TUnitRef.ToString: string;
begin
  Result := Format('UnitRef: %s', [FUnitName]);
  if FUnitPath <> '' then
    Result := Result + Format(' -> %s', [FUnitPath]);
end;
// TBlock
procedure TBlock.AddStatement(Statement: TASTNode);
begin
  Statement.Parent := Self;
  FStatements.Add(Statement);
end;
function TBlock.ToString: string;
begin
  Result := Format('Block (%d statements)', [FStatements.Count]);
end;
constructor TBlock.Create;
begin
  FNodeType := ntBlock;
  FStatements := TASTNodeList.Create(True);
end;
constructor TBlock.Create(const ASrcPos: TSrcPos);
{Versión del constructor que indica la posición del bloque}
begin
  FNodeType := ntBlock;
  FSrcPos := ASrcPos;
  FStatements := TASTNodeList.Create(True);
end;
destructor TBlock.Destroy;
begin
  FStatements.Free;
  inherited;
end;
// TProgram
constructor TProgram.Create;
begin
  inherited Create(ntProgram, False);
  FUsedUnits := TUnitRefList.Create(True);
end;
destructor TProgram.Destroy;
begin
  FUsedUnits.Free;
  inherited;
end;
function TProgram.ToString: string;
begin
  Result := Format('Program: %s', [FName]);
  if FUsedUnits.Count > 0 then
    Result := Result + Format(' (uses %d units)', [FUsedUnits.Count]);
  if FDeclarations <> nil then
    Result := Result + Format(' (%d decls)', [FDeclarations.Count]);
end;
// TUnit
procedure TUnit.Clear;
begin
  FInterfaceUses.Clear;
  FImplementationUses.Clear;
  FInterfaceDecls.Clear;
  FImplementationDecls.Clear;
  if FInitializationBlock <> Nil then FInitializationBlock.Statements.Clear;
  if FFinalizationBlock <> Nil then FFinalizationBlock.Statements.Clear;
end;
constructor TUnit.Create;
begin
  FNodeType := ntUnit;
  FInterfaceUses := TUnitRefList.Create(True);
  FImplementationUses := TUnitRefList.Create(True);
  FInterfaceDecls := TASTNodeList.Create(True);
  FImplementationDecls := TASTNodeList.Create(True);
  FInitializationBlock := nil;
  FFinalizationBlock := nil;
end;
destructor TUnit.Destroy;
begin
  FInterfaceUses.Free;
  FImplementationUses.Free;
  FInterfaceDecls.Free;
  FImplementationDecls.Free;
  FInitializationBlock.Free;
  FFinalizationBlock.Free;
  inherited;
end;
function TUnit.ToString: string;
begin
  Result := Format('Unit: %s', [FUnitName]);
  if FInterfaceUses.Count > 0 then
    Result := Result + Format(' (interface uses %d units)', [FInterfaceUses.Count]);
  if FImplementationUses.Count > 0 then
    Result := Result + Format(' (impl uses %d units)', [FImplementationUses.Count]);
  if FInterfaceDecls.Count > 0 then
    Result := Result + Format(' (interface %d decls)', [FInterfaceDecls.Count]);
  if FImplementationDecls.Count > 0 then
    Result := Result + Format(' (impl %d decls)', [FImplementationDecls.Count]);
end;
{$endregion}
end.
