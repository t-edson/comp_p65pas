unit ASTunit;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, Classes, fgl, alexiaLex;
type  // Tipos de nodos
  TASTNodeType = (
    //Nodos de expresiones
    ntVariableRef,   //Referencia a variable: x, valor, ...
    ntNumberLiteral, //Literal numérico: 123, 456.
    ntBooleanLiteral,//Literal booleano: true, false.
    ntStringLiteral, //Literal de cadena: 'Hola'.
    ntBinaryOp,      //Operación binaria. Ej. En "a+b", la operación binaria es el "+".
    ntUnaryOp,       //Operación unaria (un operando). Ej. -x, not a.
    ntFunctionCall,  //Llamada a función: max(a, b).
    ntFieldAccess,   //Acceso a campo (persona.nombre).
    ntPointerDeref,  //Acceso a dirección de puntero (p^).
    ntArrayIndex,    //Acceso a arreglo (variable[index]).
    //Nodos de sentencias
    ntAssignment,    //Asignación de valor a variable.
    ntIfStatement,   //Condicional IF-THEN-ELSE.
    //ntProcedureCall, //No se usan Llamada a un procedimiento. Se maneja como llamada a función.
    ntWhileLoop,     //Bucle WHILE-DO.
    ntRepeatUntil,   //Bucle REPEAT-UNTIL.
    ntForLoop,       //Bucle FOR-TO/DOWNTO-DO.
    ntCaseStatement, //Estructura CASE.
    ntCaseBranch,    //Rama individual de un CASE.
    ntWithStatement, //Estructura WITH ... DO
    ntExitStatement, //Instrucción EXIT
    //Nodos de declaraciones
    ntVarDecl,       //Declaración de variable: var x: byte;
    ntConstDecl,     //Declaración de constantes: const PI=3;
    ntProcDecl,      //Declaración de procedimiento: procedure algo; begin ... end;
    ntFunctDecl,      //Declaración de función.
    ntParamDecl,     //Parámetro de procedimiento/función: var x: byte
    //Nodos auxiliares para declaraciones de tipos
    ntArrayRange,    //Rango de arreglo (1..10)
    ntFieldDecl,     //Campo dentro de un RECORD
    //Nodos de declaraciones de tipos
    ntSimpleType,    //Tipo simple, ya predefinido por el sistema.
    ntSubrangeType,  //Subrango
    ntEnumType,      //Enumerado
    ntArrayType,     //Tipo arreglo
    ntRecordType,    //Tipo RECORD
    ntPointerType,   //Puntero
    ntAliasType,     //Alias
    //Nodos estructurales
    ntProgram,       //Nodo raíz del programa completo: program MiPrograma;
    ntDeclarations,  //Sección de declaraciones de variables, tipos, o procedimientos.
    ntBlock          //Bloque de instrucciones (begin...end)
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
  TProcDecl = class;
  TFunctDecl = class;
  TDeclarations = class;

  // Listas genéricas especializadas
  TASTNodeList = specialize TFPGObjectList<TASTNode>;
  TVarDeclList = specialize TFPGObjectList<TVarDecl>;
  TProcDeclList = specialize TFPGObjectList<TProcDecl>;
  TFunctionDeclList = specialize TFPGObjectList<TFunctDecl>;
  TExpressionList = specialize TFPGObjectList<TExpression>;
  TCaseBranchList = specialize TFPGObjectList<TCaseBranch>;

  // Nodo base (clase abstracta)
  TASTNode = class
  private
    FNodeType: TASTNodeType;
    FSrcPos: TSrcPos;
  public
    constructor Create(ANodeType: TASTNodeType; const ASrcPos: TSrcPos);

    property NodeType: TASTNodeType read FNodeType;
    property SrcPos: TSrcPos read FSrcPos;
    property LineNumber: Integer read FSrcPos.row;
    property ColumnNumber: Integer read FSrcPos.col;
    property ContextId: Integer read FSrcPos.idCtx;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); virtual;
  end;

  // Expresión (clase abstracta)
  TExpression = class(TASTNode)
  public
    constructor Create(ANodeType: TASTNodeType; const ASrcPos: TSrcPos);
  end;

  {Clase abstracta base para contenedores de código (procedimientos, funciones y programa
  principal)}
  TCodeContainer = class(TASTNode)
  private
    FName: string;
    FParameters: TVarDeclList;
    FDeclarations: TDeclarations;
    FBody: TBlock;
  public
    property Name: string read FName write FName;
    property Parameters: TVarDeclList read FParameters;
    property Declarations: TDeclarations read FDeclarations;
    property Body: TBlock read FBody write FBody;
    procedure AddParameter(Param: TVarDecl);
  public
    procedure Clear;
    procedure PrintDebug(Indent: Integer = 0); override;
    constructor Create(ANodeType: TASTNodeType; const ASrcPos: TSrcPos);
    destructor Destroy; override;
  end;
type  //Nodos de expresiones
  // Referencia a variable
  TVariableRef = class(TExpression)
  private
    FName: string;
    FDeclaration: TVarDecl;
  public
    constructor Create(const AName: string; const ASrcPos: TSrcPos);

    property Name: string read FName;
    property Declaration: TVarDecl read FDeclaration write FDeclaration;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
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
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Literal booleano
  TBooleanLiteral = class(TExpression)
  private
    FValue: Boolean;
  public
    constructor Create(AValue: Boolean; const ASrcPos: TSrcPos);

    property Value: Boolean read FValue;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Literal string
  TStringLiteral = class(TExpression)
  private
    FValue: string;
  public
    constructor Create(const AValue: string; const ASrcPos: TSrcPos);

    property Value: string read FValue;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Expresión binaria
  TBinaryOp = class(TExpression)
  private
    FOp: string;
    FLeft: TExpression;
    FRight: TExpression;
  public
    constructor Create(const AOp: string; ALeft, ARight: TExpression;
                       const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property Op: string read FOp;
    property Left: TExpression read FLeft;
    property Right: TExpression read FRight;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Expresión unaria
  TUnaryOp = class(TExpression)
  private
    FOp: string;
    FOperand: TExpression;
  public
    constructor Create(const AOp: string; AOperand: TExpression;
                       const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property Op: string read FOp;
    property Operand: TExpression read FOperand;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Llamada a función
  TFunctionCall = class(TExpression)
  private
    FName: string;
    FArguments: TExpressionList;
  public
    constructor Create(const AName: string; const ASrcPos: TSrcPos);
    destructor Destroy; override;

    procedure AddArgument(Arg: TExpression);

    property Name: string read FName;
    property Arguments: TExpressionList read FArguments;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Acceso a campo: persona.edad
  TFieldAccess = class(TExpression)
  private
    FRecordVar: TExpression;   // La variable registro (puede ser simple o acceso a campo)
    FFieldName: string;        // Nombre del campo
  public
    constructor Create(ARecordVar: TExpression; const AFieldName: string;
                       const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property RecordVar: TExpression read FRecordVar;
    property FieldName: string read FFieldName;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
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
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Acceso a arreglo: variable[index]
  TArrayIndex = class(TExpression)
  private
    FArrayVar: TExpression;    //La variable arreglo o expresión
    FIndices: TExpressionList;  //Lista de índices (multidimensional)
  public
    procedure AddIndex(Index: TExpression);
    property ArrayVar: TExpression read FArrayVar;
    property Indices: TExpressionList read FIndices;

    constructor Create(AArrayVar: TExpression; const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
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
    procedure PrintDebug(Indent: Integer = 0); override;
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
    procedure PrintDebug(Indent: Integer = 0); override;
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
    procedure PrintDebug(Indent: Integer = 0); override;
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
    procedure PrintDebug(Indent: Integer = 0); override;
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
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
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
    procedure PrintDebug(Indent: Integer = 0); override;
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
    procedure PrintDebug(Indent: Integer = 0); override;
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
    procedure PrintDebug(Indent: Integer = 0); override;
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
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
type  //Nodos de declaraciones
  // Declaraciones de variables
  TVarDecl = class(TASTNode)
  private
    FName: string;
    FDataTypeName: string;
    FIsParameter: Boolean;
    FIsByReference: Boolean;
  public
    constructor Create(const AName: string;
                       const ADataTypeName: string; const ASrcPos: TSrcPos);

    property Name: string read FName;
    property DataTypeName: string read FDataTypeName;
    property IsParameter: Boolean read FIsParameter write FIsParameter;
    property IsByReference: Boolean read FIsByReference write FIsByReference;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Declaraciones de constantes
  TConstDecl = class(TASTNode)
  private
    FName: string;
    FValue: TExpression;  // La expresión que define el valor
    FConstType: string;   // Tipo opcional (si se especifica)
  public
    constructor Create(const AName: string; AValue: TExpression;
                       const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property Name: string read FName;
    property Value: TExpression read FValue;
    property ConstType: string read FConstType write FConstType;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Declaración de procedimiento
  TProcDecl = class(TCodeContainer)
  public  //Inicialización y depuración
    function ToString: string; override;
    constructor Create(const AName: string; const ASrcPos: TSrcPos);
  end;
  { TFunctDecl }
  // Declaración de función
  TFunctDecl = class(TCodeContainer)
  private
    //FReturnType: TDataType;
    FReturnTypeName: string;
  public
    property ReturnTypeName: string read FReturnTypeName write FReturnTypeName;
  public  //Inicialización y depuración
    function ToString: string; override;
    constructor Create(const AName: string; const ASrcPos: TSrcPos);
  end;
type  //Definiciones previas para declaraciones de tipos
  //Clase base para las declaraciones de tipo
  TTypeDef = class(TASTnode)
    private
      FTypeName: string;  // Nombre del tipo (para tipos simples o alias)
    public
      property TypeName: string read FTypeName write FTypeName;
    public  //Inicialización y depuración
      constructor Create(ANodeType: TASTNodeType; const ATypeName: string;
          const ASrcPos: TSrcPos);
      function ToString: string; override;
      procedure PrintDebug(Indent: Integer = 0); override;
    end;

  // Rango de arreglo (1..10, 'a'..'z', etc.)
  TArrayRange = class(TASTNode)
  private
    FLowExpr: TExpression;   // Límite inferior
    FHighExpr: TExpression;  // Límite superior
  public
    constructor Create(ALowExpr, AHighExpr: TExpression; const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property LowExpr: TExpression read FLowExpr;
    property HighExpr: TExpression read FHighExpr;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  TArrayRangeList = specialize TFPGObjectList<TArrayRange>;

  // Definición de campo (para registros)
  // ============================================================
  TFieldDef = class
  private
    FName: string;
    FTypeName: string;
    FTypeDef: TTypeDef;  // Para tipos definidos inline
    FSrcPos: TSrcPos;
  public
    constructor Create(const AName: string; const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property Name: string read FName;
    property TypeName: string read FTypeName write FTypeName;
    property TypeDef: TTypeDef read FTypeDef write FTypeDef;
    property SrcPos: TSrcPos read FSrcPos;

    function ToString: string;
    procedure PrintDebug(Indent: Integer = 0);
  end;
  TFieldDefList = specialize TFPGObjectList<TFieldDef>;

type  //Nodos de declaraciones de tipos
  // Declaración de tipos pedefinidos (integer, byte, boolean, etc.)
  {Este nodo representa a una supuesta definición de los tipos básicos, que se supone ya
  están definidos. No se creará por códio.}
  TSimpleTypeDef = class(TTypeDef)
  public
    constructor Create(const ATypeName: string; const ASrcPos: TSrcPos);
    function ToString: string; override;
  end;
  // Subrango (1..10, 'a'..'z')
  TSubrangeTypeDef = class(TTypeDef)
  private
    FLowExpr: TExpression;
    FHighExpr: TExpression;
  public
    constructor Create(ALowExpr, AHighExpr: TExpression; const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property LowExpr: TExpression read FLowExpr;
    property HighExpr: TExpression read FHighExpr;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Enumerado (Rojo, Verde, Azul)
  TEnumTypeDef = class(TTypeDef)
  private
    FValues: TStringList;  // Lista de nombres de valores
  public
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;

    procedure AddValue(const Value: string);
    property Values: TStringList read FValues;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Alias (type TEdad = integer)
  TAliasTypeDef = class(TTypeDef)
  private
    FBaseTypeName: string;
    FBaseTypeDef: TTypeDef;
  public
    constructor Create(const ABaseTypeName: string; const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property BaseTypeName: string read FBaseTypeName;
    property BaseTypeDef: TTypeDef read FBaseTypeDef write FBaseTypeDef;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Arreglo (array[1..10] of TPersona)
  TArrayTypeDef = class(TTypeDef)
  private
    FIndexRanges: TArrayRangeList;
    FElementTypeName: string;
    FElementTypeDef: TTypeDef;  // Para tipos definidos inline
  public
    procedure AddRange(Range: TArrayRange);
    property IndexRanges: TArrayRangeList read FIndexRanges;
    property ElementTypeName: string read FElementTypeName write FElementTypeName;
    property ElementTypeDef: TTypeDef read FElementTypeDef write FElementTypeDef;
  public  //Inicialización y depuración
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;
    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Registro (record ... end)
  TRecordTypeDef = class(TTypeDef)
  private
    FFields: TFieldDefList;  // Lista de TFieldDef
  public
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;

    procedure AddField(Field: TFieldDef);
    property Fields: TFieldDefList read FFields;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Puntero (^TLista, ^integer)
  // ============================================================
  TPointerTypeDef = class(TTypeDef)
  private
    FTargetTypeName: string;
    FTargetTypeDef: TTypeDef;  // Para tipos definidos inline
  public
    constructor Create(const ATargetTypeName: string; const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property TargetTypeName: string read FTargetTypeName;
    property TargetTypeDef: TTypeDef read FTargetTypeDef write FTargetTypeDef;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
type  //Nodos estructurales
  // Contenedor de declaraciones
  TDeclarations = class(TASTNode)
  private
    FItems: TASTNodeList;  // Mezcla de VarDecl, ProcDecl, FunctionDecl
  public
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;

    procedure Add(Decl: TASTNode);
    property Items: TASTNodeList read FItems;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Bloque (lista de instrucciones)
  TBlock = class(TASTNode)
  private
    FStatements: TASTNodeList;
  public
    procedure AddStatement(Statement: TASTNode);
    property Statements: TASTNodeList read FStatements;
  public  //Inicialización y depuración
    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;
  end;
  { TProgram }
  // Programa prinicpal
  TProgram = class(TCodeContainer)
  public
    property srcDec: TSrcPos write FSrcPos;  //Acceso para actualizar "SrcPos".
  public  //Inicialización y depuración
    function ToString: string; override;
    constructor Create(const AName: string; const ASrcPos: TSrcPos);
  end;

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
  Result := Format('Node(%d) at %s', [Ord(FNodeType), FSrcPos.RowColString]);
end;
procedure TASTNode.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
end;
// TExpression
constructor TExpression.Create(ANodeType: TASTNodeType; const ASrcPos: TSrcPos);
begin
  inherited Create(ANodeType, ASrcPos);
end;
// TCodeContainer
procedure TCodeContainer.AddParameter(Param: TVarDecl);
begin
  Param.IsParameter := True;
  FParameters.Add(Param);
end;
procedure TCodeContainer.Clear;
{Limpia al árbol de sintaxis del programa o subprograma, y lo deja listo para iniciar el
llenado}
begin
  //Limpiar declaraciones (eliminar todos los elementos)
  FDeclarations.Items.Clear;
  //Limpiar cuerpo principal (eliminar todas las instrucciones)
  FBody.Statements.Clear;
end;
procedure TCodeContainer.PrintDebug(Indent: Integer = 0);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  if FParameters.Count > 0 then begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Parameters:');
    for i := 0 to FParameters.Count - 1 do
      FParameters[i].PrintDebug(Indent + 4);
  end;

  if FDeclarations.Items.Count > 0 then
    FDeclarations.PrintDebug(Indent + 2)
  else
    WriteLn(StringOfChar(' ', Indent + 2), 'Local declarations: (none)');

  if FBody <> nil then begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Body:');
    FBody.PrintDebug(Indent + 4);
  end;
end;
constructor TCodeContainer.Create(ANodeType: TASTNodeType; const ASrcPos: TSrcPos);
begin
  inherited Create(ANodeType, ASrcPos);
  FDeclarations := TDeclarations.Create(ASrcPos);
  {Crea los elementos fijos del programa. Notar que FBody (que representa al cuerpo
  del programa principal o subprograma) se crea en la misma posición que el programa,
  lo cual no es tan consistente porque FBody debería apuntar al BEGIN del programa,
  pero se puede actualizar después.
  Se crea aquí, al crear al programa, para controlar su construcción y destrucción.}
  FBody := TBlock.Create(ASrcPos);
  FParameters := TVarDeclList.Create(True);
end;
destructor TCodeContainer.Destroy;
begin
  FParameters.Destroy;
  FBody.Destroy;
  FDeclarations.Destroy;
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
  Result := Format('VarRef: %s', [FName]);
  if FDeclaration <> nil then
    Result := Result + ' -> ' + FDeclaration.Name;
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TVariableRef.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
end;
// TNumberLiteral
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

  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TNumberLiteral.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
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
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TBooleanLiteral.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
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
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TStringLiteral.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
end;
// TBinaryOp
constructor TBinaryOp.Create(const AOp: string; ALeft, ARight: TExpression;
                             const ASrcPos: TSrcPos);
begin
  inherited Create(ntBinaryOp, ASrcPos);
  FOp := AOp;
  FLeft := ALeft;
  FRight := ARight;
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
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TBinaryOp.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Left:');
  FLeft.PrintDebug(Indent + 4);
  WriteLn(StringOfChar(' ', Indent + 2), 'Right:');
  FRight.PrintDebug(Indent + 4);
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
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TUnaryOp.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Operand:');
  FOperand.PrintDebug(Indent + 4);
end;
// TFunctionCall
constructor TFunctionCall.Create(const AName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntFunctionCall, ASrcPos);
  FName := AName;
  FArguments := TExpressionList.Create(True);
end;
destructor TFunctionCall.Destroy;
begin
  FArguments.Free;
  inherited;
end;
procedure TFunctionCall.AddArgument(Arg: TExpression);
begin
  FArguments.Add(Arg);
end;
function TFunctionCall.ToString: string;
begin
  Result := Format('FunctionCall: %s (%d args)', [FName, FArguments.Count]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TFunctionCall.PrintDebug(Indent: Integer = 0);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  if FArguments.Count > 0 then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Arguments:');
    for i := 0 to FArguments.Count - 1 do
      FArguments[i].PrintDebug(Indent + 4);
  end;
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
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TFieldAccess.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Record variable:');
  FRecordVar.PrintDebug(Indent + 4);
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
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TPointerDeref.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Pointer:');
  FPointer.PrintDebug(Indent + 4);
end;
// TArrayIndex
procedure TArrayIndex.AddIndex(Index: TExpression);
begin
  FIndices.Add(Index);
end;
constructor TArrayIndex.Create(AArrayVar: TExpression; const ASrcPos: TSrcPos);
begin
  inherited Create(ntArrayIndex, ASrcPos);
  FArrayVar := AArrayVar;
  FIndices := TExpressionList.Create(True);
end;
destructor TArrayIndex.Destroy;
begin
  FArrayVar.Free;
  FIndices.Free;
  inherited Destroy;
end;
function TArrayIndex.ToString: string;
begin
  Result := Format('ArrayIndex: %s (%d indices)',
                   [FArrayVar.ToString, FIndices.Count]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TArrayIndex.PrintDebug(Indent: Integer);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Array variable:');
  FArrayVar.PrintDebug(Indent + 4);
  if FIndices.Count > 0 then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Indices:');
    for i := 0 to FIndices.Count - 1 do
      FIndices[i].PrintDebug(Indent + 4);
  end;
end;
{$endregion}
{$region "Nodos de sentencias"}
// TAssignment
constructor TAssignment.Create(ATarget: TExpression; AValue: TExpression;
  const ASrcPos: TSrcPos);
begin
  inherited Create(ntAssignment, ASrcPos);
  FTarget := ATarget;
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
    ntArrayIndex:
      TargetStr := TArrayIndex(FTarget).ArrayVar.ToString + '[...]';
    ntFieldAccess:
      TargetStr := TFieldAccess(FTarget).RecordVar.ToString + '.' +
                   TFieldAccess(FTarget).FieldName;
    else
      TargetStr := '<Expression>';
  end;
  Result := Format('Assignment: %s := ...', [TargetStr]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TAssignment.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  FTarget.PrintDebug(Indent + 2);
  FValue.PrintDebug(Indent + 2);
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
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TIfStatement.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Condition:');
  FCondition.PrintDebug(Indent + 4);
  WriteLn(StringOfChar(' ', Indent + 2), 'Then branch:');
  FThenBranch.PrintDebug(Indent + 4);
  if FElseBranch <> nil then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Else branch:');
    FElseBranch.PrintDebug(Indent + 4);
  end;
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
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TWhileLoop.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Condition:');
  FCondition.PrintDebug(Indent + 4);
  WriteLn(StringOfChar(' ', Indent + 2), 'Body:');
  FBody.PrintDebug(Indent + 4);
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
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TRepeatUntil.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Body:');
  FBody.PrintDebug(Indent + 4);
  WriteLn(StringOfChar(' ', Indent + 2), 'Condition (exit when true):');
  FCondition.PrintDebug(Indent + 4);
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
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TForLoop.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Control variable:');
  FControlVar.PrintDebug(Indent + 4);
  WriteLn(StringOfChar(' ', Indent + 2), Format('Direction: %s',
           [ForDirectionToString(FDirection)]));
  WriteLn(StringOfChar(' ', Indent + 2), 'Start expression:');
  FStartExpr.PrintDebug(Indent + 4);
  WriteLn(StringOfChar(' ', Indent + 2), 'End expression:');
  FEndExpr.PrintDebug(Indent + 4);
  WriteLn(StringOfChar(' ', Indent + 2), 'Body:');
  FBody.PrintDebug(Indent + 4);
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
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TCaseStatement.PrintDebug(Indent: Integer = 0);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Selector:');
  FSelector.PrintDebug(Indent + 4);
  WriteLn(StringOfChar(' ', Indent + 2), 'Branches:');
  for i := 0 to FBranches.Count - 1 do
    FBranches[i].PrintDebug(Indent + 4);
  if FElseBranch <> nil then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Else branch:');
    FElseBranch.PrintDebug(Indent + 4);
  end;
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
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TCaseBranch.PrintDebug(Indent: Integer = 0);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Constants:');
  for i := 0 to FConstants.Count - 1 do
    FConstants[i].PrintDebug(Indent + 4);
  WriteLn(StringOfChar(' ', Indent + 2), 'Statement:');
  if FStatement <> nil then
    FStatement.PrintDebug(Indent + 4)
  else
    WriteLn(StringOfChar(' ', Indent + 4), '(empty)');
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
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TWithStatement.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Record variable:');
  FRecordVar.PrintDebug(Indent + 4);
  WriteLn(StringOfChar(' ', Indent + 2), 'Body:');
  FBody.PrintDebug(Indent + 4);
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
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TExitStatement.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  if HasReturnValue then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Return value:');
    FReturnValue.PrintDebug(Indent + 4);
  end;
end;
{$endregion}
{$region "Nodos de declaraciones"}
// TVarDecl
constructor TVarDecl.Create(const AName: string; const ADataTypeName: string;
  const ASrcPos: TSrcPos);
begin
  inherited Create(ntVarDecl, ASrcPos);
  FName := AName;
  FDataTypeName := ADataTypeName;
  FIsParameter := False;
  FIsByReference := False;
end;
function TVarDecl.ToString: string;
begin
  Result := Format('VarDecl: %s: %s', [FName, FDataTypeName]);
  if FIsParameter then
  begin
    Result := Result + ' (parameter';
    if FIsByReference then
      Result := Result + ', var';
    Result := Result + ')';
  end;
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TVarDecl.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
end;
// TConstDecl
constructor TConstDecl.Create(const AName: string; AValue: TExpression;
                              const ASrcPos: TSrcPos);
begin
  inherited Create(ntConstDecl, ASrcPos);
  FName := AName;
  FValue := AValue;
  FConstType := '';
end;
destructor TConstDecl.Destroy;
begin
  FValue.Free;
  inherited;
end;
function TConstDecl.ToString: string;
begin
  Result := Format('ConstDecl: %s = ', [FName]);
  if FConstType <> '' then
    Result := Result + Format(':%s ', [FConstType]);
  if FValue <> nil then
    Result := Result + FValue.ToString
  else
    Result := Result + '(nil)';
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TConstDecl.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  if FValue <> nil then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Value:');
    FValue.PrintDebug(Indent + 4);
  end;
end;
// TProcDecl
function TProcDecl.ToString: string;
begin
  Result := Format('Procedure: %s (%d params, %d locals)',
                   [FName, FParameters.Count, FDeclarations.Items.Count]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
constructor TProcDecl.Create(const AName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntProcDecl, ASrcPos);
  FName := AName;
end;
// TFunctDecl
function TFunctDecl.ToString: string;
begin
  Result := Format('Function: %s: %s (%d params, %d locals)',
           [FName, FReturnTypeName, FParameters.Count, FDeclarations.Items.Count]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
constructor TFunctDecl.Create(const AName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntFunctDecl, ASrcPos);
  FName := AName;
  //Para simplificar el análisis sintáctico, conviene que el tipo de retorno se actualice
  //después de leer los parámetros por eso no se incluye en el constructor.
  //FReturnTypeName := AReturnTypeName;
end;
{$endregion}
{$region "Definiciones previas para declaraciones de tipos"}
{ TArrayRange }
constructor TArrayRange.Create(ALowExpr, AHighExpr: TExpression;
  const ASrcPos: TSrcPos);
begin
  inherited Create(ntArrayRange, ASrcPos);
  FLowExpr := ALowExpr;
  FHighExpr := AHighExpr;
end;
destructor TArrayRange.Destroy;
begin
  FLowExpr.Free;
  FHighExpr.Free;
  inherited;
end;
function TArrayRange.ToString: string;
begin
  Result := 'ArrayRange';
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TArrayRange.PrintDebug(Indent: Integer);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Low:');
  if FLowExpr <> nil then
    FLowExpr.PrintDebug(Indent + 4)
  else
    WriteLn(StringOfChar(' ', Indent + 4), '(nil)');
  WriteLn(StringOfChar(' ', Indent + 2), 'High:');
  if FHighExpr <> nil then
    FHighExpr.PrintDebug(Indent + 4)
  else
    WriteLn(StringOfChar(' ', Indent + 4), '(nil)');
end;
// TFieldDef
constructor TFieldDef.Create(const AName: string; const ASrcPos: TSrcPos);
begin
  FName := AName;
  FSrcPos := ASrcPos;
  FTypeName := '';
  FTypeDef := nil;
end;
destructor TFieldDef.Destroy;
begin
  FTypeDef.Free;
  inherited;
end;
function TFieldDef.ToString: string;
var
  typName: String;
begin
  if FTypeDef <> nil then typName := FTypeDef.TypeName
  else typName := FTypeName;
  Result := Format('Field: %s: %s', [FName, typName]);
end;
procedure TFieldDef.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  if FTypeDef <> nil then
    FTypeDef.PrintDebug(Indent + 2);
end;
// TTypeDef
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
procedure TTypeDef.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
end;
{$endregion}
{$region "Nodos de declaraciones de tipos"}
// TSimpleTypeDef
constructor TSimpleTypeDef.Create(const ATypeName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntSimpleType, ATypeName, ASrcPos);
end;
function TSimpleTypeDef.ToString: string;
begin
  Result := Format('SimpleType: %s', [FTypeName]);
end;
// TSubrangeTypeDef
constructor TSubrangeTypeDef.Create(ALowExpr, AHighExpr: TExpression;
  const ASrcPos: TSrcPos);
begin
  inherited Create(ntSubrangeType, '', ASrcPos);
  FLowExpr := ALowExpr;
  FHighExpr := AHighExpr;
end;
destructor TSubrangeTypeDef.Destroy;
begin
  FLowExpr.Free;
  FHighExpr.Free;
  inherited;
end;
function TSubrangeTypeDef.ToString: string;
begin
  Result := Format('Subrange: %s..%s',
                   [FLowExpr.ToString, FHighExpr.ToString]);
end;
procedure TSubrangeTypeDef.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Low:');
  FLowExpr.PrintDebug(Indent + 4);
  WriteLn(StringOfChar(' ', Indent + 2), 'High:');
  FHighExpr.PrintDebug(Indent + 4);
end;
// TEnumTypeDef
constructor TEnumTypeDef.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntEnumType, '', ASrcPos);
  FValues := TStringList.Create;
end;
destructor TEnumTypeDef.Destroy;
begin
  FValues.Free;
  inherited;
end;
procedure TEnumTypeDef.AddValue(const Value: string);
begin
  FValues.Add(Value);
end;
function TEnumTypeDef.ToString: string;
begin
  Result := Format('Enum: (%s)', [FValues.CommaText]);
end;
procedure TEnumTypeDef.PrintDebug(Indent: Integer = 0);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Values:');
  for i := 0 to FValues.Count - 1 do
    WriteLn(StringOfChar(' ', Indent + 4), FValues[i]);
end;
// TAliasTypeDef
constructor TAliasTypeDef.Create(const ABaseTypeName: string;
  const ASrcPos: TSrcPos);
begin
  inherited Create(ntAliasType, '', ASrcPos);
  FBaseTypeName := ABaseTypeName;
end;
destructor TAliasTypeDef.Destroy;
begin
  inherited Destroy;
end;
function TAliasTypeDef.ToString: string;
begin
  Result := Format('Alias: %s = %s', [FTypeName, FBaseTypeName]);
end;
procedure TAliasTypeDef.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
end;
// TArrayTypeDef
procedure TArrayTypeDef.AddRange(Range: TArrayRange);
begin
  FIndexRanges.Add(Range);
end;
constructor TArrayTypeDef.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntArrayType, '', ASrcPos);
  FIndexRanges := TArrayRangeList.Create(True);
  FElementTypeName := '';
  FElementTypeDef := nil;
end;
destructor TArrayTypeDef.Destroy;
begin
  FIndexRanges.Free;
  FElementTypeDef.Free;
  inherited;
end;
function TArrayTypeDef.ToString: string;
var
  typName: String;
begin
  if FElementTypeDef <> nil then
    typName := FElementTypeDef.TypeName
  else
    typName := FElementTypeName;
  Result := Format('ArrayType: [%d dims] of %s', [FIndexRanges.Count, typName]);
end;
procedure TArrayTypeDef.PrintDebug(Indent: Integer = 0);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Index ranges:');
  for i := 0 to FIndexRanges.Count - 1 do
    FIndexRanges[i].PrintDebug(Indent + 4);
  WriteLn(StringOfChar(' ', Indent + 2), 'Element type: ', FElementTypeName);
  if FElementTypeDef <> nil then
    FElementTypeDef.PrintDebug(Indent + 4);
end;
// TRecordTypeDef
constructor TRecordTypeDef.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntRecordType, '', ASrcPos);
  FFields := TFieldDefList.Create(True);
end;
destructor TRecordTypeDef.Destroy;
begin
  FFields.Free;
  inherited;
end;
procedure TRecordTypeDef.AddField(Field: TFieldDef);
begin
  FFields.Add(Field);
end;
function TRecordTypeDef.ToString: string;
begin
  Result := Format('Record: %d fields', [FFields.Count]);
end;
procedure TRecordTypeDef.PrintDebug(Indent: Integer = 0);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Fields:');
  for i := 0 to FFields.Count - 1 do
    FFields[i].PrintDebug(Indent + 4);
end;
// TPointerTypeDef
constructor TPointerTypeDef.Create(const ATargetTypeName: string;
  const ASrcPos: TSrcPos);
begin
  inherited Create(ntPointerType, '', ASrcPos);
  FTargetTypeName := ATargetTypeName;
  FTargetTypeDef := nil;
end;
destructor TPointerTypeDef.Destroy;
begin
  FTargetTypeDef.Free;
  inherited;
end;
function TPointerTypeDef.ToString: string;
begin
  Result := Format('Pointer: ^%s', [FTargetTypeName]);
end;
procedure TPointerTypeDef.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Target type: ', FTargetTypeName);
  if FTargetTypeDef <> nil then
    FTargetTypeDef.PrintDebug(Indent + 4);
end;
{$endregion}
{$region "Nodos estructurales"}
// TDeclarations
constructor TDeclarations.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntDeclarations, ASrcPos);
  FItems := TASTNodeList.Create(True);
end;
destructor TDeclarations.Destroy;
begin
  FItems.Free;
  inherited;
end;
procedure TDeclarations.Add(Decl: TASTNode);
begin
  FItems.Add(Decl);
end;
function TDeclarations.ToString: string;
begin
  Result := Format('Declarations (%d items)', [FItems.Count]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TDeclarations.PrintDebug(Indent: Integer = 0);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  for i := 0 to FItems.Count - 1 do
    FItems[i].PrintDebug(Indent + 2);
end;
// TBlock
procedure TBlock.AddStatement(Statement: TASTNode);
begin
  FStatements.Add(Statement);
end;
function TBlock.ToString: string;
begin
  Result := Format('Block (%d statements)', [FStatements.Count]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TBlock.PrintDebug(Indent: Integer = 0);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  for i := 0 to FStatements.Count - 1 do
    FStatements[i].PrintDebug(Indent + 2);
end;
constructor TBlock.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntBlock, ASrcPos);
  FStatements := TASTNodeList.Create(True);
end;
destructor TBlock.Destroy;
begin
  FStatements.Free;
  inherited;
end;
// TProgram
function TProgram.ToString: string;
begin
  Result := Format('Program: %s', [FName]);
  if FDeclarations <> nil then
    Result := Result + Format(' (%d decls)', [FDeclarations.Items.Count]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
constructor TProgram.Create(const AName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntProgram, ASrcPos);
  FName := AName;
end;
{$endregion}
end.
