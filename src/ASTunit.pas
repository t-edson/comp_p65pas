unit ASTunit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fgl, alexiaLex;

type
  // Tipos de nodos
  TASTNodeType = (
    //Nodos de expresiones
    ntVariableRef,   //Referencia a variable: x, valor, ...
    ntNumberLiteral, //Literal numérico: 123, 456
    ntBooleanLiteral, //Literal booleano: true, false
    ntStringLiteral, //Literal de cadena: 'Hola'
    ntBinaryOp,      //Operación binaria. Ej. En "a+b", la operación binaria es el "+".
    ntUnaryOp,       //Operación unaria (un operando). Ej. -x, not a
    ntFunctionCall,  //Llamada a función: max(a, b)
    ntArrayIndex,    //Acceso a arreglo (variable[index])
    ntFieldAccess,  // Acceso a campo (persona.nombre)
    //Nodos de sentencias
    ntAssignment,    //Asignación de valor a variable.
    ntIfStatement,   //Condicional IF-THEN-ELSE.
    //ntProcedureCall, //Llamada a un procedimiento
    ntWhileLoop,     //Bucle WHILE-DO.
    ntRepeatUntil,   //Bucle REPEAT-UNTIL.
    ntForLoop,       //Bucle FOR-TO/DOWNTO-DO.
    ntCaseStatement, //Estructura CASE.
    ntCaseBranch,    //Rama individual de un CASE.
    //Nodos de declaraciones
    ntVarDecl,       //Declaración de variable: var x: byte;
    ntConstDecl,     //Declaración de constantes: const PI=3;
    ntProcDecl,      //Declaración de procedimiento: procedure algo; begin ... end;
    ntFunction,      //Declaración de función.
    ntParamDecl,     //Parámetro de procedimiento/función: var x: byte
    ntTypeDecl,      //Declaración de tipo: type mi_tipo = byte;
    ntArrayType,     //Declaración de tipo arreglo
    ntArrayRange,    //Rango de arreglo (1..10)
    ntRecordType,   // Declaración de tipo RECORD
    ntFieldDecl,    // Declaración de campo dentro de un RECORD
    //Nodos estructurales
    ntProgram,       //Nodo raíz del programa completo: program MiPrograma;
    ntDeclarations,  //Sección de declaraciones de variables, tipos, o procedimientos.
    ntBlock          //Bloque de instrucciones (begin...end)
  );
type //Declaraciones y clases base para el AST
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
  TTypeDef = class;
  TDeclarations = class;

  // Listas genéricas especializadas
  TASTNodeList = specialize TFPGObjectList<TASTNode>;
  TVarDeclList = specialize TFPGObjectList<TVarDecl>;
  TProcDeclList = specialize TFPGObjectList<TProcDecl>;
  TFunctionDeclList = specialize TFPGObjectList<TFunctDecl>;
  TExpressionList = specialize TFPGObjectList<TExpression>;
  TCaseBranchList = specialize TFPGObjectList<TCaseBranch>;

  // Nodo base abstracto
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

  // Clase base para contenedores de código (procedimientos, funciones y programa principal)
  TDeclContainer = class(TASTNode)
  private
    FDeclarations: TDeclarations;
  public
    constructor Create(ANodeType: TASTNodeType; const ASrcPos: TSrcPos);
    destructor Destroy; override;
    procedure AddDeclaration(Decl: TASTNode);
    property Declarations: TDeclarations read FDeclarations;
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
  // Acceso a arreglo: variable[index]
  TArrayIndex = class(TExpression)  // Hereda de TExpression
  private
    FArrayVar: TVariableRef;   // La variable arreglo
    FIndices: TExpressionList;  // Lista de índices (multidimensional)
  public
    constructor Create(AArrayVar: TVariableRef; const ASrcPos: TSrcPos);
    destructor Destroy; override;

    procedure AddIndex(Index: TExpression);
    property ArrayVar: TVariableRef read FArrayVar;
    property Indices: TExpressionList read FIndices;

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
    constructor Create(AControlVar: TVariableRef; ADirection: TForDirection;
                       AStartExpr, AEndExpr: TExpression; ABody: TBlock;
                       const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property ControlVar: TVariableRef read FControlVar;
    property Direction: TForDirection read FDirection;
    property StartExpr: TExpression read FStartExpr;
    property EndExpr: TExpression read FEndExpr;
    property Body: TBlock read FBody;

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
    constructor Create(ASelector: TExpression; const ASrcPos: TSrcPos);
    destructor Destroy; override;

    procedure AddBranch(Branch: TCaseBranch);

    property Selector: TExpression read FSelector;
    property Branches: TCaseBranchList read FBranches;
    property ElseBranch: TBlock read FElseBranch write FElseBranch;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Rama de CASE (constante: instrucción)
  TCaseBranch = class(TASTNode)
  private
    FConstants: TExpressionList;  // Lista de constantes
    FStatement: TBlock;          // Instrucción a ejecutar
  public
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;

    procedure AddConstant(ConstExpr: TExpression);
    property Constants: TExpressionList read FConstants;
    property Statement: TBlock read FStatement write FStatement;

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
  TProcDecl = class(TDeclContainer)
  private
    FName: string;
    FParameters: TVarDeclList;
    FBody: TBlock;
  public
    property Name: string read FName;
    procedure AddParameter(Param: TVarDecl);
    property Parameters: TVarDeclList read FParameters;
    property Body: TBlock read FBody write FBody;
  public  //Inicialización y depuración
    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
    constructor Create(const AName: string; const ASrcPos: TSrcPos);
    destructor Destroy; override;
  end;
  { TFunctDecl }
  // Declaración de función
  TFunctDecl = class(TDeclContainer)
  private
    FName: string;
    //FReturnType: TDataType;
    FReturnTypeName: string;
    FParameters: TVarDeclList;
    FBody: TBlock;
  public
    property Name: string read FName;
    procedure AddParameter(Param: TVarDecl);
    property ReturnTypeName: string read FReturnTypeName;
    property Parameters: TVarDeclList read FParameters;
    property Body: TBlock read FBody write FBody;
  public  //Inicialización y depuración
    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
    constructor Create(const AName: string;
                       const AReturnTypeName: string; const ASrcPos: TSrcPos);
    destructor Destroy; override;
  end;
type  //Definiciones previas para declaraciones de tipos
  //Categoría de tipos
  TTypeKind = (
    tkSimple,      //Tipo simple (integer, byte, etc.)
    tkSubrange,    //Subrango (1..10)
    tkEnum,        //Enumerado (Rojo, Verde, Azul)
    tkArray,       //Arreglo (array[1..10] of integer)
    tkRecord,      //Registro (record ... end)
    tkPointer,     //Puntero (^integer)
    tkAlias        //Alias (type TEdad = integer)
  );
  { TArrayRange }
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

  //Clase base para las declaraciones de tipo
  TTypeDef = class
    private
      FTypeKind: TTypeKind;
      FTypeName: string;  // Nombre del tipo (para tipos simples o alias)
    public
      constructor Create(ATypeKind: TTypeKind; const ATypeName: string = '');

      property TypeKind: TTypeKind read FTypeKind;
      property TypeName: string read FTypeName write FTypeName;

      function ToString: string; virtual;
      procedure PrintDebug(Indent: Integer = 0); virtual;
    end;
type  //Nodos de declaraciones de tipos
  // Tipo simple (integer, byte, boolean, etc.)
  TSimpleTypeDef = class(TTypeDef)
  public
    constructor Create(const ATypeName: string);

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Subrango (1..10, 'a'..'z')
  TSubrangeTypeDef = class(TTypeDef)
  private
    FLowExpr: TExpression;
    FHighExpr: TExpression;
  public
    constructor Create(ALowExpr, AHighExpr: TExpression);
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
    constructor Create;
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
  public
    constructor Create(const ABaseTypeName: string);

    property BaseTypeName: string read FBaseTypeName;

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
    constructor Create;
    destructor Destroy; override;

    procedure AddRange(Range: TArrayRange);
    property IndexRanges: TArrayRangeList read FIndexRanges;
    property ElementTypeName: string read FElementTypeName write FElementTypeName;
    property ElementTypeDef: TTypeDef read FElementTypeDef write FElementTypeDef;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Registro (record ... end)
  // ============================================================
  TRecordTypeDef = class(TTypeDef)
  private
    FFields: TFieldDefList;  // Lista de TFieldDef
  public
    constructor Create;
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
    constructor Create(const ATargetTypeName: string);
    destructor Destroy; override;

    property TargetTypeName: string read FTargetTypeName;
    property TargetTypeDef: TTypeDef read FTargetTypeDef write FTargetTypeDef;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  { TTypeDecl }
  // Declaración de tipo (TTypeDecl actualizado)
  // ============================================================
  TTypeDecl = class(TASTNode)
  private
    FName: string;
    FTypeDef: TTypeDef;  // ← AHORA es un objeto estructurado
  public
    constructor Create(const AName: string; ATypeDef: TTypeDef; const ASrcPos: TSrcPos);
    destructor Destroy; override;

    property Name: string read FName;
    property TypeDef: TTypeDef read FTypeDef;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
{

  { TArrayType }
  // Declaración de tipo arreglo: array[1..10] of integer
  TArrayType = class(TASTNode)
  private
    FIndexRanges: TArrayRangeList;  //Lista de TArrayRange (multidimensional)
    FElementType: string;         //Nombre del tipo de los elementos
  public
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;

    procedure AddRange(Range: TArrayRange);
    property IndexRanges: TArrayRangeList read FIndexRanges;
    property ElementType: string read FElementType write FElementType;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;

  // Declaración de campo dentro de un RECORD
  TFieldDecl = class(TASTNode)
  private
    FName: string;
    FTypeName: string;
  public
    constructor Create(const AName, ATypeName: string; const ASrcPos: TSrcPos);

    property Name: string read FName;
    property TypeName: string read FTypeName;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  TFieldDeclList = specialize TFPGObjectList<TFieldDecl>;

  // Declaración de tipo RECORD
  TRecordType = class(TASTNode)
  private
    FFields: TFieldDeclList;  // Lista de TFieldDecl
  public
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;

    procedure AddField(Field: TFieldDecl);
    property Fields: TFieldDeclList read FFields;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
}
type  //Nodos estructurales
  // Contenedor de declaraciones
  TDeclarations = class(TASTNode)
  private
    FItems: TASTNodeList;  // Mezcla de VarDecl, ProcDecl, FunctionDecl
  public
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;

    procedure AddDeclaration(Decl: TASTNode);
    property Items: TASTNodeList read FItems;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  // Bloque (lista de instrucciones)
  TBlock = class(TASTNode)
  private
    FStatements: TASTNodeList;
  public
    constructor Create(const ASrcPos: TSrcPos);
    destructor Destroy; override;

    procedure AddStatement(Statement: TASTNode);
    property Statements: TASTNodeList read FStatements;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  { TProgram }
  // Programa completo
  TProgram = class(TDeclContainer)
  private
    FName: string;
    FMainBody: TBlock;
  public
    procedure Clear;

    property Name: string read FName write FName;
    property srcDec: TSrcPos write FSrcPos;
    property MainBody: TBlock read FMainBody write FMainBody;
  public  //Inicialización y depuración
    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
    constructor Create(const AName: string; const ASrcPos: TSrcPos);
    destructor Destroy; override;
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
// TDeclContainer
constructor TDeclContainer.Create(ANodeType: TASTNodeType; const ASrcPos: TSrcPos);
begin
  inherited Create(ANodeType, ASrcPos);
  FDeclarations := TDeclarations.Create(ASrcPos);
end;
destructor TDeclContainer.Destroy;
begin
  FDeclarations.Free;
  inherited;
end;
procedure TDeclContainer.AddDeclaration(Decl: TASTNode);
begin
  FDeclarations.AddDeclaration(Decl);
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
// TArrayIndex
constructor TArrayIndex.Create(AArrayVar: TVariableRef; const ASrcPos: TSrcPos);
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
procedure TArrayIndex.AddIndex(Index: TExpression);
begin
  FIndices.Add(Index);
end;
function TArrayIndex.ToString: string;
begin
  Result := Format('ArrayIndex: %s (%d indices)',
                   [FArrayVar.Name, FIndices.Count]);
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
begin
  if FTarget.NodeType = ntVariableRef then begin
    Result := Format('Assignment: %s := ...', [TVariableRef(FTarget).Name]);
    Result := Result + Format(' at %s', [FSrcPos.RowColString]);
  end else begin
    Result := 'Assignment: <Expression> := ...';
  end;
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
  FBranches.Free;
  FreeAndNil(FElseBranch);
  inherited;
end;
procedure TCaseStatement.AddBranch(Branch: TCaseBranch);
begin
  FBranches.Add(Branch);
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
procedure TProcDecl.AddParameter(Param: TVarDecl);
begin
  Param.IsParameter := True;
  FParameters.Add(Param);
end;
function TProcDecl.ToString: string;
begin
  Result := Format('Procedure: %s (%d params, %d locals)',
                   [FName, FParameters.Count, FDeclarations.Items.Count]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TProcDecl.PrintDebug(Indent: Integer = 0);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  if FParameters.Count > 0 then begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Parameters:');
    for i := 0 to FParameters.Count - 1 do
      FParameters[i].PrintDebug(Indent + 4);
  end;
  if FDeclarations.Items.Count > 0 then begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Local declarations:');
    for i := 0 to FDeclarations.Items.Count - 1 do
      FDeclarations.Items[i].PrintDebug(Indent + 4);
  end;
  if FBody <> nil then begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Body:');
    FBody.PrintDebug(Indent + 4);
  end;
end;
constructor TProcDecl.Create(const AName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntProcDecl, ASrcPos);
  FName := AName;
  FParameters := TVarDeclList.Create(True);
  FBody := TBlock.Create(ASrcPos);
end;
destructor TProcDecl.Destroy;
begin
  FBody.Free;
  FParameters.Free;
  inherited;
end;
// TFunctDecl
procedure TFunctDecl.AddParameter(Param: TVarDecl);
begin
  Param.IsParameter := True;
  FParameters.Add(Param);
end;
function TFunctDecl.ToString: string;
begin
  Result := Format('Function: %s: %s (%d params, %d locals)',
           [FName, FReturnTypeName, FParameters.Count, FDeclarations.Items.Count]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TFunctDecl.PrintDebug(Indent: Integer = 0);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  if FParameters.Count > 0 then begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Parameters:');
    for i := 0 to FParameters.Count - 1 do
      FParameters[i].PrintDebug(Indent + 4);
  end;
  if FDeclarations.Items.Count > 0 then begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Local declarations:');
    for i := 0 to FDeclarations.Items.Count - 1 do
      FDeclarations.Items[i].PrintDebug(Indent + 4);
  end;
  if FBody <> nil then begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Body:');
    FBody.PrintDebug(Indent + 4);
  end;
end;
constructor TFunctDecl.Create(const AName: string;
  const AReturnTypeName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntFunction, ASrcPos);
  FName := AName;
  FReturnTypeName := AReturnTypeName;
  FParameters := TVarDeclList.Create(True);
  FBody := TBlock.Create(ASrcPos);
end;
destructor TFunctDecl.Destroy;
begin
  FBody.Free;
  FParameters.Free;
  inherited;
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
constructor TTypeDef.Create(ATypeKind: TTypeKind; const ATypeName: string);
begin
  FTypeKind := ATypeKind;
  FTypeName := ATypeName;
end;
function TTypeDef.ToString: string;
begin
  Result := Format('TypeDef(%d)', [Ord(FTypeKind)]);
end;
procedure TTypeDef.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
end;
{$endregion}
{$region "Nodos de declaraciones de tipos"}
// TSimpleTypeDef
constructor TSimpleTypeDef.Create(const ATypeName: string);
begin
  inherited Create(tkSimple, ATypeName);
end;
function TSimpleTypeDef.ToString: string;
begin
  Result := Format('SimpleType: %s', [FTypeName]);
end;
procedure TSimpleTypeDef.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
end;
// TSubrangeTypeDef
constructor TSubrangeTypeDef.Create(ALowExpr, AHighExpr: TExpression);
begin
  inherited Create(tkSubrange);
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
constructor TEnumTypeDef.Create;
begin
  inherited Create(tkEnum);
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
constructor TAliasTypeDef.Create(const ABaseTypeName: string);
begin
  inherited Create(tkAlias);
  FBaseTypeName := ABaseTypeName;
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
constructor TArrayTypeDef.Create;
begin
  inherited Create(tkArray);
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
procedure TArrayTypeDef.AddRange(Range: TArrayRange);
begin
  FIndexRanges.Add(Range);
end;
function TArrayTypeDef.ToString: string;
var
  typName: String;
begin
  if FElementTypeDef <> nil then typName := FElementTypeDef.TypeName
  else typName:= FElementTypeName;
  Result := Format('Array: [%d dims] of %s', [FIndexRanges.Count, typName]);
end;
procedure TArrayTypeDef.PrintDebug(Indent: Integer = 0);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  WriteLn(StringOfChar(' ', Indent + 2), 'Index ranges:');
  for i := 0 to FIndexRanges.Count - 1 do
    FIndexRanges[i].PrintDebug(Indent + 4);
  WriteLn(StringOfChar(' ', Indent + 2), 'Element type: ',
           FElementTypeName);
  if FElementTypeDef <> nil then
    FElementTypeDef.PrintDebug(Indent + 4);
end;
// TRecordTypeDef
constructor TRecordTypeDef.Create;
begin
  inherited Create(tkRecord);
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
constructor TPointerTypeDef.Create(const ATargetTypeName: string);
begin
  inherited Create(tkPointer);
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
// TTypeDecl
constructor TTypeDecl.Create(const AName: string; ATypeDef: TTypeDef; const ASrcPos: TSrcPos);
begin
  inherited Create(ntTypeDecl, ASrcPos);
  FName := AName;
  FTypeDef := ATypeDef;
end;
destructor TTypeDecl.Destroy;
begin
  FTypeDef.Free;
  inherited;
end;
function TTypeDecl.ToString: string;
begin
  Result := Format('TypeDecl: %s = %s', [FName, FTypeDef.ToString]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TTypeDecl.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  FTypeDef.PrintDebug(Indent + 2);
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
procedure TDeclarations.AddDeclaration(Decl: TASTNode);
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
// TProgram
procedure TProgram.Clear;
{Limpia al árbol de sintaxis y lo deja listo para inicia el llenado}
begin
  //Limpiar declaraciones (eliminar todos los elementos)
  FDeclarations.Items.Clear;
  //Limpiar cuerpo principal (eliminar todas las instrucciones)
  FMainBody.Statements.Clear;
end;
function TProgram.ToString: string;
begin
  Result := Format('Program: %s', [FName]);
  if FDeclarations <> nil then
    Result := Result + Format(' (%d decls)', [FDeclarations.Items.Count]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TProgram.PrintDebug(Indent: Integer = 0);
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  if FDeclarations <> nil then begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Declarations:');
    FDeclarations.PrintDebug(Indent + 4);
  end;
  if FMainBody <> nil then begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Main body:');
    FMainBody.PrintDebug(Indent + 4);
  end;
end;
constructor TProgram.Create(const AName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntProgram, ASrcPos);
  FName := AName;
  {Crea los elementos fijos del programa. Notar que FMainBody (que representa al cuerpo
  del programa principal) se crea en la misma posición que el programa, lo cual no es
  tan consistente porque FMainBody debería apuntar al BEGIN del programa principal,
  pero se puede actualizar después.
  Se crea aquí, al crear al programa, para controlar su construcción y destrucción.}
  FMainBody := TBlock.Create(ASrcPos);
end;
destructor TProgram.Destroy;
begin
  FreeAndNil(FMainBody);
  inherited;
end;
{$endregion}
end.
