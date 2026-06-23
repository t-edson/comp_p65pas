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
    //Nodos de sentencias
    ntAssignment,    //Asignación de valor a variable.
    ntIfStatement,   //Condicional IF-THEN-ELSE.
    ntProcedureCall, //Llamada a un procedimiento
    ntWhileLoop,     //Bucle WHILE-DO.
    ntRepeatUntil,   //Bucle REPEAT-UNTIL.
    ntForLoop,       //Bucle FOR-TO/DOWNTO-DO.
    ntCaseStatement, //Estructura CASE.
    ntCaseBranch,    //Rama individual de un CASE.
    //Nodos de declaraciones
    ntVarDecl,       //Declaración de variable: var x: byte;
    ntProcDecl,      //Declaración de procedimiento: procedure algo; begin ... end;
    ntFunction,      //Declaración de función.
    ntParamDecl,     //Parámetro de procedimiento/función: var x: byte
    ntTypeDecl,      //Declaración de tipo: type mi_tipo = byte;
    ntArrayType,     //Declaración de tipo arreglo
    ntArrayRange,    //Rango de arreglo (1..10)
    //Nodos estructurales
    ntProgram,       //Nodo raíz del programa completo: program MiPrograma;
    ntDeclarations,  //Sección de declaraciones de variables, tipos, o procedimientos.
    ntBlock          //Bloque de instrucciones (begin...end)
  );
type //Declaraciones y clases base: TASTNode y TExpression
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
  TProcedureCall = class;
  TCaseBranch = class;
  TProcDecl = class;
  TFunctionDecl = class;
  TDeclarations = class;

  // Listas genéricas especializadas
  TASTNodeList = specialize TFPGObjectList<TASTNode>;
  TVarDeclList = specialize TFPGObjectList<TVarDecl>;
  TProcDeclList = specialize TFPGObjectList<TProcDecl>;
  TFunctionDeclList = specialize TFPGObjectList<TFunctionDecl>;
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
  TNumberLiteral = class(TExpression)
  private
    FValue: Integer;
    //FDataType: TDataType;
  public
    constructor Create(AValue: Integer; const ASrcPos: TSrcPos);

    property Value: Integer read FValue;

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
  // Llamada a función (expresión)
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
  // Llamada a procedimiento (sentencia)
  TProcedureCall = class(TASTNode)
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
  // Declaración de procedimiento
  TProcDecl = class(TASTNode)
  private
    FName: string;
    FParameters: TVarDeclList;
    FLocalDeclarations: TVarDeclList;
    FBody: TBlock;
  public
    constructor Create(const AName: string; const ASrcPos: TSrcPos);
    destructor Destroy; override;

    procedure AddParameter(Param: TVarDecl);
    procedure AddLocalDecl(Decl: TVarDecl);

    property Name: string read FName;
    property Parameters: TVarDeclList read FParameters;
    property LocalDeclarations: TVarDeclList read FLocalDeclarations;
    property Body: TBlock read FBody write FBody;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  { TFunctionDecl }
  // Declaración de función
  TFunctionDecl = class(TASTNode)
  private
    FName: string;
    //FReturnType: TDataType;
    FReturnTypeName: string;
    FParameters: TVarDeclList;
    FLocalDeclarations: TVarDeclList;
    FBody: TBlock;
  public
    constructor Create(const AName: string;
                       const AReturnTypeName: string; const ASrcPos: TSrcPos);
    destructor Destroy; override;

    procedure AddParameter(Param: TVarDecl);
    procedure AddLocalDecl(Decl: TVarDecl);

    property Name: string read FName;
    property ReturnTypeName: string read FReturnTypeName;
    property Parameters: TVarDeclList read FParameters;
    property LocalDeclarations: TVarDeclList read FLocalDeclarations;
    property Body: TBlock read FBody write FBody;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;
  end;
  { TTypeDecl }
  TTypeDecl = class(TASTNode)
  private
    FName: string;
    FTypeDefinition: string;  // Simplificado: guardar como string
    // O mejor: un AST para la definición del tipo
  public
    constructor Create(const AName, ATypeDefinition: string; const ASrcPos: TSrcPos);
    property Name: string read FName;
    property TypeDefinition: string read FTypeDefinition;
  end;

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

  { TArrayIndex }
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
type  //Nodos estructurales
  { TProgram }
  // Programa completo
  TProgram = class(TASTNode)
  private
    FName: string;
    FDeclarations: TDeclarations;  // UNIFICADO: todas las declaraciones
    FMainBody: TBlock;
  public
    constructor Create(const AName: string; const ASrcPos: TSrcPos);
    destructor Destroy; override;
    procedure Clear;
    // Métodos de conveniencia para añadir declaraciones
    procedure AddGlobalDecl(Decl: TVarDecl);
    procedure AddProcedure(Proc: TProcDecl);
    procedure AddFunction(Func: TFunctionDecl);
    procedure AddTypeDecl(Decl: TTypeDecl);

    property Name: string read FName write FName;
    property srcDec: TSrcPos write FSrcPos;
    property Declarations: TDeclarations read FDeclarations;
    property MainBody: TBlock read FMainBody write FMainBody;

    function ToString: string; override;
    procedure PrintDebug(Indent: Integer = 0); override;

  end;
  // Contenedor de declaraciones (UNIFICADO - mantiene orden)
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
constructor TNumberLiteral.Create(AValue: Integer; const ASrcPos: TSrcPos);
begin
  inherited Create(ntNumberLiteral, ASrcPos);
  FValue := AValue;
end;
function TNumberLiteral.ToString: string;
begin
  Result := Format('NumberLiteral: %d', [FValue]);
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
// TProcedureCall
constructor TProcedureCall.Create(const AName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntProcedureCall, ASrcPos);
  FName := AName;
  FArguments := TExpressionList.Create(True);
end;
destructor TProcedureCall.Destroy;
begin
  FArguments.Free;
  inherited;
end;
procedure TProcedureCall.AddArgument(Arg: TExpression);
begin
  FArguments.Add(Arg);
end;
function TProcedureCall.ToString: string;
begin
  Result := Format('ProcedureCall: %s (%d args)', [FName, FArguments.Count]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TProcedureCall.PrintDebug(Indent: Integer = 0);
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
// TProcDecl
constructor TProcDecl.Create(const AName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntProcDecl, ASrcPos);
  FName := AName;
  FParameters := TVarDeclList.Create(True);
  FLocalDeclarations := TVarDeclList.Create(True);
  FBody := nil;
end;
destructor TProcDecl.Destroy;
begin
  FParameters.Free;
  FLocalDeclarations.Free;
  FreeAndNil(FBody);
  inherited;
end;
procedure TProcDecl.AddParameter(Param: TVarDecl);
begin
  Param.IsParameter := True;
  FParameters.Add(Param);
end;
procedure TProcDecl.AddLocalDecl(Decl: TVarDecl);
begin
  FLocalDeclarations.Add(Decl);
end;
function TProcDecl.ToString: string;
begin
  Result := Format('Procedure: %s (%d params, %d locals)',
                   [FName, FParameters.Count, FLocalDeclarations.Count]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TProcDecl.PrintDebug(Indent: Integer = 0);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);

  if FParameters.Count > 0 then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Parameters:');
    for i := 0 to FParameters.Count - 1 do
      FParameters[i].PrintDebug(Indent + 4);
  end;

  if FLocalDeclarations.Count > 0 then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Local declarations:');
    for i := 0 to FLocalDeclarations.Count - 1 do
      FLocalDeclarations[i].PrintDebug(Indent + 4);
  end;

  if FBody <> nil then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Body:');
    FBody.PrintDebug(Indent + 4);
  end;
end;
// TFunctionDecl
constructor TFunctionDecl.Create(const AName: string;
  const AReturnTypeName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntFunction, ASrcPos);
  FName := AName;
  FReturnTypeName := AReturnTypeName;
  FParameters := TVarDeclList.Create(True);
  FLocalDeclarations := TVarDeclList.Create(True);
  FBody := nil;
end;
destructor TFunctionDecl.Destroy;
begin
  FParameters.Free;
  FLocalDeclarations.Free;
  FreeAndNil(FBody);
  inherited;
end;
procedure TFunctionDecl.AddParameter(Param: TVarDecl);
begin
  Param.IsParameter := True;
  FParameters.Add(Param);
end;
procedure TFunctionDecl.AddLocalDecl(Decl: TVarDecl);
begin
  FLocalDeclarations.Add(Decl);
end;
function TFunctionDecl.ToString: string;
begin
  Result := Format('Function: %s: %s (%d params, %d locals)',
                   [FName, FReturnTypeName, FParameters.Count, FLocalDeclarations.Count]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TFunctionDecl.PrintDebug(Indent: Integer = 0);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);

  if FParameters.Count > 0 then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Parameters:');
    for i := 0 to FParameters.Count - 1 do
      FParameters[i].PrintDebug(Indent + 4);
  end;

  if FLocalDeclarations.Count > 0 then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Local declarations:');
    for i := 0 to FLocalDeclarations.Count - 1 do
      FLocalDeclarations[i].PrintDebug(Indent + 4);
  end;

  if FBody <> nil then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Body:');
    FBody.PrintDebug(Indent + 4);
  end;
end;
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
{ TTypeDecl }
constructor TTypeDecl.Create(const AName, ATypeDefinition: string;
  const ASrcPos: TSrcPos);
begin
  inherited Create(ntTypeDecl, ASrcPos);
  FName := AName;
  FTypeDefinition := ATypeDefinition
end;
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
{ TArrayType }
constructor TArrayType.Create(const ASrcPos: TSrcPos);
begin
  inherited Create(ntArrayType, ASrcPos);
  FIndexRanges := TArrayRangeList.Create(True);  // True = owns objects
  FElementType := '';
end;
destructor TArrayType.Destroy;
begin
  FIndexRanges.Free;
  inherited Destroy;
end;
procedure TArrayType.AddRange(Range: TArrayRange);
begin
  FIndexRanges.Add(Range);
end;
function TArrayType.ToString: string;
begin
  Result := Format('ArrayType: [%d dims] of %s',
                   [FIndexRanges.Count, FElementType]);
  Result := Result + Format(' at %s', [FSrcPos.RowColString]);
end;
procedure TArrayType.PrintDebug(Indent: Integer);
var
  i: Integer;
begin
  WriteLn(StringOfChar(' ', Indent), ToString);
  if FIndexRanges.Count > 0 then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Index ranges:');
    for i := 0 to FIndexRanges.Count - 1 do
      FIndexRanges[i].PrintDebug(Indent + 4);
  end;
  WriteLn(StringOfChar(' ', Indent + 2), 'Element type: ', FElementType);
end;

{ TArrayIndex }

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
// TProgram
constructor TProgram.Create(const AName: string; const ASrcPos: TSrcPos);
begin
  inherited Create(ntProgram, ASrcPos);
  FName := AName;
  {Crea los elementos fijos del programa. Notar que FMainBody (que representa al cuerpo
  del programa principal) se crea en la misma posición que el programa, lo cual no es
  tan consistente porque FMainBody debería apuntar al BEGIN del programa principal,
  pero se puede actualizar después.
  Se crea aquí, al crear al programa, para controlar su construcción y destrucción.}
  FDeclarations := TDeclarations.Create(ASrcPos);
  FMainBody := TBlock.Create(ASrcPos);
end;
destructor TProgram.Destroy;
begin
  FDeclarations.Free;
  FreeAndNil(FMainBody);
  inherited;
end;
procedure TProgram.Clear;
begin
  // 1. Limpiar declaraciones (eliminar todos los elementos)
  //    TDeclarations.Items es TASTNodeList con OwnsObjects=True,
  //    por lo que los elementos se liberan automáticamente
  FDeclarations.Items.Clear;

  // 2. Limpiar cuerpo principal (eliminar todas las instrucciones)
  //    TBlock.Statements es TASTNodeList con OwnsObjects=True,
  //    por lo que los elementos se liberan automáticamente
  FMainBody.Statements.Clear;
end;
procedure TProgram.AddGlobalDecl(Decl: TVarDecl);
begin
  FDeclarations.AddDeclaration(Decl);
end;
procedure TProgram.AddProcedure(Proc: TProcDecl);
begin
  FDeclarations.AddDeclaration(Proc);
end;
procedure TProgram.AddFunction(Func: TFunctionDecl);
begin
  FDeclarations.AddDeclaration(Func);
end;
procedure TProgram.AddTypeDecl(Decl: TTypeDecl);
begin
  FDeclarations.AddDeclaration(Decl);
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

  if FDeclarations <> nil then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Declarations:');
    FDeclarations.PrintDebug(Indent + 4);
  end;

  if FMainBody <> nil then
  begin
    WriteLn(StringOfChar(' ', Indent + 2), 'Main body:');
    FMainBody.PrintDebug(Indent + 4);
  end;
end;

end.
