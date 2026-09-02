{Unidad que implementa las estructuras de datos que se usan en el Análisis Semántico.}
unit SemAnalizer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Math, AstPascal, LazLogger, alexiaLex;
type
  //Tipos de símbolos
  TSymbolKind = (
    skVariable,      //Variable
    skConstant,      //Constante
    skType,          //Tipo
    skProcedure,     //Procedimiento
    skFunction,      //Función
    skParameter,     //Parámetro
    skField,         //Campo
    skEnumValue      //Valor de enumerado
  );

  //Clase TScope
  TScope = class;

  //Símbolo (identificador declarado)
  TSymbol = class
  private
    FName: string;
    FKind: TSymbolKind;
    FDataType: TTypeDef;
    FDeclaration: TASTNode;
    FScope: TScope;
    FIsForward: Boolean;
    FIsIntrinsic: Boolean;
    FParameters: TASTNodeList;
    FReturnType: TTypeDef;
    FIsDataTypeOwner: Boolean;   //Indica si es propietario del objeto FDataType
    FIsReturnTypeOwner: Boolean; //Indica si es propietario del objeto FReturnType
    FIsSelf: Boolean;            //Indica que es la referencia a "Self".
  public
    property Name: string read FName;
    property Kind: TSymbolKind read FKind;
    property DataType: TTypeDef read FDataType write FDataType;
    property Declaration: TASTNode read FDeclaration write FDeclaration;
    property Scope: TScope read FScope write FScope;
    property IsForward: Boolean read FIsForward write FIsForward;
    property IsIntrinsic: Boolean read FIsIntrinsic write FIsIntrinsic;
    property Parameters: TASTNodeList read FParameters write FParameters;
    //Tipo de retorno. Usado solo para procedimientos y funciones.
    {Se mantiene separado de "DataType" porque son semánticamente diferentes.}
    property ReturnType: TTypeDef read FReturnType write FReturnType;
    //Banderas
    property IsDataTypeOwner: Boolean read FIsDataTypeOwner write FIsDataTypeOwner;
    property IsReturnTypeOwner: Boolean read FIsReturnTypeOwner write FIsReturnTypeOwner;
    property IsSelf: Boolean read FIsSelf write FIsSelf;
  public //Inicialización
    constructor Create(const AName: string; AKind: TSymbolKind);
    destructor Destroy; override;
  end;

  //Ámbito (scope)
  TScope = class
  private
    FParent: TScope;         //Ámbito padre
    FSymbols: TStringList;   //Lista de símbolos
    FChildScopes: TList;
  public
    procedure Declare(Sym: TSymbol);
    function Lookup(const AName: string): TSymbol;
    function LookupRecursive(const AName: string): TSymbol;
    function GetSymbols: TStringList;
    procedure AddChild(Child: TScope);
    property Parent: TScope read FParent;
  public  //Inicialización
    procedure Clear;
    constructor Create(AParent: TScope = nil);
    destructor Destroy; override;
  end;

  //Analizador semántico
  TSemanticAnalyzer = class
  private
    msg: TMessageManager;    //Referencia al gestor de mensajes
    lex: TAleLexer;
    FGlobalScope: TScope;    //Ámbito global
    FCurrentScope: TScope;   //Ámbito actual
    FCurrentProcedure: TProcFunctDecl;
    FErrors: Integer;
    FWarnings: Integer;
    FCurrentUnit: TUnit;
    FInWith: Boolean;
    FWithScope: TScope;
    procedure VisitTypeDecl(TypeDecl: TTypeDecl);
  public   //Atributos publicos
    property Errors: Integer read FErrors;
    property Warnings: Integer read FWarnings;
    property GlobalScope: TScope read FGlobalScope;
  private  //Utilidades y ámbitos
    //Utilidades
    function IsInFunction: Boolean;
    function IsInProcedure: Boolean;
    function GetCurrentFunction: TProcFunctDecl;
    function GetCurrentProcedure: TProcFunctDecl;
    procedure CheckForwardDeclarations;
    function CompareParameters(Sym: TSymbol; Proc: TProcFunctDecl): Boolean;
    //Mensajes
    procedure Error(const txt: string; const SrcPos: TSrcPos);
    procedure Warning(const txt: string; const SrcPos: TSrcPos);
    // Manejo de ámbitos
    procedure EnterScope;
    procedure ExitScope;
    procedure EnterWithScope(RecordVar: TExpression);
    procedure ExitWithScope;
  private  //Resolución de tipos
    function ResolveType(const TypeName: string): TTypeDef;
    function GetFinalTypeDef(TypeDef: TTypeDef): TTypeDef;
    function ResolveTypeDef(TypeDef: TTypeDef): TTypeDef;
    public
    function GetTypeOf(Expr: TExpression): TTypeDef;
    private
    function AreTypesCompatible(T1, T2: TTypeDef): Boolean;
    function IsNumericType(TypeDef: TTypeDef): Boolean;
    function IsOrdinalType(TypeDef: TTypeDef): Boolean;
  private  //Registro de símbolos
    procedure RegisterDeclarations(Decls: TASTNodeList);
    procedure RegisterProcDecl(ProcFunctDecl: TProcFunctDecl);
    procedure RegisterVarDecl(VarDecl: TVarDecl);
    procedure RegisterConstDecl(ConstDecl: TConstDecl);
    procedure RegisterTypeDecl(TypeDecl: TTypeDecl);
  private  //Visitantes de declaraciones y definiciones de tipos
    procedure VisitVarDecl(VarDecl: TVarDecl);
    procedure VisitConstDecl(ConstDecl: TConstDecl);
    procedure VisitProcFunctDecl(ProcFunctDecl: TProcFunctDecl);
    //Definiciones de tipos
    procedure VisitAliasTypeDef(TypeDef: TTypeDef);
    procedure VisitArrayTypeDef(ArrayType: TArrayTypeDef);
    procedure VisitRecordTypeDef(RecordType: TRecordTypeDef);
    procedure VisitEnumTypeDef(EnumType: TEnumTypeDef);
    procedure VisitSubrangeTypeDef(SubrangeType: TSubranTypeDef);
    procedure VisitPointerTypeDef(PointerType: TPointerTypeDef);
  private  //Visitantes de sentencias
    procedure VisitAssignment(Assign: TAssignment);
    procedure VisitIfStatement(IfStmt: TIfStatement);
    procedure VisitWhileLoop(WhileLoop: TWhileLoop);
    procedure VisitRepeatUntil(RepeatUntil: TRepeatUntil);
    procedure VisitForLoop(ForLoop: TForLoop);
    procedure VisitCaseStatement(CaseStmt: TCaseStatement);
    procedure VisitCaseBranch(CaseBranch: TCaseBranch);
    procedure VisitWithStatement(WithStmt: TWithStatement);
    procedure VisitExitStatement(ExitStmt: TExitStatement);
  private  //Visitantes de expresiones
    procedure VisitVariableRef(VarRef: TVariableRef);
    procedure VisitNumberLiteral(NumLit: TNumberLiteral);
    procedure VisitBooleanLiteral(BoolLit: TBooleanLiteral);
    procedure VisitStringLiteral(StrLit: TStringLiteral);
    procedure VisitBinaryOp(BinOp: TBinaryOp);
    procedure VisitUnaryOp(UnaryOp: TUnaryOp);
    procedure VisitFunctionCall(ProcFuncCall: TProcFunctCall);
    procedure VisitFieldAccess(FieldAccess: TFieldAccess);
    procedure VisitPointerDeref(PointerDeref: TPointerDeref);
    procedure VisitArrayRef(ArrayRef: TArrayRef);
    procedure VisitArrayLiteral(ArrayLit: TArrayLiteral);
    procedure VisitRecordLiteral(RecordLit: TRecordLiteral);
    procedure VisitPointerLiteral(PointerLit: TPointerLiteral);
  private  //Visitantes de nodos principales
    procedure VisitNode(Node: TASTNode);
    procedure VisitBlock(Block: TBlock);
    procedure VisitProgram(Prog: TProgram);
    procedure VisitUnit(Unit0: TUnit);
  public   //Análisis e Inicialización
    function Analyze(Prog: TProgram): Boolean; overload;
    function Analyze(Unit0: TUnit): Boolean; overload;
    procedure Reset;
    procedure RegisterBuiltinTypes;
    procedure RegisterIntrinsicProcedures;
    constructor Create(Amsg: TMessageManager; Alex: TAleLexer);
    destructor Destroy; override;
  end;

implementation

function CompareExpressions(Expr1, Expr2: TExpression): Integer;
var
  Val1, Val2: Int64;
begin
  // Simplificado: asumimos que son literales numéricos o de caracteres
  // En una implementación real, necesitarías evaluar las expresiones
  if (Expr1.NodeType = ntNumberLiteral) and (Expr2.NodeType = ntNumberLiteral) then begin
    Val1 := TNumberLiteral(Expr1).IntValue;
    Val2 := TNumberLiteral(Expr2).IntValue;
    if Val1 < Val2 then Result := -1
    else if Val1 > Val2 then Result := 1
    else Result := 0;
  end else begin
    Result := 0;  // No se puede comparar
  end;
end;
{$region "TSymbol y TScope"}
{ TSymbol }
constructor TSymbol.Create(const AName: string; AKind: TSymbolKind);
begin
  FName := AName;
  FKind := AKind;
  FDataType := nil;
  FDeclaration := nil;
  FScope := nil;
  FIsForward := False;
  FParameters := nil;
  FReturnType := nil;
end;
destructor TSymbol.Destroy;
begin
  FParameters.Free;   //Libera por si se ha usado
  //Libera tipo de datos, si este analizador es el propietario
  if FIsDataTypeOwner then FDataType.Free;
  //Libera tipo de retorno, si este analizador es el propietario
  if FIsReturnTypeOwner then FReturnType.Free;
  inherited;
end;
{ TScope }
procedure TScope.Declare(Sym: TSymbol);
var
  i: Integer;
begin
  if FSymbols.Find(Sym.Name, i) then
    raise Exception.Create('Símbolo ya declarado: ' + Sym.Name);
  Sym.Scope := Self;
  FSymbols.AddObject(Sym.Name, Sym);
end;
function TScope.Lookup(const AName: string): TSymbol;
var
  i: Integer;
begin
  if FSymbols.Find(AName, i) then
    Result := TSymbol(FSymbols.Objects[i])
  else
    Result := nil;
end;
function TScope.LookupRecursive(const AName: string): TSymbol;
var
  Scope: TScope;
begin
  Scope := Self;
  while Scope <> nil do begin
    Result := Scope.Lookup(AName);
    if Result <> nil then
      Exit;
    Scope := Scope.FParent;
  end;
  Result := nil;
end;
function TScope.GetSymbols: TStringList;
begin
  Result := FSymbols;
end;
procedure TScope.AddChild(Child: TScope);
begin
  FChildScopes.Add(Child);
end;
procedure TScope.Clear;
var
  i: Integer;
begin
  //Liberar símbolos
  for i := 0 to FSymbols.Count - 1 do begin
    FSymbols.Objects[i].Destroy;
  end;
  FSymbols.Clear;
  //Liberar ámbitos hijos recursivamente
  for i := 0 to FChildScopes.Count - 1 do begin
    TScope(FChildScopes[i]).Destroy;
  end;
  FChildScopes.Clear;
end;
constructor TScope.Create(AParent: TScope = nil);
begin
  FParent := AParent;
  FSymbols := TStringList.Create;
  FSymbols.Sorted := True;
  FChildScopes := TList.Create;
end;
destructor TScope.Destroy;
var
  i: Integer;
begin
  for i := 0 to FChildScopes.Count - 1 do begin
    TScope(FChildScopes[i]).Free;
  end;
  FChildScopes.Destroy;
  for i := 0 to FSymbols.Count - 1 do begin
    FSymbols.Objects[i].Free;
  end;
  FSymbols.Destroy;
  inherited;
end;
{$endregion}
{ TSemanticAnalyzer }
{$region "Utilidades y ámbitos"}
// Utilidades
function TSemanticAnalyzer.IsInFunction: Boolean;
begin
  Result := (FCurrentProcedure <> nil) and FCurrentProcedure.IsFunction;
end;
function TSemanticAnalyzer.IsInProcedure: Boolean;
begin
  Result := (FCurrentProcedure <> nil) and not FCurrentProcedure.IsFunction;
end;
function TSemanticAnalyzer.GetCurrentFunction: TProcFunctDecl;
begin
  if IsInFunction then
    Result := FCurrentProcedure
  else
    Result := nil;
end;
function TSemanticAnalyzer.GetCurrentProcedure: TProcFunctDecl;
begin
  if IsInProcedure then
    Result := FCurrentProcedure
  else
    Result := nil;
end;
procedure TSemanticAnalyzer.CheckForwardDeclarations;
var
  i: Integer;
  Sym: TSymbol;
  Symbols: TStringList;
begin
  //Recorrer todos los símbolos del ámbito global
  Symbols := FGlobalScope.GetSymbols;
  for i := 0 to Symbols.Count - 1 do begin
    Sym := TSymbol(Symbols.Objects[i]);
    if Sym.IsForward then
      Error('Declaración FORWARD sin implementación: ' + Sym.Name,
            Sym.Declaration.SrcPos);
  end;
end;
function TSemanticAnalyzer.CompareParameters(Sym: TSymbol; Proc: TProcFunctDecl): Boolean;
var
  i: Integer;
  Param1, Param2: TVarDecl;
begin
  // Verificar número de parámetros
  if Sym.Parameters = nil then begin
    Result := (Proc.Parameters = nil) or (Proc.Parameters.Count = 0);
    Exit;
  end;
  if (Proc.Parameters = nil) or (Sym.Parameters.Count <> Proc.Parameters.Count) then
    Exit(False);
  // Comparar cada parámetro
  for i := 0 to Sym.Parameters.Count - 1 do begin
    Param1 := TVarDecl(Sym.Parameters[i]);
    Param2 := TVarDecl(Proc.Parameters[i]);
    // Comparar nombres (opcional)
    if Param1.Name <> Param2.Name then Exit(False);
    // Comparar tipos
    if Param1.TypeDef.TypeName <> Param2.TypeDef.TypeName then Exit(False);
    // Comparar tipo de parámetro (var, const, out)
    if Param1.ParamType <> Param2.ParamType then
      Exit(False);
  end;
  Result := True;
end;
//Mensajes
procedure TSemanticAnalyzer.Error(const txt: string; const SrcPos: TSrcPos);
begin
  Inc(FErrors);
  msg.error(lex.GetMsgInfoE(txt, srcPos));
end;
procedure TSemanticAnalyzer.Warning(const txt: string; const SrcPos: TSrcPos);
begin
  Inc(FWarnings);
  msg.warn(lex.GetMsgInfo(txt, srcPos));
end;
//Manejo de ámbitos
procedure TSemanticAnalyzer.EnterScope;
var
  NewScope: TScope;
begin
  //Crea un nuevo ámbito con el ámbito actual como padre
  NewScope := TScope.Create(FCurrentScope);
  //Registrar el hijo en el padre.
  if FCurrentScope <> nil then
    FCurrentScope.AddChild(NewScope);
  //Establece el nuevo ámbito como ámbito actual
  FCurrentScope := NewScope;
end;
procedure TSemanticAnalyzer.ExitScope;
begin
  if FCurrentScope <> nil then begin
    //La liberación se hace desde el Scope padre
    //Parent := FCurrentScope.FParent;
    //FCurrentScope.Free;
    //FCurrentScope := Parent;
    //Retornamos a al Scope padre
    FCurrentScope := FCurrentScope.FParent;
  end;
end;
procedure TSemanticAnalyzer.EnterWithScope(RecordVar: TExpression);
{Usado para entrar dentro del cuerpo de una senetencia WITH.}
var
  RecordType: TTypeDef;
  Sym: TSymbol;
  NewScope: TScope;
  i: Integer;
  FieldDecl: TVarDecl;
begin
  if RecordVar = nil then
    Exit;

  RecordType := GetTypeOf(RecordVar);
  if RecordType = nil then
    Exit;

  if not (RecordType.NodeType = ntRecordTypeDef) then
    Exit;

  // Crear nuevo ámbito para WITH
  NewScope := TScope.Create(FCurrentScope);
  if FCurrentScope <> nil then
    FCurrentScope.AddChild(NewScope);

  // Registrar los campos del registro como símbolos
  for i := 0 to TRecordTypeDef(RecordType).Fields.Count - 1 do begin
    if TRecordTypeDef(RecordType).Fields[i].NodeType = ntVarDecl then begin
      FieldDecl := TVarDecl(TRecordTypeDef(RecordType).Fields[i]);
      Sym := TSymbol.Create(FieldDecl.Name, skField);
      Sym.DataType := ResolveType(FieldDecl.TypeDef.TypeName);
      Sym.Declaration := FieldDecl;
      NewScope.Declare(Sym);
    end;
  end;

  FWithScope := NewScope;
  FCurrentScope := NewScope;
  FInWith := True;
end;
procedure TSemanticAnalyzer.ExitWithScope;
begin
  if FInWith and (FWithScope <> nil) then begin
    FCurrentScope := FWithScope.FParent;
    FWithScope.Free;
    FWithScope := nil;
    FInWith := False;
  end;
end;
{$endregion}
{$region "Resolución de tipos"}
function TSemanticAnalyzer.ResolveType(const TypeName: string): TTypeDef;
{Resuelve un tipo por su nombre en la tabla de símbolos}
var
  Sym: TSymbol;
begin
  Sym := FCurrentScope.LookupRecursive(UpperCase(TypeName));
  if Sym = nil then    //No existe
    Result := nil
  else if Sym.Kind = skType then  //Es un tipo
    Result := Sym.DataType
  else                 //Es otra cosa
    Result := nil;
end;
function TSemanticAnalyzer.GetFinalTypeDef(TypeDef: TTypeDef): TTypeDef;
{Devuelve el tipo final de una definición de tipo. Funciona de la siguiente forma:
- Para los tipos de sistema (integer, byte, ...) devuelve el mismo tipo.
- Para las definiciones INLINE o estructurados (array of .., o record ... end) devuelve
el mismo tipo.
- En el caso de un tipo alias, debe devolver el tipo final (un tipo de sistema o una
definición INLINE).
}
var
  AliasTypeDef: TAliasTypeDef;
  BaseTypeDef: TTypeDef;
begin
  //Si es un alias, resolver el tipo base
  if TypeDef.NodeType = ntAliasTypeDef then begin
    //Es un alias, hay que resolver el nombre.
    AliasTypeDef := TAliasTypeDef(TypeDef);
    BaseTypeDef := ResolveType(AliasTypeDef.BaseType);
    //Puede que la definición enocntrada no sea fundamental.
    Result := GetFinalTypeDef(BaseTypeDef);
  end else begin
    Result := TypeDef;
  end;
end;
function TSemanticAnalyzer.ResolveTypeDef(TypeDef: TTypeDef): TTypeDef;
{Resuelve los campos "Declaration" y "FinalDef" de "TypeDef", usando la tabla de símbolos,
cuando "TypeDef" es un alias.
Devuelve una referencia al campo "FinalDef". Si no logra resolver el tipo, genera un error
y devuelve NIL.}
var
  SymType: TSymbol;
  AliasTypeDef: TAliasTypeDef;
begin
  if TypeDef.NodeType = ntAliasTypeDef then begin  //Es alias
    AliasTypeDef := TAliasTypeDef(TypeDef);
    //Busca el tipo por nombre
    SymType := FCurrentScope.LookupRecursive(UpperCase(AliasTypeDef.BaseType));
    if (SymType <> nil) and (SymType.Kind = skType) then begin
      //Encontramos la declaración del tipo
      AliasTypeDef.Declaration := TTypeDecl(SymType.Declaration);
      //Resolvemos la definición final del tipo
      AliasTypeDef.FinalDef := GetFinalTypeDef(SymType.DataType);
      Result := AliasTypeDef.FinalDef;
    end else begin
      Error('Tipo desconocido: ' + AliasTypeDef.BaseType, TypeDef.SrcPos);
      Result := Nil;
    end;
  end else begin
    //Debe ser definición INLINE.
    Result := TypeDef;   //La definición es el mismo tipo (arreglo, registro, ...).
  end;
end;
function TSemanticAnalyzer.GetTypeOf(Expr: TExpression): TTypeDef;
{Devuelve el tipo de una expresión.  **** Esta función debe desaparecer ***}
var
  Sym: TSymbol;
  ArrayVarType, RecordVarType: TTypeDef;
  ArrayType: TArrayTypeDef;
  Fields: TASTNodeList;
  FieldAccess: TFieldAccess;
  i: Integer;
  FieldDecl: TVarDecl;
begin
  if Expr = nil then Exit(nil);
  case Expr.NodeType of
    ntNumberLiteral: begin
      Result := Expr.ExpTypeDef;  //La expresión ya debe haber sido visitada
    end;
    ntBooleanLiteral:
      Result := Expr.ExpTypeDef;  //La expresión ya debe haber sido visitada
    ntStringLiteral:
      Result := Expr.ExpTypeDef;  //La expresión ya debe haber sido visitada
    ntVariableRef: begin
      Result := Expr.ExpTypeDef;  //La expresión ya debe haber sido visitada
    end;
    ntBinaryOp: begin
      Result := Expr.ExpTypeDef;  //La expresión ya debe haber sido visitada
    end;
    ntUnaryOp: begin
      Result := Expr.ExpTypeDef;  //La expresión ya debe haber sido visitada
    end;
    ntProcFunctCall: begin
      Result := Expr.ExpTypeDef;  //La expresión ya debe haber sido visitada
    end;
    ntFieldAccess: begin
      Result := Expr.ExpTypeDef;  //La expresión ya debe haber sido visitada
    end;
    ntArrayRef: begin
      Result := Expr.ExpTypeDef;  //La expresión ya debe haber sido visitada
    end;
    ntPointerDeref: begin
      Result := Expr.ExpTypeDef;  //La expresión ya debe haber sido visitada
    end;
    ntPointerLiteral:
      Result := ResolveType('POINTER');
    else
      Result := nil;
  end;
end;
function TSemanticAnalyzer.AreTypesCompatible(T1, T2: TTypeDef): Boolean;
{Analiza la compatibilidad de tipos para asignación o comparación}
var
  Base1, Base2: TTypeDef;
begin
  if (T1 = nil) or (T2 = nil) then Exit(False);
  if T1 = T2 then Exit(True);

  //Obtenemos los tipos base
  Base1 := T1.GetFinalDef;
  if Base1 = Nil then Exit(false);
  if Base1.NodeType = ntSubranTypeDef then Base1 := TSubranTypeDef(Base1).BaseType;

  Base2 := T2.GetFinalDef;
  if Base2 = Nil then Exit(false);
  if Base2.NodeType = ntSubranTypeDef then Base2 := TSubranTypeDef(Base2).BaseType;

  if Base1 = Base2 then Exit(True);

  // === TIPOS SIMPLES ===
  if (Base1.NodeType = ntSimpleTypeDef) and (Base2.NodeType = ntSimpleTypeDef) then begin
    // Numéricos: todos compatibles entre sí
    if IsNumericType(Base1) and IsNumericType(Base2) then
      Exit(True);
    // Strings
    if (Base1.TypeName = 'STRING') and (Base2.TypeName = 'STRING') then
      Exit(True);
    // Booleanos
    if (Base1.TypeName = 'BOOLEAN') and (Base2.TypeName = 'BOOLEAN') then
      Exit(True);
    // Char
    if (Base1.TypeName = 'CHAR') then
      if Base2.TypeName = 'CHAR' then begin
        Exit(True);
      end else if (Base2.TypeName = 'STRING') then begin
        //*** Esto solo es válido si el operando2 es literal de 1 caracter, como 'A'.
        Exit(True);
      end else begin
        Exit(False);
      end;
  end;

  // === ENUMERADOS ===
  if (Base1.NodeType = ntEnumTypeDef) and (Base2.NodeType = ntEnumTypeDef) then
    Exit(Base1 = Base2);

  // === ARREGLOS ===
  //if (Base1.NodeType = ntArrayTypeDef) and (Base2.NodeType = ntArrayTypeDef) then begin
  //  Arr1 := TArrayTypeDef(Base1);
  //  Arr2 := TArrayTypeDef(Base2);
  //  // Compatibles si tienen el mismo número de dimensiones y el mismo tipo de elementos
  //  if (Arr1.IndexRanges.Count = Arr2.IndexRanges.Count) then
  //    Exit(AreTypesCompatible(GetElementType(Arr1), GetElementType(Arr2)));
  //end;

  // === RECORDS ===
  if (Base1.NodeType = ntRecordTypeDef) and (Base2.NodeType = ntRecordTypeDef) then
    Exit(Base1 = Base2);

  // === PUNTEROS ===
  //if (Base1.NodeType = ntPointerTypeDef) and (Base2.NodeType = ntPointerTypeDef) then begin
  //  Ptr1 := TPointerTypeDef(Base1);
  //  Ptr2 := TPointerTypeDef(Base2);
  //  Exit(AreTypesCompatible(GetTargetType(Ptr1), GetTargetType(Ptr2)));
  //end;

  // === SUBRANGOS ===
  if (Base1.NodeType = ntSubranTypeDef) and (Base2.NodeType = ntSubranTypeDef) then
    Exit(True);  // Todos los subrangos son compatibles entre sí

  Result := False;
end;
function TSemanticAnalyzer.IsNumericType(TypeDef: TTypeDef): Boolean;
begin
  if TypeDef = nil then
    Exit(False);

  if TypeDef.NodeType = ntSimpleTypeDef then begin
    Result := (TypeDef.TypeName = 'INTEGER') or
              (TypeDef.TypeName = 'BYTE') or
              (TypeDef.TypeName = 'WORD') or
              (TypeDef.TypeName = 'REAL');
    Exit;
  end;

  if TypeDef.NodeType = ntSubranTypeDef then
    Exit(True);

  Result := False;
end;
function TSemanticAnalyzer.IsOrdinalType(TypeDef: TTypeDef): Boolean;
begin
  if TypeDef = nil then
    Exit(False);

  if TypeDef.NodeType = ntSimpleTypeDef then begin
    Result := (TypeDef.TypeName = 'INTEGER') or
              (TypeDef.TypeName = 'BYTE') or
              (TypeDef.TypeName = 'WORD') or
              (TypeDef.TypeName = 'BOOLEAN') or
              (TypeDef.TypeName = 'CHAR');
    Exit;
  end;

  if TypeDef.NodeType = ntEnumTypeDef then
    Exit(True);

  if TypeDef.NodeType = ntSubranTypeDef then
    Exit(True);

  Result := False;
end;
{$endregion}
{$region "Registro de símbolos"}
procedure TSemanticAnalyzer.RegisterVarDecl(VarDecl: TVarDecl);
var
  Sym: TSymbol;
begin
  // Verificar duplicado
  if FCurrentScope.Lookup(VarDecl.Name) <> nil then begin
    Error('Variable duplicada: ' + VarDecl.Name, VarDecl.SrcPos);
    Exit;
  end;
  //Crea símbolo
  Sym := TSymbol.Create(VarDecl.Name, skVariable);
  Sym.DataType := VarDecl.TypeDef;
  Sym.Declaration := VarDecl;
  FCurrentScope.Declare(Sym);
  //Resuelve tipo. si es un alias.
  ResolveTypeDef(VarDecl.TypeDef);
end;
procedure TSemanticAnalyzer.RegisterConstDecl(ConstDecl: TConstDecl);
var
  Sym: TSymbol;
  TypeDef: TTypeDef;
begin
  // Verificar duplicado
  if FCurrentScope.Lookup(ConstDecl.Name) <> nil then begin
    Error('Constante duplicada: ' + ConstDecl.Name, ConstDecl.SrcPos);
    Exit;
  end;
  // Resolver tipo
  if ConstDecl.HasType then begin
    TypeDef := ConstDecl.TypeDef;
    //Resuelve tipo. si es un alias.
    ResolveTypeDef(ConstDecl.TypeDef);
  end else begin
    // Inferir tipo del valor
    if ConstDecl.Value <> nil then begin
      VisitNode(ConstDecl.Value);   //Valida y resuelve tipo.
      TypeDef := ConstDecl.Value.ExpTypeDef;
    end else begin
      TypeDef := nil;
    end;
  end;
  // Crear símbolo
  Sym := TSymbol.Create(ConstDecl.Name, skConstant);
  Sym.DataType := TypeDef;
  Sym.Declaration := ConstDecl;
  FCurrentScope.Declare(Sym);
end;
procedure TSemanticAnalyzer.RegisterProcDecl(ProcFunctDecl: TProcFunctDecl);
{Registra las declaraciones de procedimientos/funciones, pero sin analizar el cuerpo, aún.}
var
  Sym: TSymbol;
  i: Integer;
  Param: TVarDecl;
begin
  //Verifica si es una declaración duplicada
  Sym := FCurrentScope.Lookup(ProcFunctDecl.Name);
  if Sym <> nil then begin
    if Sym.IsForward and not ProcFunctDecl.IsForward then begin
      //Es la implementación de un FORWARD.
      //Verificamos que los parámetros coincidan.
      if not CompareParameters(Sym, ProcFunctDecl) then begin
        Error('La implementación de ' + ProcFunctDecl.Name +
              ' no coincide con la declaración FORWARD', ProcFunctDecl.SrcPos);
        Exit;
      end;
      //Actualizar el símbolo con la implementación
      Sym.Declaration := ProcFunctDecl;
      Sym.IsForward := False;
      Exit;
    end else begin
      //Duplicado real
      Error('Procedimiento/Función duplicado: ' + ProcFunctDecl.Name, ProcFunctDecl.SrcPos);
      Exit;
    end;
  end;
  //Es un símbolo (proc/función) nuevo. Hay que crearlo.
  if ProcFunctDecl.IsFunction then
    Sym := TSymbol.Create(ProcFunctDecl.Name, skFunction)
  else
    Sym := TSymbol.Create(ProcFunctDecl.Name, skProcedure);
  Sym.Declaration := ProcFunctDecl;
  Sym.IsForward := ProcFunctDecl.IsForward;
  //Registrar parámetros (solo para validación, no se declaran en el ámbito global)
  if ProcFunctDecl.Parameters <> nil then begin
    Sym.Parameters := TASTNodeList.Create(False);   //No debe liberar los nodos parámetro, porque ya los hace el AST.
    for i := 0 to ProcFunctDecl.Parameters.Count - 1 do begin
      Param := TVarDecl(ProcFunctDecl.Parameters[i]);
      Sym.Parameters.Add(Param);
    end;
  end;
  //Tipo de retorno para funciones
  if ProcFunctDecl.IsFunction then begin
    Sym.ReturnType := ProcFunctDecl.ReturnTypeDef;
    //Resuelve si es un alias
    ResolveTypeDef(ProcFunctDecl.ReturnTypeDef);
  end;
  FCurrentScope.Declare(Sym);
end;
procedure TSemanticAnalyzer.RegisterTypeDecl(TypeDecl: TTypeDecl);
{Registra el nombre de un tipo a partir de una declaración de un tipo.
Notar que el registro solo consiste en guardar el nombre y la definición, tal cual se
encuentra, en la tabla de símbolos. El análisis detallado de la definición del tipo de
realizará posteriormente, en la visita a la declaración.}
var
  Sym: TSymbol;
begin
  if TypeDecl.Name = '' then Exit; // Tipo anónimo (inline)
  //Verifica duplicado
  if FCurrentScope.Lookup(TypeDecl.Name) <> nil then begin
    Error('Tipo duplicado: ' + TypeDecl.Name, TypeDecl.SrcPos);
    Exit;
  end;
  //Registra el nombre del tipo
  Sym := TSymbol.Create(TypeDecl.Name, skType);
  Sym.DataType := TypeDecl.Definition;
  Sym.Declaration := TypeDecl;
  FCurrentScope.Declare(Sym);
end;
procedure TSemanticAnalyzer.RegisterDeclarations(Decls: TASTNodeList);
var
  i: Integer;
  Node: TASTNode;
begin
  if Decls = nil then Exit;
  for i := 0 to Decls.Count - 1 do begin
    Node := Decls[i];
    case Node.NodeType of
      ntVarDecl:
        RegisterVarDecl(TVarDecl(Node));
      ntConstDecl:
        RegisterConstDecl(TConstDecl(Node));
      ntProcFunctDecl:
        RegisterProcDecl(TProcFunctDecl(Node));
      ntTypeDecl:
        RegisterTypeDecl(TTypeDecl(Node));
    end;
  end;
end;
{$endregion}
{$region "Visitantes de declaraciones"}
procedure TSemanticAnalyzer.VisitVarDecl(VarDecl: TVarDecl);
{Visita a una variable para validar la asignación inicial y el caso ABSOLUTE.}
var
  InitType, VarType: TTypeDef;
begin
  // Ya fue registrada en RegisterVarDecl
  // Verificar inicialización
  if VarDecl.initVal <> nil then begin
    InitType := GetTypeOf(VarDecl.initVal);
    VarType := VarDecl.TypeDef;
    if not AreTypesCompatible(VarType, InitType) then
      Error('Tipo de inicialización incompatible para: ' + VarDecl.Name, VarDecl.SrcPos);
  end;
  // Verificar ABSOLUTE
  if VarDecl.hasAdic = DEC_ABSOL then begin
    if VarDecl.absAddr = nil then
      Error('Dirección ABSOLUTE no especificada', VarDecl.SrcPos);
  end;
end;
procedure TSemanticAnalyzer.VisitConstDecl(ConstDecl: TConstDecl);
begin
  // Ya fue registrada en RegisterConstDecl
  // Verificar que el valor sea constante
  if ConstDecl.Value <> nil then begin
    // Verificar que no haya referencias a variables
    // (implementación simplificada)
  end;
end;
procedure TSemanticAnalyzer.VisitProcFunctDecl(ProcFunctDecl: TProcFunctDecl);
var
  i: Integer;
  Param: TVarDecl;
  Sym, SelfSym, FieldSym: TSymbol;
  OldProcedure: TProcFunctDecl;
  Field: TASTNode;
begin
  if ProcFunctDecl.IsForward then Exit;
  OldProcedure := FCurrentProcedure;
  FCurrentProcedure := ProcFunctDecl;
  EnterScope;
  try
    //Registrar los parámetros en el ámbito local.
    if ProcFunctDecl.Parameters <> nil then begin
      for i := 0 to ProcFunctDecl.Parameters.Count - 1 do begin
        Param := TVarDecl(ProcFunctDecl.Parameters[i]);
        //Registra el parámetro
        Sym := TSymbol.Create(Param.Name, skParameter);
        Sym.DataType := Param.TypeDef;
        Sym.Declaration := Param;
        FCurrentScope.Declare(Sym);
        //Visita para validar y resolver los tipos.
        VisitNode(Param.TypeDef);
      end;
    end;
    //Validamos si estamos en un método
    if ProcFunctDecl.IsMethod and (ProcFunctDecl.RecordType <> nil) then begin
      //Es un método de RECORD. Registramos "Self".
      SelfSym := TSymbol.Create('SELF', skParameter);
      SelfSym.DataType := ProcFunctDecl.RecordType;
      SelfSym.IsSelf := True;
      FCurrentScope.Declare(SelfSym);
      //Registramos los campos del record como accesibles
      for Field in ProcFunctDecl.RecordType.Fields do begin
        if Field is TVarDecl then begin
          FieldSym := TSymbol.Create(TVarDecl(Field).Name, skField);
          FieldSym.DataType := ResolveType(TVarDecl(Field).TypeDef.TypeName);
          FieldSym.Declaration := Field;
          FCurrentScope.Declare(FieldSym);
        end;
      end;
    end;
//    //Analiza el tipo de retorno
//    if ProcFunctDecl.ReturnTypeDef <> Nil then begin
//      VisitNode(ProcFunctDecl.ReturnTypeDef);
//    end;
    //Analiza las declaraciones locales.
    if ProcFunctDecl.Declarations <> nil then begin
      RegisterDeclarations(ProcFunctDecl.Declarations);
    end;
    //Analiza el cuerpo
    if ProcFunctDecl.Body <> nil then begin
      VisitBlock(ProcFunctDecl.Body);
    end;
  finally
    ExitScope;
    FCurrentProcedure := OldProcedure;
  end;
end;
procedure TSemanticAnalyzer.VisitTypeDecl(TypeDecl: TTypeDecl);
{Visitante para las declaraciones de tipos. Lo que nos interesa aquí, es visitar las
definiciones de los tipos, porque la valiación de duplicidad en los nombres, ya la hizo
RegisterTypeDecl().}
begin
  //Definiciones de Tipos
  VisitNode(TypeDecl.Definition);
end;
//Definiciones de tipos
procedure TSemanticAnalyzer.VisitAliasTypeDef(TypeDef: TTypeDef);
{Visita declaraciones de tipos sismples.}
begin
  //El tipo ya fue registrado en la tabla de símbolos. Solo falta resolver la definición
  //para los casos alias.
  ResolveTypeDef(TypeDef);
end;
procedure TSemanticAnalyzer.VisitArrayTypeDef(ArrayType: TArrayTypeDef);
var
  ElemTypeDef: TTypeDef;
  i: Integer;
  Range: TArrayRange;
begin
  //Verifica tipo de elementos
  ElemTypeDef := ArrayType.ElemTypeDef;
  //Visita para validar y resolver los tipos alias directos o anidados.
  VisitNode(ElemTypeDef);

  //Verifica rangos de índices
  for i := 0 to ArrayType.IndexRanges.Count - 1 do begin
    Range := ArrayType.IndexRanges[i];
    if Range.LowExpr <> nil then
      VisitNode(Range.LowExpr);
    if Range.HighExpr <> nil then
      VisitNode(Range.HighExpr);
  end;
end;
procedure TSemanticAnalyzer.VisitRecordTypeDef(RecordType: TRecordTypeDef);
var
  i, j: Integer;
  Branch: TVariantBranch;
  astNode: TASTNode;
  Field : TVarDecl;
begin
  //Analiza los campos del registro
  for astNode in RecordType.Fields do begin
    if astNode.NodeType = ntVarDecl then begin
      Field := TVarDecl(astNode);
      //Visita para validar y resolver los tipos alias directos o anidados.
      VisitNode(Field.TypeDef);
    end;
  end;
  //Analiza variantes
  if RecordType.Branches <> nil then begin
    for i := 0 to RecordType.Branches.Count - 1 do begin
      Branch := RecordType.Branches[i];
      // Analizar selectores
      for j := 0 to Branch.SelectorValues.Count - 1 do begin
        VisitNode(Branch.SelectorValues[j]);
      end;
      // Analizar campos
      for astNode in Branch.Fields do begin
        if astNode.NodeType = ntVarDecl then begin
          Field := TVarDecl(astNode);
          //Visita para validar y resolver los tipos alias directos o anidados.
          VisitNode(Field.TypeDef);
        end;
      end;
    end;
  end;
end;
procedure TSemanticAnalyzer.VisitEnumTypeDef(EnumType: TEnumTypeDef);
var
  i: Integer;
  ValueName: String;
  Sym: TSymbol;
begin
  // Verificar valores duplicados
//  for i := 0 to EnumType.Values.Count - 1 do begin
//    for j := i + 1 to EnumType.Values.Count - 1 do
//    begin
//      if EnumType.Values[i] = EnumType.Values[j] then
//        Error('Valor de enumerado duplicado: ' + EnumType.Values[i], EnumType.SrcPos);
//    end;
//  end;
  //Registrar cada valor del enumerado como símbolo.
  for i := 0 to EnumType.Values.Count - 1 do begin
    ValueName := EnumType.Values[i];
    // Verificar duplicado
    if FCurrentScope.Lookup(ValueName) <> nil then begin
      Error('Valor de enumerado duplicado: ' + ValueName, EnumType.SrcPos)
    end else begin
      // Crear símbolo para el valor del enumerado
      Sym := TSymbol.Create(ValueName, skEnumValue);
      Sym.DataType := EnumType;  // El tipo del valor es el enumerado mismo
      Sym.Declaration := EnumType;
      Sym.IsDataTypeOwner := False;  // No es propietario del tipo
      FCurrentScope.Declare(Sym);
    end;
  end;
end;
procedure TSemanticAnalyzer.VisitSubrangeTypeDef(SubrangeType: TSubranTypeDef);
var
  LowType, HighType: TTypeDef;
begin
  //Analizar límites
  VisitNode(SubrangeType.LowExpr);
  VisitNode(SubrangeType.HighExpr);
  //Detectar el tipo base del subrango
  if (SubrangeType.LowExpr.NodeType = ntNumberLiteral) and
     (SubrangeType.HighExpr.NodeType = ntNumberLiteral) then begin
    //Los límites son números, el tipo base es INTEGER.
    SubrangeType.BaseType := ResolveType('INTEGER');
    SubrangeType.BaseTypeName := 'INTEGER';
  end else if (SubrangeType.LowExpr.NodeType = ntStringLiteral) and
              (SubrangeType.HighExpr.NodeType = ntStringLiteral) then begin
    //Los límites son caracteres ('a'..'z'), el tipo base es CHAR.
    SubrangeType.BaseType := ResolveType('CHAR');
    SubrangeType.BaseTypeName := 'CHAR';
  end else begin
    //Valida consistencia de tipos del rango
    LowType := GetTypeOf(SubrangeType.LowExpr);
    if LowType = nil then begin
      Error('No se puede determinar el tipo de límite inferior', SubrangeType.SrcPos);
      Exit;
    end;
    HighType := GetTypeOf(SubrangeType.HighExpr);
    if HighType = nil then begin
      Error('No se puede determinar el tipo del límite superior', SubrangeType.SrcPos);
      Exit;
    end;
    if LowType <> HighType then begin
      Error('Los límites del subrango deben ser del mismo tipo', SubrangeType.SrcPos);
      Exit;
    end;
    //Verifica que el tipo sea ordinal (integer, char, enum, etc.)
    if not IsOrdinalType(LowType) then begin
      Error('El tipo del subrango debe ser ordinal',
            SubrangeType.SrcPos);
      Exit;
    end;
    //Se usa el mismo tipo de los límites para el tipo base
    SubrangeType.BaseType := LowType;
    SubrangeType.BaseTypeName := LowType.TypeName;
  end;
  //Valida el orden
  if CompareExpressions(SubrangeType.LowExpr, SubrangeType.HighExpr) > 0 then begin
    Error('El límite inferior debe ser menor o igual al límite superior',
          SubrangeType.SrcPos);
  end
end;
procedure TSemanticAnalyzer.VisitPointerTypeDef(PointerType: TPointerTypeDef);
var
  TargetType: TTypeDef;
begin
  //Visita para validar y resolver los tipos alias directos o anidados.
  VisitNode(PointerType.TargetTypeDef);
end;
{$endregion}
{$region "Visitantes de sentencias"}
procedure TSemanticAnalyzer.VisitAssignment(Assign: TAssignment);
var
  TargetType, ValueType: TTypeDef;
begin
  //Analiza destino
  VisitNode(Assign.Target);
  TargetType := Assign.Target.ExpTypeDef;

  //Analiza valor
  VisitNode(Assign.Value);
  ValueType := Assign.Value.ExpTypeDef;

  // Verificar compatibilidad
  if TargetType = nil then
    Error('No se puede determinar el tipo del destino', Assign.Target.SrcPos)
  else if ValueType = nil then
    Error('No se puede determinar el tipo del valor', Assign.Value.SrcPos)
  else if not AreTypesCompatible(TargetType, ValueType) then
    Error('Tipos incompatibles en la asignación', Assign.SrcPos);
end;
procedure TSemanticAnalyzer.VisitIfStatement(IfStmt: TIfStatement);
var
  CondType: TTypeDef;
begin
  // Analizar condición
  VisitNode(IfStmt.Condition);
  CondType := GetTypeOf(IfStmt.Condition);
  if CondType = nil then
    Error('No se puede determinar el tipo de la condición', IfStmt.Condition.SrcPos)
  else if (CondType.TypeName <> 'BOOLEAN') and (not (CondType.NodeType = ntSimpleTypeDef)) then
    Warning('La condición debería ser booleana', IfStmt.Condition.SrcPos);

  // Analizar ramas
  if IfStmt.ThenBranch <> nil then
    VisitBlock(IfStmt.ThenBranch);
  if IfStmt.ElseBranch <> nil then
    VisitBlock(IfStmt.ElseBranch);
end;
procedure TSemanticAnalyzer.VisitWhileLoop(WhileLoop: TWhileLoop);
begin
  VisitNode(WhileLoop.Condition);
  if WhileLoop.Body <> nil then
    VisitBlock(WhileLoop.Body);
end;
procedure TSemanticAnalyzer.VisitRepeatUntil(RepeatUntil: TRepeatUntil);
begin
  if RepeatUntil.Body <> nil then
    VisitBlock(RepeatUntil.Body);
  VisitNode(RepeatUntil.Condition);
end;
procedure TSemanticAnalyzer.VisitForLoop(ForLoop: TForLoop);
var
  ControlType: TTypeDef;
  Sym: TSymbol;
begin
  //Verificar la variable de control
  if ForLoop.ControlVar <> nil then begin
    Sym := FCurrentScope.LookupRecursive(ForLoop.ControlVar.Name);
    if Sym = nil then begin
      Error('Variable de control no declarada: ' + ForLoop.ControlVar.Name, ForLoop.ControlVar.SrcPos)
    end else begin
      ControlType := Sym.DataType;
      if not IsOrdinalType(ControlType.GetFinalDef) then
        Error('La variable de control debe ser de tipo ordinal', ForLoop.ControlVar.SrcPos);
    end;
  end;

  //Analiza expresiones
  VisitNode(ForLoop.StartExpr);
  VisitNode(ForLoop.EndExpr);

  //Analiza cuerpo
  if ForLoop.Body <> nil then
    VisitBlock(ForLoop.Body);
end;
procedure TSemanticAnalyzer.VisitCaseStatement(CaseStmt: TCaseStatement);
var
  i: Integer;
  SelectorType: TTypeDef;
begin
  // Analizar selector
  VisitNode(CaseStmt.Selector);
  SelectorType := GetTypeOf(CaseStmt.Selector);
  if SelectorType = nil then
    Error('No se puede determinar el tipo del selector', CaseStmt.Selector.SrcPos);

  // Analizar ramas
  for i := 0 to CaseStmt.Branches.Count - 1 do
    VisitCaseBranch(CaseStmt.Branches[i]);

  // Analizar ELSE
  if CaseStmt.ElseBranch <> nil then
    VisitBlock(CaseStmt.ElseBranch);
end;
procedure TSemanticAnalyzer.VisitCaseBranch(CaseBranch: TCaseBranch);
var
  i: Integer;
begin
  // Analizar constantes
  for i := 0 to CaseBranch.Constants.Count - 1 do
    VisitNode(CaseBranch.Constants[i]);

  // Analizar instrucción
  if CaseBranch.Statement <> nil then
    VisitBlock(CaseBranch.Statement);
end;
procedure TSemanticAnalyzer.VisitWithStatement(WithStmt: TWithStatement);
var
  RecordType: TTypeDef;
begin
  // Analizar la expresión del WITH
  VisitNode(WithStmt.RecordVar);
  RecordType := GetTypeOf(WithStmt.RecordVar);

  // Verificar que sea un registro
  if RecordType = nil then
    Error('WITH requiere una expresión de tipo RECORD', WithStmt.RecordVar.SrcPos)
  else if not (RecordType.NodeType = ntRecordTypeDef) then
    Error('WITH solo puede usarse con RECORDs', WithStmt.RecordVar.SrcPos);

  // Entrar en el ámbito del WITH
  EnterWithScope(WithStmt.RecordVar);
  try
    // Analizar el cuerpo
    if WithStmt.Body <> nil then
      VisitBlock(WithStmt.Body);
  finally
    ExitWithScope;
  end;
end;
procedure TSemanticAnalyzer.VisitExitStatement(ExitStmt: TExitStatement);
var
  ReturnType: TTypeDef;
  ValueType: TTypeDef;
  Func: TProcFunctDecl;
begin
  if ExitStmt.HasReturnValue then begin
    //Verificamos que estamos en una función
    if not IsInFunction then begin
      Error('EXIT con valor solo permitido en funciones', ExitStmt.SrcPos);
      Exit;
    end;
    //Verificamos compatibilidad del valor de retorno
    VisitNode(ExitStmt.ReturnValue);
    ValueType := GetTypeOf(ExitStmt.ReturnValue);
    Func := GetCurrentFunction;
    if Func <> nil then begin
      ReturnType := Func.ReturnTypeDef;
      if ReturnType = nil then
        Error('No se puede determinar el tipo de retorno de la función', ExitStmt.SrcPos)
      else if not AreTypesCompatible(ReturnType, ValueType) then
        Error('Tipo de retorno incompatible en EXIT', ExitStmt.SrcPos);
    end;
  end
  else
  begin
    // EXIT sin valor: permitido en procedimientos y funciones
    // En funciones, genera advertencia
    if IsInFunction then
      Warning('EXIT sin valor en función', ExitStmt.SrcPos);
  end;
end;
{$endregion}
{$region "Visitantes de expresiones"}
procedure TSemanticAnalyzer.VisitVariableRef(VarRef: TVariableRef);
var
  Sym: TSymbol;
begin
  //Busca el nombre de la variable.
  Sym := FCurrentScope.LookupRecursive(VarRef.Name);
  if Sym = nil then begin
    Error('Variable no declarada: ' + VarRef.Name, VarRef.SrcPos);
    Exit;
  end;
  //Enlaza a la declaración.
  VarRef.Declaration := TVarDecl(Sym.Declaration);
  //Actualiza el tipo de la expresión
  if Sym.FKind = skFunction then begin
    //Era una función
    VarRef.ExpTypeDef := Sym.ReturnType;
  end else begin
    //Debería ser una variable
    VarRef.ExpTypeDef := Sym.DataType;
  end;
end;
procedure TSemanticAnalyzer.VisitNumberLiteral(NumLit: TNumberLiteral);
begin
  //Nada que verificar, los literales son siempre correctos.
  //Solo resolvemos el tipo
  if NumLit.IsInteger then
    NumLit.ExpTypeDef := ResolveType('INTEGER')
  else
    NumLit.ExpTypeDef := ResolveType('REAL');
end;
procedure TSemanticAnalyzer.VisitBooleanLiteral(BoolLit: TBooleanLiteral);
begin
  //Nada que verificar.
  BoolLit.ExpTypeDef := ResolveType('BOOLEAN')
end;
procedure TSemanticAnalyzer.VisitStringLiteral(StrLit: TStringLiteral);
begin
  //Nada que verificar
  StrLit.ExpTypeDef := ResolveType('STRING')
end;
procedure TSemanticAnalyzer.VisitBinaryOp(BinOp: TBinaryOp);
var
  LeftType, RightType: TTypeDef;
begin
  // Analizar operandos
  VisitNode(BinOp.Left);
  VisitNode(BinOp.Right);
  //Obtiene los tipos de los operandos
  LeftType := BinOp.Left.ExpTypeDef;
  if LeftType = Nil then begin
    //Error('No se puede determinar el tipo del operando', BinOp.Left.SrcPos);  ***Para no saturar de mensajes
    Exit;
  end;
  RightType := BinOp.Right.ExpTypeDef;
  if RightType = Nil then begin
    //Error('No se puede determinar el tipo del operando', BinOp.Right.SrcPos);
    Exit;
  end;
  //Lee las definiciones finales de tipos
  LeftType := LeftType.GetFinalDef;
  RightType := RightType.GetFinalDef;
  //Verifica las operaciones
  case BinOp.Op of
    '+', '-', '*', '/', 'div', 'mod': begin
      if not IsNumericType(LeftType) then
        Warning('Operador aritmético con tipo no numérico', BinOp.Left.SrcPos);
      if not IsNumericType(RightType) then
        Warning('Operador aritmético con tipo no numérico', BinOp.Right.SrcPos);
      //Asignamos tipo de la expresión.
      {*** Por ahora asumimos el Tipo INTEGER pero el análisis debe ser más complicado.
      Se ha planeado asignar métodos (con operadores) a los tipos básicos, de modo que una
      oepración como "a + b", se traduzca a "a._add(b)". }
      BinOp.ExpTypeDef := ResolveType('INTEGER');
    end;
    'and', 'or', 'not': begin
      // Verificar tipos booleanos
      BinOp.ExpTypeDef := ResolveType('BOOLEAN');
    end;
    '=', '<>', '<', '>', '<=', '>=': begin
      // Verificar compatibilidad
      if (LeftType <> nil) and (RightType <> nil) then begin
        if not AreTypesCompatible(LeftType, RightType) then
          Warning('Comparación de tipos incompatibles', BinOp.SrcPos);
      end;
      BinOp.ExpTypeDef := ResolveType('BOOLEAN');
    end;
  end;
end;
procedure TSemanticAnalyzer.VisitUnaryOp(UnaryOp: TUnaryOp);
var
  OpType: TTypeDef;
begin
  VisitNode(UnaryOp.Operand);
  OpType := GetTypeOf(UnaryOp.Operand);
  case UnaryOp.Op of
    '+', '-': begin
      if not IsNumericType(OpType) then
        Warning('Operador unario con tipo no numérico', UnaryOp.Operand.SrcPos);
      UnaryOp.ExpTypeDef := ResolveType('INTEGER');
    end;
    'not': begin
      if (OpType <> nil) and (OpType.TypeName <> 'BOOLEAN') then
        Warning('NOT aplicado a tipo no booleano', UnaryOp.Operand.SrcPos);
      UnaryOp.ExpTypeDef := ResolveType('BOOLEAN');
    end;
  end;
end;
procedure TSemanticAnalyzer.VisitFunctionCall(ProcFuncCall: TProcFunctCall);
{Visita la llamada a un procedimiento o función, que se ha identificado como tal en el
análisis sintáctico.}
var
  Sym: TSymbol;
  i: Integer;
  ArgType: TTypeDef;
  Param: TVarDecl;
  Parent: TASTNode;
begin
  //Buscar el procedimiento/función.
  Sym := FCurrentScope.LookupRecursive(ProcFuncCall.Name);
  if Sym = nil then begin
    Error('Identificador no declarado: ' + ProcFuncCall.Name, ProcFuncCall.SrcPos);
    Exit;
  end;
  if (Sym.Kind <> skFunction) and (Sym.Kind <> skProcedure) then begin
    Error(ProcFuncCall.Name + ' no es una función o procedimiento', ProcFuncCall.SrcPos);
    Exit;
  end;
  //Enlaza referencia a la declaración, si existe
  if not Sym.IsIntrinsic then begin
    //Debe haber declaración
    if Sym.Declaration.NodeType = ntProcFunctDecl then begin
      //Su declaración figura como procedimiento o función
      ProcFuncCall.Declaration := TProcFunctDecl(Sym.Declaration); //Enlaza a declaración
    end else begin  //Figura como otra cosa
      Error('Declaración inválida para: ' + ProcFuncCall.Name, ProcFuncCall.SrcPos);
      Exit;
    end;
  end;
  //Completa atributos
  ProcFuncCall.IsProcedure := (Sym.Kind = skProcedure);   //Aquí se puede saber si es proc. o función.
  ProcFuncCall.IsIntrinsic := Sym.IsIntrinsic;            //Y si es del sistema.
  //Verifica argumentos
  if Sym.IsIntrinsic then begin
    //Verificación flexible: aceptan cualquier número de argumentos
    //y cualquier tipo (dentro de lo razonable)
    for i := 0 to ProcFuncCall.Arguments.Count - 1 do begin
      VisitNode(ProcFuncCall.Arguments[i]);
      // No verificamos tipos estrictos
    end;
  end else begin
    //Proc./Funciones normales
    if Sym.Parameters <> nil then begin
      if ProcFuncCall.Arguments.Count <> Sym.Parameters.Count then
        Error('Número incorrecto de argumentos para ' + ProcFuncCall.Name + ' (esperaba ' +
              IntToStr(Sym.Parameters.Count) + ', tiene ' +
              IntToStr(ProcFuncCall.Arguments.Count) + ')', ProcFuncCall.SrcPos);

      //Verifica tipos de argumentos
      for i := 0 to Min(ProcFuncCall.Arguments.Count, Sym.Parameters.Count) - 1 do begin
        VisitNode(ProcFuncCall.Arguments[i]);
        ArgType := GetTypeOf(ProcFuncCall.Arguments[i]);
        Param := TVarDecl(Sym.Parameters[i]);
        ResolveTypeDef(Param.TypeDef);   //*** ¿Es necesario? ¿No sería VisitNode()?
        if not AreTypesCompatible(Param.TypeDef, ArgType) then begin
          Error('Tipo de argumento incompatible para parámetro ' + IntToStr(i+1) + ' de ' +
                ProcFuncCall.Name, ProcFuncCall.Arguments[i].SrcPos);
        end;
      end;
    end else begin
      //Sin parámetros declarados, verifica que no haya argumentos
      if ProcFuncCall.Arguments.Count > 0 then
        Error(ProcFuncCall.Name + ' no acepta argumentos', ProcFuncCall.SrcPos);
    end;
  end;
  //Si es procedimiento, verificar que se use como sentencia
  if ProcFuncCall.IsProcedure then begin
    //Verificar contexto: ¿está en una sentencia o en una expresión?
    Parent := ProcFuncCall.Parent;
    if Parent = Nil then
      //No se identifica al padre
    else if Parent.NodeType = ntBlock then
      // OK: está en una sentencia
    else begin
      Error('El procedimiento ' + ProcFuncCall.Name + ' no puede usarse como expresión',
            ProcFuncCall.SrcPos);
    end;
  end else begin
    //Es función. Debe devolver un tipo.
    ProcFuncCall.ExpTypeDef := Sym.ReturnType;
  end;
end;
procedure TSemanticAnalyzer.VisitFieldAccess(FieldAccess: TFieldAccess);
var
  RecordVarType: TTypeDef;
  FoundField: Boolean;
  i: Integer;
  FieldDecl: TVarDecl;
  RecordTypeDef: TRecordTypeDef;
  Field: TASTNode;
begin
  //Analiza la variable registro y resuelve tipos
  VisitNode(FieldAccess.RecordVar);
  RecordVarType := FieldAccess.RecordVar.ExpTypeDef;
  if RecordVarType = nil then begin
    Error('No se puede determinar el tipo del registro', FieldAccess.RecordVar.SrcPos);
    Exit;
  end;
  //"RecordVarType" puede ser un alias (como "MiRecord") en lugar del RECORD.
  RecordVarType := RecordVarType.GetFinalDef;   //La definición final, debe ser un RECORD.
  if RecordVarType = Nil then begin
    //No se ha encontrado la definición del RECORD.
    //Error('No se tiene información del registro.', FieldAccess.SrcPos);  *** Para no saturar de mensajes
    Exit;
  end;
  //Busca el campo en el registro
  FoundField := False;
  if RecordVarType.NodeType = ntRecordTypeDef then begin
    RecordTypeDef := TRecordTypeDef(RecordVarType);
    for Field in RecordTypeDef.Fields do begin
      if Field.NodeType = ntVarDecl then begin
        FieldDecl := TVarDecl(Field);
        if CompareText(FieldDecl.Name, FieldAccess.FieldName)=0 then begin
          FieldAccess.ExpTypeDef := FieldDecl.TypeDef;  //Actualiza el tipo
          FoundField := True;
          Break;
        end;
      end;
    end;
  end;
  if not FoundField then begin
    Error('Campo no encontrado en el RECORD: ' + FieldAccess.FieldName, FieldAccess.SrcPos);
  end;
end;
procedure TSemanticAnalyzer.VisitPointerDeref(PointerDeref: TPointerDeref);
var
  PtrType: TTypeDef;
  PointerTypeDef: TPointerTypeDef;
begin
  VisitNode(PointerDeref.Pointer);
  PtrType := PointerDeref.Pointer.ExpTypeDef;
  if PtrType <> nil then begin
    PtrType := PtrType.GetFinalDef;   //Por si es un alias a puntero
    if not (PtrType.NodeType = ntPointerTypeDef) then begin
      Error('^ solo puede aplicarse a punteros', PointerDeref.SrcPos);
    end;
    PointerTypeDef := TPointerTypeDef(PtrType);
    PointerDeref.ExpTypeDef := PointerTypeDef.TargetTypeDef;
  end;
end;
procedure TSemanticAnalyzer.VisitArrayRef(ArrayRef: TArrayRef);
var
  ArrayVarType, IdxType: TTypeDef;
  i: Integer;
  index: TExpression;
begin
  //Analiza el nodo de la variable arreglo.
  VisitNode(ArrayRef.ArrayVar);
  //Determina el tipo del arreglo base
  ArrayVarType := GetTypeOf(ArrayRef.ArrayVar);
  if ArrayVarType = Nil then begin
    Error('No se puede determinar el tipo del arreglo', ArrayRef.ArrayVar.SrcPos);
    Exit;
  end;
  ArrayVarType := ArrayVarType.GetFinalDef;   //Por si es un alias del arreglo.
  if ArrayVarType = Nil then begin
    Error('No se puede determinar el tipo del arreglo', ArrayRef.ArrayVar.SrcPos);
    Exit;
  end;
  //Verificar que sea un arreglo
  if ArrayVarType.NodeType <> ntArrayTypeDef then begin
    Error('[] solo puede aplicarse a arreglos', ArrayRef.SrcPos);
    Exit;
  end;
  //Actualiza el tipo de la expresión, para que no tenga que buscarse de nuevo.
  ArrayRef.ExpTypeDef := TArrayTypeDef(ArrayVarType).ElemTypeDef;
  //Verifica número de índices
  if ArrayRef.Indices.Count <> TArrayTypeDef(ArrayVarType).IndexRanges.Count then
    Error('Número incorrecto de índices para el arreglo (esperaba ' +
          IntToStr(TArrayTypeDef(ArrayVarType).IndexRanges.Count) + ', tiene ' +
          IntToStr(ArrayRef.Indices.Count) + ')', ArrayRef.SrcPos);
  //Analiza índices
  for index in ArrayRef.Indices do begin
    VisitNode(index);
    IdxType := index.ExpTypeDef;
    if IdxType = Nil then begin
      Error('No se puede determinar el tipo del índice.', ArrayRef.SrcPos);
      Continue;
    end;
    if not IsOrdinalType(IdxType.GetFinalDef) then begin
      Warning('El índice debe ser de tipo ordinal', index.SrcPos);
    end;
  end;
end;
procedure TSemanticAnalyzer.VisitArrayLiteral(ArrayLit: TArrayLiteral);
var
  i: Integer;
begin
  for i := 0 to ArrayLit.Values.Count - 1 do
    VisitNode(ArrayLit.Values[i]);
end;
procedure TSemanticAnalyzer.VisitRecordLiteral(RecordLit: TRecordLiteral);
var
  i: Integer;
  Init: TFieldInitializer;
begin
  for i := 0 to RecordLit.FieldInitializers.Count - 1 do
  begin
    Init := RecordLit.FieldInitializers[i];
    VisitNode(Init.Value);
  end;
end;
procedure TSemanticAnalyzer.VisitPointerLiteral(PointerLit: TPointerLiteral);
begin
  // nil es válido para cualquier puntero
  // Las direcciones literales se verifican en el contexto
end;
{$endregion}
{$region "Visitantes de nodos principales}
procedure TSemanticAnalyzer.VisitNode(Node: TASTNode);
begin
  if Node = nil then Exit;

  case Node.NodeType of
    //Programas y unidades
    ntProgram       : VisitProgram(TProgram(Node));
    ntUnit          : VisitUnit(TUnit(Node));
    //Bloques y declaraciones
    ntBlock         : VisitBlock(TBlock(Node));
    ntVarDecl       : VisitVarDecl(TVarDecl(Node));
    ntConstDecl     : VisitConstDecl(TConstDecl(Node));
    ntProcFunctDecl : VisitProcFunctDecl(TProcFunctDecl(Node));
    ntTypeDecl      : VisitTypeDecl(TTypeDecl(Node));
    //Definiciones de tipos
    ntSimpleTypeDef : ;
    ntSubranTypeDef : VisitSubrangeTypeDef(TSubranTypeDef(Node));
    ntEnumTypeDef   : VisitEnumTypeDef(TEnumTypeDef(Node));
    ntArrayTypeDef  : VisitArrayTypeDef(TArrayTypeDef(Node));
    ntRecordTypeDef : VisitRecordTypeDef(TRecordTypeDef(Node));
    ntPointerTypeDef: VisitPointerTypeDef(TPointerTypeDef(Node));
    ntAliasTypeDef  : VisitAliasTypeDef(TTypeDef(Node));
    ntProcedTypeDef : ;  //*** Falta implementar
    //Sentencias
    ntAssignment    : VisitAssignment(TAssignment(Node));
    ntIfStatement   : VisitIfStatement(TIfStatement(Node));
    ntWhileLoop     : VisitWhileLoop(TWhileLoop(Node));
    ntRepeatUntil   : VisitRepeatUntil(TRepeatUntil(Node));
    ntForLoop       : VisitForLoop(TForLoop(Node));
    ntCaseStatement : VisitCaseStatement(TCaseStatement(Node));
    ntCaseBranch    : VisitCaseBranch(TCaseBranch(Node));
    ntWithStatement : VisitWithStatement(TWithStatement(Node));
    ntExitStatement : VisitExitStatement(TExitStatement(Node));
    //Expresiones
    ntVariableRef   : VisitVariableRef(TVariableRef(Node));
    ntNumberLiteral : VisitNumberLiteral(TNumberLiteral(Node));
    ntBooleanLiteral: VisitBooleanLiteral(TBooleanLiteral(Node));
    ntStringLiteral : VisitStringLiteral(TStringLiteral(Node));
    ntBinaryOp      : VisitBinaryOp(TBinaryOp(Node));
    ntUnaryOp       : VisitUnaryOp(TUnaryOp(Node));
    ntProcFunctCall : VisitFunctionCall(TProcFunctCall(Node));
    ntFieldAccess   : VisitFieldAccess(TFieldAccess(Node));
    ntPointerDeref  : VisitPointerDeref(TPointerDeref(Node));
    ntArrayRef      : VisitArrayRef(TArrayRef(Node));
    ntArrayLiteral  : VisitArrayLiteral(TArrayLiteral(Node));
    ntRecordLiteral : VisitRecordLiteral(TRecordLiteral(Node));
    ntPointerLiteral: VisitPointerLiteral(TPointerLiteral(Node));
  end;
end;
procedure TSemanticAnalyzer.VisitBlock(Block: TBlock);
var
  i: Integer;
begin
  if Block = nil then Exit;
  EnterScope;
  try
    for i := 0 to Block.Statements.Count - 1 do
      VisitNode(Block.Statements[i]);
  finally
    ExitScope;
  end;
end;
procedure TSemanticAnalyzer.VisitProgram(Prog: TProgram);
var
  Node: TASTNode;
begin
  if Prog = nil then Exit;
  //Registra primero las declaraciones globales
  RegisterDeclarations(Prog.Declarations);

  {Analizar primero las declaraciones de tipos y variables para registrar sus símbolos
  (como los valores de los enumerados).}
  for Node in Prog.Declarations do begin
    if Node.NodeType in [ntTypeDecl, ntVarDecl] then begin
        VisitNode(TTypeDecl(Node).Definition);
    end;
  end;

  //Analizar los cuerpos de los procedimientos/funciones
  for Node in Prog.Declarations do begin
    if Node.NodeType = ntProcFunctDecl then begin
      if not TProcFunctDecl(Node).IsForward then
        VisitProcFunctDecl(TProcFunctDecl(Node));
    end;
  end;
  CheckForwardDeclarations;
  //Analiza el cuerpo principal
  VisitBlock(Prog.Body);
end;
procedure TSemanticAnalyzer.VisitUnit(Unit0: TUnit);
var
  Node: TASTNode;
begin
  if Unit0 = nil then Exit;
  FCurrentUnit := Unit0;
  //Registrar declaraciones de interface
  RegisterDeclarations(Unit0.InterfaceDecls);
  //Registrar declaraciones de implementation
  RegisterDeclarations(Unit0.ImplementationDecls);
  //Analizar cuerpos de procedimientos/funciones en IMPLEMENTATION
  for Node in Unit0.ImplementationDecls do begin
    if Node.NodeType = ntProcFunctDecl then begin
      if not TProcFunctDecl(Node).IsForward then
        VisitProcFunctDecl(TProcFunctDecl(Node));
    end;
  end;
  // Analizar initialization y finalization
  if Unit0.InitializationBlock <> nil then
    VisitBlock(Unit0.InitializationBlock);
  if Unit0.FinalizationBlock <> nil then
    VisitBlock(Unit0.FinalizationBlock);
end;
{$endregion}
{$region "Análisis e Inicialización"}
function TSemanticAnalyzer.Analyze(Prog: TProgram): Boolean;
begin
  Reset;
  RegisterBuiltinTypes;
  RegisterIntrinsicProcedures;
  VisitProgram(Prog);
  Result := FErrors = 0;
end;
function TSemanticAnalyzer.Analyze(Unit0: TUnit): Boolean;
begin
  Reset;
  RegisterBuiltinTypes;
  RegisterIntrinsicProcedures;
  VisitUnit(Unit0);
  Result := FErrors = 0;
end;
procedure TSemanticAnalyzer.Reset;
begin
  FGlobalScope.Clear;
  FCurrentScope := FGlobalScope;
  FErrors := 0;
  FWarnings := 0;
  FCurrentProcedure := nil;
  FCurrentUnit := nil;
  FInWith := False;
  FWithScope := nil;
end;
procedure TSemanticAnalyzer.RegisterBuiltinTypes;
var
  Sym: TSymbol;
  SrcPos: TSrcPos;
begin
  SrcPos.idCtx := 0;
  SrcPos.row := 0;
  SrcPos.col := 0;

  // Tipos predefinidos
  Sym := TSymbol.Create('INTEGER', skType);
  Sym.DataType := TSimpleTypeDef.Create('INTEGER', SrcPos);
  Sym.IsDataTypeOwner := True;  //Para que se libere aquí ya que el AST no lo hará.
  FGlobalScope.Declare(Sym);

  Sym := TSymbol.Create('BYTE', skType);
  Sym.DataType := TSimpleTypeDef.Create('BYTE', SrcPos);
  Sym.IsDataTypeOwner := True;  //Para que se libere aquí ya que el AST no lo hará.
  FGlobalScope.Declare(Sym);

  Sym := TSymbol.Create('WORD', skType);
  Sym.DataType := TSimpleTypeDef.Create('WORD', SrcPos);
  Sym.IsDataTypeOwner := True;  //Para que se libere aquí ya que el AST no lo hará.
  FGlobalScope.Declare(Sym);

  Sym := TSymbol.Create('BOOLEAN', skType);
  Sym.DataType := TSimpleTypeDef.Create('BOOLEAN', SrcPos);
  Sym.IsDataTypeOwner := True;  //Para que se libere aquí ya que el AST no lo hará.
  FGlobalScope.Declare(Sym);

  Sym := TSymbol.Create('CHAR', skType);
  Sym.DataType := TSimpleTypeDef.Create('CHAR', SrcPos);
  Sym.IsDataTypeOwner := True;  //Para que se libere aquí ya que el AST no lo hará.
  FGlobalScope.Declare(Sym);

  Sym := TSymbol.Create('STRING', skType);
  Sym.DataType := TSimpleTypeDef.Create('STRING', SrcPos);
  Sym.IsDataTypeOwner := True;  //Para que se libere aquí ya que el AST no lo hará.
  FGlobalScope.Declare(Sym);

  Sym := TSymbol.Create('REAL', skType);
  Sym.DataType := TSimpleTypeDef.Create('REAL', SrcPos);
  Sym.IsDataTypeOwner := True;  //Para que se libere aquí ya que el AST no lo hará.
  FGlobalScope.Declare(Sym);
end;
procedure TSemanticAnalyzer.RegisterIntrinsicProcedures;
{Registra Procedimientos intrínsecos (del sistema) en la tabla de símbolos.}
var
  Sym: TSymbol;
  Param: TVarDecl;
  DummyPos: TSrcPos;
begin
  DummyPos.idCtx := 0;
  DummyPos.row := 0;
  DummyPos.col := 0;
  //---------- WRITE
  Sym := TSymbol.Create('WRITE', skProcedure);
  Sym.Declaration := nil;
  Sym.IsIntrinsic := True;
  Sym.Parameters := TASTNodeList.Create(True);
  //Parámetros: argumentos variables (array of const)
  Param := TVarDecl.Create('Args', DummyPos);
  Param.TypeDef := TAliasTypeDef.Create('ARRAY_OF_CONST', DummyPos);
  Param.TypeOwner := True;
  Param.IsParameter := True;
  Sym.Parameters.Add(Param);
  FGlobalScope.Declare(Sym);
  //---------- WRITELN
  Sym := TSymbol.Create('WRITELN', skProcedure);
  Sym.Declaration := nil;
  Sym.IsIntrinsic := True;
  Sym.Parameters := TASTNodeList.Create(True);
  //Parámetros: argumentos variables (array of const)
  Param := TVarDecl.Create('Args', DummyPos);
  Param.TypeDef := TAliasTypeDef.Create('ARRAY_OF_CONST', DummyPos);
  Param.TypeOwner := True;
  Param.IsParameter := True;
  Sym.Parameters.Add(Param);
  FGlobalScope.Declare(Sym);
  //---------- READ
  Sym := TSymbol.Create('READ', skProcedure);
  Sym.Declaration := nil;
  Sym.IsIntrinsic := True;
  Sym.Parameters := TASTNodeList.Create(True);
  //Parámetros: argumentos variables (array of const)
  Param := TVarDecl.Create('Args', DummyPos);
  Param.TypeDef := TAliasTypeDef.Create('ARRAY_OF_CONST', DummyPos);
  Param.TypeOwner := True;
  Param.IsParameter := True;
  Sym.Parameters.Add(Param);
  FGlobalScope.Declare(Sym);
  //---------- READLN
  Sym := TSymbol.Create('READLN', skProcedure);
  Sym.Declaration := nil;
  Sym.IsIntrinsic := True;
  Sym.Parameters := TASTNodeList.Create(True);
  //Parámetros: argumentos variables (array of const)
  Param := TVarDecl.Create('Args', DummyPos);
  Param.TypeDef := TAliasTypeDef.Create('ARRAY_OF_CONST', DummyPos);
  Param.TypeOwner := True;
  Param.IsParameter := True;
  Sym.Parameters.Add(Param);
  FGlobalScope.Declare(Sym);
end;
constructor TSemanticAnalyzer.Create(Amsg: TMessageManager; Alex: TAleLexer);
begin
  msg := Amsg;
  lex := ALex;
  FGlobalScope := TScope.Create;
  //FCurrentScope := FGlobalScope;
  //FErrors := 0;
  //FWarnings := 0;
  //FCurrentProcedure := nil;
  //FCurrentUnit := nil;
  //FInWith := False;
  //FWithScope := nil;
  //FUnitManager := nil;
end;
destructor TSemanticAnalyzer.Destroy;
begin
  FGlobalScope.Free;
  FWithScope.Free;      //Elimina si se ha creado.
  inherited;
end;
{$endregion}
end.
