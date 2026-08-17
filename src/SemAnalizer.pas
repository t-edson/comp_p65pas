{Unidad que implementa las estructuras de datos que se usan en el Análisis Semántico.}
unit SemAnalizer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Math, AstPascal, LazLogger, alexiaLex;
type
  //Tipos de símbolos
  TSymbolKind = (
    skVariable,
    skConstant,
    skType,
    skProcedure,
    skFunction,
    skParameter,
    skField,
    skEnumValue
  );

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
  public //Inicialización
    constructor Create(const AName: string; AKind: TSymbolKind);
    destructor Destroy; override;
  end;

  //Ámbito (scope)

  { TScope }

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
    FCurrentProcedure: TProcDecl;
    FErrors: Integer;
    FWarnings: Integer;
    FCurrentUnit: TUnit;
    FUnitManager: TObject; // Referencia al UnitManager (opcional)
    FInWith: Boolean;
    FWithScope: TScope;
    // Registro de símbolos
    function CompareParameters(Sym: TSymbol; Proc: TProcDecl): Boolean;
    procedure RegisterDeclarations(Decls: TDeclarations);
    procedure RegisterProcDecl(Proc: TProcDecl);
    procedure RegisterVarDecl(VarDecl: TVarDecl);
    procedure RegisterConstDecl(ConstDecl: TConstDecl);
    procedure RegisterTypeDef(TypeDef: TTypeDef);
    // Resolución de tipos
    function ResolveType(const TypeName: string): TTypeDef;
    function ResolveTypeDef(TypeDef: TTypeDef): TTypeDef;
    function GetTypeOf(Expr: TExpression): TTypeDef;
    function AreTypesCompatible(T1, T2: TTypeDef): Boolean;
    function IsNumericType(TypeDef: TTypeDef): Boolean;
    function IsOrdinalType(TypeDef: TTypeDef): Boolean;
    // Visitantes
    procedure VisitNode(Node: TASTNode);
    procedure VisitProgram(Prog: TProgram);
    procedure VisitUnit(Unit0: TUnit);
    procedure VisitBlock(Block: TBlock);
    procedure VisitDeclarations(Decls: TDeclarations);
    procedure VisitVarDecl(VarDecl: TVarDecl);
    procedure VisitConstDecl(ConstDecl: TConstDecl);
    procedure VisitProcDecl(Proc: TProcDecl);
    procedure VisitTypeDef(TypeDef: TTypeDef);
    procedure VisitArrayTypeDef(ArrayType: TArrayTypeDef);
    procedure VisitRecordTypeDef(RecordType: TRecordTypeDef);
    procedure VisitEnumTypeDef(EnumType: TEnumTypeDef);
    procedure VisitSubrangeTypeDef(SubrangeType: TSubrangeTypeDef);
    procedure VisitPointerTypeDef(PointerType: TPointerTypeDef);
    // Visitantes de sentencias
    procedure VisitAssignment(Assign: TAssignment);
    procedure VisitIfStatement(IfStmt: TIfStatement);
    procedure VisitWhileLoop(WhileLoop: TWhileLoop);
    procedure VisitRepeatUntil(RepeatUntil: TRepeatUntil);
    procedure VisitForLoop(ForLoop: TForLoop);
    procedure VisitCaseStatement(CaseStmt: TCaseStatement);
    procedure VisitCaseBranch(CaseBranch: TCaseBranch);
    procedure VisitWithStatement(WithStmt: TWithStatement);
    procedure VisitExitStatement(ExitStmt: TExitStatement);
    // Visitantes de expresiones
    procedure VisitVariableRef(VarRef: TVariableRef);
    procedure VisitNumberLiteral(NumLit: TNumberLiteral);
    procedure VisitBooleanLiteral(BoolLit: TBooleanLiteral);
    procedure VisitStringLiteral(StrLit: TStringLiteral);
    procedure VisitBinaryOp(BinOp: TBinaryOp);
    procedure VisitUnaryOp(UnaryOp: TUnaryOp);
    procedure VisitFunctionCall(FuncCall: TFunctionCall);
    procedure VisitFieldAccess(FieldAccess: TFieldAccess);
    procedure VisitPointerDeref(PointerDeref: TPointerDeref);
    procedure VisitArrayIndex(ArrayIndex: TArrayIndex);
    procedure VisitArrayLiteral(ArrayLit: TArrayLiteral);
    procedure VisitRecordLiteral(RecordLit: TRecordLiteral);
    procedure VisitPointerLiteral(PointerLit: TPointerLiteral);
    // Manejo de ámbitos
    procedure EnterScope;
    procedure ExitScope;
    procedure EnterWithScope(RecordVar: TExpression);
    procedure ExitWithScope;
    // Manejo de errores
    procedure Error(const txt: string; const SrcPos: TSrcPos);
    procedure Warning(const txt: string; const SrcPos: TSrcPos);
    function GetCurrentLocation: TSrcPos;
    // Utilidades
    function IsInFunction: Boolean;
    function IsInProcedure: Boolean;
    function GetCurrentFunction: TProcDecl;
    function GetCurrentProcedure: TProcDecl;
  public   //Métodos principales
    property Errors: Integer read FErrors;
    property Warnings: Integer read FWarnings;
    property GlobalScope: TScope read FGlobalScope;
    function Analyze(Prog: TProgram): Boolean; overload;
    function Analyze(Unit0: TUnit): Boolean; overload;
    procedure SetUnitManager(AManager: TObject);
  public //Inicialización
    procedure Reset;
    procedure RegisterBuiltinTypes;
    procedure RegisterIntrinsicProcedures;
    constructor Create(Amsg: TMessageManager; Alex: TAleLexer);
    destructor Destroy; override;
  end;

implementation

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
  while Scope <> nil do
  begin
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
  Child: TScope;
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
{ TSemanticAnalyzer }
// Registro de símbolos
procedure TSemanticAnalyzer.RegisterDeclarations(Decls: TDeclarations);
var
  i: Integer;
  Node: TASTNode;
begin
  if Decls = nil then Exit;
  for i := 0 to Decls.Items.Count - 1 do begin
    Node := Decls.Items[i];
    case Node.NodeType of
      ntVarDecl:
        RegisterVarDecl(TVarDecl(Node));
      ntConstDecl:
        RegisterConstDecl(TConstDecl(Node));
      ntProcDecl:
        RegisterProcDecl(TProcDecl(Node));
      ntSimpleType, ntSubrangeType, ntEnumType, ntArrayType,
      ntRecordType, ntPointerType, ntAliasType, ntProceduralType:
        RegisterTypeDef(TTypeDef(Node));
    end;
  end;
end;
procedure TSemanticAnalyzer.RegisterVarDecl(VarDecl: TVarDecl);
var
  Sym: TSymbol;
  TypeDef: TTypeDef;
begin
  // Verificar duplicado
  if FCurrentScope.Lookup(VarDecl.Name) <> nil then begin
    Error('Variable duplicada: ' + VarDecl.Name, VarDecl.SrcPos);
    Exit;
  end;
  // Resolver tipo
  if VarDecl.TypeName <> '' then
    TypeDef := ResolveType(VarDecl.TypeName)
  else if VarDecl.TypeDef <> nil then
    TypeDef := ResolveTypeDef(VarDecl.TypeDef)
  else begin
    Error('Tipo no especificado para: ' + VarDecl.Name, VarDecl.SrcPos);
    Exit;
  end;
  if TypeDef = nil then begin
    Error('Tipo desconocido: ' + VarDecl.TypeName, VarDecl.TypeSrc);
    Exit;
  end;
  // Crear símbolo
  Sym := TSymbol.Create(VarDecl.Name, skVariable);
  Sym.DataType := TypeDef;
  Sym.Declaration := VarDecl;
  FCurrentScope.Declare(Sym);
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
    TypeDef := ResolveType(ConstDecl.TypeName)
  end else begin
    // Inferir tipo del valor
    if ConstDecl.Value <> nil then
      TypeDef := GetTypeOf(ConstDecl.Value)
    else
      TypeDef := nil;
  end;
  // Crear símbolo
  Sym := TSymbol.Create(ConstDecl.Name, skConstant);
  Sym.DataType := TypeDef;
  Sym.Declaration := ConstDecl;
  FCurrentScope.Declare(Sym);
end;
function TSemanticAnalyzer.CompareParameters(Sym: TSymbol; Proc: TProcDecl): Boolean;
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
    if Param1.TypeName <> Param2.TypeName then Exit(False);
    // Comparar tipo de parámetro (var, const, out)
    if Param1.ParamType <> Param2.ParamType then
      Exit(False);
  end;
  Result := True;
end;
procedure TSemanticAnalyzer.RegisterProcDecl(Proc: TProcDecl);
{Registra las declaraciones de procedimientos/funciones, pero sin analizar el cuerpo, aún.}
var
  Sym: TSymbol;
  i: Integer;
  Param: TVarDecl;
begin
  //Verifica si es una declaración duplicada
  Sym := FCurrentScope.Lookup(Proc.Name);
  if Sym <> nil then begin
    if Sym.IsForward and not Proc.IsForward then begin
      //Es la implementación de un FORWARD.
      //Verificamos que los parámetros coincidan.
      if not CompareParameters(Sym, Proc) then begin
        Error('La implementación de ' + Proc.Name +
              ' no coincide con la declaración FORWARD', Proc.SrcPos);
        Exit;
      end;
      //Actualizar el símbolo con la implementación
      Sym.Declaration := Proc;
      Sym.IsForward := False;
      Exit;
    end else begin
      //Duplicado real
      Error('Procedimiento/Función duplicado: ' + Proc.Name, Proc.SrcPos);
      Exit;
    end;
    end;
  // Crear símbolo
  if Proc.IsFunction then
    Sym := TSymbol.Create(Proc.Name, skFunction)
  else
    Sym := TSymbol.Create(Proc.Name, skProcedure);
  Sym.Declaration := Proc;
  Sym.IsForward := Proc.IsForward;
  // Registrar parámetros (solo para validación, no se declaran en el ámbito global)
  if Proc.Parameters <> nil then begin
    Sym.Parameters := TASTNodeList.Create(True);
    for i := 0 to Proc.Parameters.Count - 1 do begin
      Param := TVarDecl(Proc.Parameters[i]);
      Sym.Parameters.Add(Param);
    end;
  end;
  //Tipo de retorno para funciones
  if Proc.IsFunction then begin
    if Proc.ReturnTypeName <> '' then
      Sym.ReturnType := ResolveType(Proc.ReturnTypeName)
    else if Proc.ReturnTypeDef <> nil then
      Sym.ReturnType := ResolveTypeDef(Proc.ReturnTypeDef);
    if Sym.ReturnType = nil then
      Error('Tipo de retorno desconocido para: ' + Proc.Name, Proc.SrcPos);
  end;
  FCurrentScope.Declare(Sym);
end;
procedure TSemanticAnalyzer.RegisterTypeDef(TypeDef: TTypeDef);
var
  Sym: TSymbol;
begin
  if TypeDef.TypeName = '' then
    Exit; // Tipo anónimo (inline)

  // Verificar duplicado
  if FCurrentScope.Lookup(TypeDef.TypeName) <> nil then begin
    Error('Tipo duplicado: ' + TypeDef.TypeName, TypeDef.SrcPos);
    Exit;
  end;

  Sym := TSymbol.Create(TypeDef.TypeName, skType);
  Sym.DataType := TypeDef;
  Sym.Declaration := TypeDef;
  FCurrentScope.Declare(Sym);
end;
// Resolución de tipos
function TSemanticAnalyzer.ResolveType(const TypeName: string): TTypeDef;
var
  Sym: TSymbol;
begin
  Sym := FCurrentScope.LookupRecursive(UpperCase(TypeName));
  if Sym = nil then
    Result := nil
  else if Sym.Kind = skType then
    Result := Sym.DataType
  else
    Result := nil;
end;
function TSemanticAnalyzer.ResolveTypeDef(TypeDef: TTypeDef): TTypeDef;
begin
  // Si es un alias, resolver el tipo base
  if TypeDef is TAliasTypeDef then
  begin
    if TAliasTypeDef(TypeDef).BaseTypeDef <> nil then
      Result := ResolveTypeDef(TAliasTypeDef(TypeDef).BaseTypeDef)
    else if TAliasTypeDef(TypeDef).BaseTypeName <> '' then
      Result := ResolveType(TAliasTypeDef(TypeDef).BaseTypeName)
    else
      Result := TypeDef;
  end
  else
    Result := TypeDef;
end;
function TSemanticAnalyzer.GetTypeOf(Expr: TExpression): TTypeDef;
var
  Sym: TSymbol;
  ArrayVarType: TTypeDef;
  ArrayType: TArrayTypeDef;
begin
  if Expr = nil then Exit(nil);

  case Expr.NodeType of
    ntNumberLiteral: begin
      if TNumberLiteral(Expr).IsInteger then
        Result := ResolveType('INTEGER')
      else
        Result := ResolveType('REAL');
    end;
    ntBooleanLiteral:
      Result := ResolveType('BOOLEAN');
    ntStringLiteral:
      Result := ResolveType('STRING');
    ntVariableRef: begin
      Sym := FCurrentScope.LookupRecursive(TVariableRef(Expr).Name);
      if Sym <> nil then
      begin
        Result := Sym.DataType;
        // Enlazar la referencia a su declaración
        TVariableRef(Expr).Declaration := TVarDecl(Sym.Declaration);
      end
      else
        Result := nil;
    end;
    ntBinaryOp: begin
      // El tipo de una operación binaria es el tipo del operando izquierdo
      // (simplificado, debería ser más complejo)
      Result := GetTypeOf(TBinaryOp(Expr).Left);
    end;
    ntUnaryOp:
      Result := GetTypeOf(TUnaryOp(Expr).Operand);
    ntFunctionCall: begin
      Sym := FCurrentScope.LookupRecursive(TFunctionCall(Expr).Name);
      if Sym <> nil then
      begin
        if Sym.Kind = skFunction then
          Result := Sym.ReturnType
        else if Sym.Kind = skProcedure then
          Result := nil
        else
          Result := nil;
      end
      else
        Result := nil;
    end;
    ntFieldAccess:
      // El tipo de un campo se resuelve durante el análisis
      Result := nil;
    ntArrayRefer: begin
      // El tipo de un arreglo es el tipo de sus elementos
      ArrayVarType := GetTypeOf(TArrayIndex(Expr).ArrayVar);  //Obtiene el tipo del arreglo
      if ArrayVarType = nil then Exit(nil);      //Valida que exista
      if ArrayVarType.NodeType <> ntArrayType then Exit(nil);  //Valida que sea arreglo
      ArrayType := TArrayTypeDef(ArrayVarType);    //Convierte a TArrayTypeDef
      // Resuelve el tipo de los elementos
      if ArrayType.ElementTypeName <> '' then
        Result := ResolveType(ArrayType.ElementTypeName)
      else if ArrayType.ElementTypeDef <> nil then
        Result := ResolveTypeDef(ArrayType.ElementTypeDef)
      else
        Result := nil;
    end;
    ntPointerLiteral:
      Result := ResolveType('POINTER');

    else
      Result := nil;
  end;
end;
function TSemanticAnalyzer.AreTypesCompatible(T1, T2: TTypeDef): Boolean;
begin
  if (T1 = nil) or (T2 = nil) then
    Exit(False);

  // Mismo tipo
  if T1 = T2 then
    Exit(True);

  // Tipos simples
  if (T1 is TSimpleTypeDef) and (T2 is TSimpleTypeDef) then
  begin
    // Compatibilidad numérica
    if IsNumericType(T1) and IsNumericType(T2) then
      Exit(True);

    // String con string
    if (T1.TypeName = 'STRING') and (T2.TypeName = 'STRING') then
      Exit(True);

    // Boolean con boolean
    if (T1.TypeName = 'BOOLEAN') and (T2.TypeName = 'BOOLEAN') then
      Exit(True);
  end;

  // Subrango con su tipo base
  if (T1 is TSubrangeTypeDef) then
    T1 := TSimpleTypeDef.Create('INTEGER', T1.SrcPos);

  Result := False;
end;
function TSemanticAnalyzer.IsNumericType(TypeDef: TTypeDef): Boolean;
begin
  if TypeDef = nil then
    Exit(False);

  if TypeDef is TSimpleTypeDef then
  begin
    Result := (TypeDef.TypeName = 'INTEGER') or
              (TypeDef.TypeName = 'BYTE') or
              (TypeDef.TypeName = 'WORD') or
              (TypeDef.TypeName = 'REAL');
    Exit;
  end;

  if TypeDef is TSubrangeTypeDef then
    Exit(True);

  Result := False;
end;
function TSemanticAnalyzer.IsOrdinalType(TypeDef: TTypeDef): Boolean;
begin
  if TypeDef = nil then
    Exit(False);

  if TypeDef is TSimpleTypeDef then
  begin
    Result := (TypeDef.TypeName = 'INTEGER') or
              (TypeDef.TypeName = 'BYTE') or
              (TypeDef.TypeName = 'WORD') or
              (TypeDef.TypeName = 'BOOLEAN') or
              (TypeDef.TypeName = 'CHAR');
    Exit;
  end;

  if TypeDef is TEnumTypeDef then
    Exit(True);

  if TypeDef is TSubrangeTypeDef then
    Exit(True);

  Result := False;
end;
// Visitantes de nodos principales
procedure TSemanticAnalyzer.VisitNode(Node: TASTNode);
begin
  if Node = nil then
    Exit;

  case Node.NodeType of
    // Programas y unidades
    ntProgram: VisitProgram(TProgram(Node));
    ntUnit: VisitUnit(TUnit(Node));

    // Bloques y declaraciones
    ntBlock: VisitBlock(TBlock(Node));
    ntDeclarations: VisitDeclarations(TDeclarations(Node));
    ntVarDecl: VisitVarDecl(TVarDecl(Node));
    ntConstDecl: VisitConstDecl(TConstDecl(Node));
    ntProcDecl: VisitProcDecl(TProcDecl(Node));

    // Tipos
    ntSimpleType: VisitTypeDef(TTypeDef(Node));
    ntSubrangeType: VisitTypeDef(TTypeDef(Node));
    ntEnumType: VisitTypeDef(TTypeDef(Node));
    ntArrayType: VisitArrayTypeDef(TArrayTypeDef(Node));
    ntRecordType: VisitRecordTypeDef(TRecordTypeDef(Node));
    ntPointerType: VisitPointerTypeDef(TPointerTypeDef(Node));
    ntAliasType: VisitTypeDef(TTypeDef(Node));
    ntProceduralType: VisitTypeDef(TTypeDef(Node));

    // Sentencias
    ntAssignment: VisitAssignment(TAssignment(Node));
    ntIfStatement: VisitIfStatement(TIfStatement(Node));
    ntWhileLoop: VisitWhileLoop(TWhileLoop(Node));
    ntRepeatUntil: VisitRepeatUntil(TRepeatUntil(Node));
    ntForLoop: VisitForLoop(TForLoop(Node));
    ntCaseStatement: VisitCaseStatement(TCaseStatement(Node));
    ntCaseBranch: VisitCaseBranch(TCaseBranch(Node));
    ntWithStatement: VisitWithStatement(TWithStatement(Node));
    ntExitStatement: VisitExitStatement(TExitStatement(Node));

    // Expresiones
    ntVariableRef: VisitVariableRef(TVariableRef(Node));
    ntNumberLiteral: VisitNumberLiteral(TNumberLiteral(Node));
    ntBooleanLiteral: VisitBooleanLiteral(TBooleanLiteral(Node));
    ntStringLiteral: VisitStringLiteral(TStringLiteral(Node));
    ntBinaryOp: VisitBinaryOp(TBinaryOp(Node));
    ntUnaryOp: VisitUnaryOp(TUnaryOp(Node));
    ntFunctionCall: VisitFunctionCall(TFunctionCall(Node));
    ntFieldAccess: VisitFieldAccess(TFieldAccess(Node));
    ntPointerDeref: VisitPointerDeref(TPointerDeref(Node));
    ntArrayRefer: VisitArrayIndex(TArrayIndex(Node));
    ntArrayLiteral: VisitArrayLiteral(TArrayLiteral(Node));
    ntRecordLiteral: VisitRecordLiteral(TRecordLiteral(Node));
    ntPointerLiteral: VisitPointerLiteral(TPointerLiteral(Node));
  end;
end;
procedure TSemanticAnalyzer.VisitProgram(Prog: TProgram);
begin
  if Prog = nil then Exit;
  // Registrar declaraciones globales
  RegisterDeclarations(Prog.Declarations);
  // Analizar el cuerpo principal
  VisitBlock(Prog.Body);
end;
procedure TSemanticAnalyzer.VisitUnit(Unit0: TUnit);
begin
  if Unit0 = nil then Exit;
  FCurrentUnit := Unit0;
  // Registrar declaraciones de interface
  RegisterDeclarations(Unit0.InterfaceDecls);
  // Registrar declaraciones de implementation
  RegisterDeclarations(Unit0.ImplementationDecls);
  // Analizar initialization y finalization
  if Unit0.InitializationBlock <> nil then
    VisitBlock(Unit0.InitializationBlock);
  if Unit0.FinalizationBlock <> nil then
    VisitBlock(Unit0.FinalizationBlock);
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
procedure TSemanticAnalyzer.VisitDeclarations(Decls: TDeclarations);
begin
  // Las declaraciones ya se registraron en RegisterDeclarations
  // Aquí solo se analizan los detalles adicionales
  if Decls = nil then
    Exit;

  // Analizar declaraciones de tipo
  RegisterDeclarations(Decls);
end;
// Visitantes de declaraciones
procedure TSemanticAnalyzer.VisitVarDecl(VarDecl: TVarDecl);
var
  InitType, VarType: TTypeDef;
begin
  // Ya fue registrada en RegisterVarDecl
  // Verificar inicialización
  if VarDecl.initVal <> nil then
  begin
    InitType := GetTypeOf(VarDecl.initVal);
    VarType := ResolveType(VarDecl.TypeName);
    if not AreTypesCompatible(VarType, InitType) then
      Error('Tipo de inicialización incompatible para: ' + VarDecl.Name, VarDecl.SrcPos);
  end;

  // Verificar ABSOLUTE
  if VarDecl.hasAdic = DEC_ABSOL then
  begin
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
procedure TSemanticAnalyzer.VisitProcDecl(Proc: TProcDecl);
var
  i: Integer;
  Param: TVarDecl;
  Sym: TSymbol;
  OldProcedure: TProcDecl;
  ParamType: TTypeDef;
begin
  if Proc.IsForward then
    Exit;
  OldProcedure := FCurrentProcedure;
  FCurrentProcedure := Proc;
  EnterScope;
  try
    // Registrar parámetros en el ámbito local
    if Proc.Parameters <> nil then begin
      for i := 0 to Proc.Parameters.Count - 1 do begin
        Param := TVarDecl(Proc.Parameters[i]);
        // Resolver tipo del parámetro
        if Param.TypeName <> '' then
          ParamType := ResolveType(Param.TypeName)
        else if Param.TypeDef <> nil then
          ParamType := ResolveTypeDef(Param.TypeDef);
        if ParamType = nil then
          Error('Tipo desconocido para parámetro: ' + Param.Name, Param.SrcPos);

        // Registrar parámetro
        Sym := TSymbol.Create(Param.Name, skParameter);
        Sym.DataType := ParamType;
        Sym.Declaration := Param;
        FCurrentScope.Declare(Sym);
      end;
    end;

    // Analizar declaraciones locales
    if Proc.Declarations <> nil then
      RegisterDeclarations(Proc.Declarations);

    // Analizar el cuerpo
    if Proc.Body <> nil then
      VisitBlock(Proc.Body);

  finally
    ExitScope;
    FCurrentProcedure := OldProcedure;
  end;
end;
procedure TSemanticAnalyzer.VisitTypeDef(TypeDef: TTypeDef);
var
  BaseType: TTypeDef;
begin
  // Ya fue registrada en RegisterTypeDef
  // Verificar definiciones recursivas
  if TypeDef is TAliasTypeDef then
  begin
    if TAliasTypeDef(TypeDef).BaseTypeName <> '' then
    begin
      BaseType := ResolveType(TAliasTypeDef(TypeDef).BaseTypeName);
      if BaseType = nil then
        Error('Tipo base desconocido: ' + TAliasTypeDef(TypeDef).BaseTypeName, TypeDef.SrcPos);
    end;
  end;
end;
procedure TSemanticAnalyzer.VisitArrayTypeDef(ArrayType: TArrayTypeDef);
var
  ElemType: TTypeDef;
  i: Integer;
  Range: TArrayRange;
begin
  // Verificar tipo de elementos
  if ArrayType.ElementTypeName <> '' then
  begin
    ElemType := ResolveType(ArrayType.ElementTypeName);
    if ElemType = nil then
      Error('Tipo de elemento desconocido: ' + ArrayType.ElementTypeName, ArrayType.SrcPos);
  end;

  // Verificar rangos de índices
  for i := 0 to ArrayType.IndexRanges.Count - 1 do
  begin
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
begin
  // Analizar campos
  for i := 0 to RecordType.Fields.Count - 1 do
  begin
    if RecordType.Fields[i] is TVarDecl then
      VisitVarDecl(TVarDecl(RecordType.Fields[i]));
  end;

  // Analizar variantes
  if RecordType.Branches <> nil then
  begin
    for i := 0 to RecordType.Branches.Count - 1 do
    begin
      Branch := RecordType.Branches[i];
      // Analizar selectores
      for j := 0 to Branch.SelectorValues.Count - 1 do
        VisitNode(Branch.SelectorValues[j]);
      // Analizar campos
      for j := 0 to Branch.Fields.Count - 1 do
      begin
        if Branch.Fields[j] is TVarDecl then
          VisitVarDecl(TVarDecl(Branch.Fields[j]));
      end;
    end;
  end;
end;
procedure TSemanticAnalyzer.VisitEnumTypeDef(EnumType: TEnumTypeDef);
var
  i, j: Integer;
begin
  // Verificar valores duplicados
  for i := 0 to EnumType.Values.Count - 1 do
  begin
    for j := i + 1 to EnumType.Values.Count - 1 do
    begin
      if EnumType.Values[i] = EnumType.Values[j] then
        Error('Valor de enumerado duplicado: ' + EnumType.Values[i], EnumType.SrcPos);
    end;
  end;
end;
procedure TSemanticAnalyzer.VisitSubrangeTypeDef(SubrangeType: TSubrangeTypeDef);
begin
  // Analizar límites
  if SubrangeType.LowExpr <> nil then
    VisitNode(SubrangeType.LowExpr);
  if SubrangeType.HighExpr <> nil then
    VisitNode(SubrangeType.HighExpr);
end;
procedure TSemanticAnalyzer.VisitPointerTypeDef(PointerType: TPointerTypeDef);
var
  TargetType: TTypeDef;
begin
  // Verificar tipo apuntado
  if PointerType.TargetTypeName <> '' then
  begin
    TargetType := ResolveType(PointerType.TargetTypeName);
    if TargetType = nil then
      Error('Tipo apuntado desconocido: ' + PointerType.TargetTypeName, PointerType.SrcPos);
  end;
end;
// Visitantes de sentencias
procedure TSemanticAnalyzer.VisitAssignment(Assign: TAssignment);
var
  TargetType, ValueType: TTypeDef;
begin
  // Analizar destino
  VisitNode(Assign.Target);
  TargetType := GetTypeOf(Assign.Target);

  // Analizar valor
  VisitNode(Assign.Value);
  ValueType := GetTypeOf(Assign.Value);

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
  else if (CondType.TypeName <> 'BOOLEAN') and (not (CondType is TSimpleTypeDef)) then
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
  // Verificar variable de control
  if ForLoop.ControlVar <> nil then
  begin
    Sym := FCurrentScope.LookupRecursive(ForLoop.ControlVar.Name);
    if Sym = nil then
      Error('Variable de control no declarada: ' + ForLoop.ControlVar.Name, ForLoop.ControlVar.SrcPos)
    else
    begin
      ControlType := Sym.DataType;
      if not IsOrdinalType(ControlType) then
        Error('La variable de control debe ser de tipo ordinal', ForLoop.ControlVar.SrcPos);
    end;
  end;

  // Analizar expresiones
  VisitNode(ForLoop.StartExpr);
  VisitNode(ForLoop.EndExpr);

  // Analizar cuerpo
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
  else if not (RecordType is TRecordTypeDef) then
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
  Func: TProcDecl;
begin
  if ExitStmt.HasReturnValue then
  begin
    // Verificar que estamos en una función
    if not IsInFunction then
    begin
      Error('EXIT con valor solo permitido en funciones', ExitStmt.SrcPos);
      Exit;
    end;

    // Verificar compatibilidad del valor de retorno
    VisitNode(ExitStmt.ReturnValue);
    ValueType := GetTypeOf(ExitStmt.ReturnValue);

    Func := GetCurrentFunction;
    if Func <> nil then
    begin
      if Func.ReturnTypeName <> '' then
        ReturnType := ResolveType(Func.ReturnTypeName)
      else if Func.ReturnTypeDef <> nil then
        ReturnType := ResolveTypeDef(Func.ReturnTypeDef);

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
// Visitantes de expresiones
procedure TSemanticAnalyzer.VisitVariableRef(VarRef: TVariableRef);
var
  Sym: TSymbol;
begin
  Sym := FCurrentScope.LookupRecursive(VarRef.Name);
  if Sym = nil then begin
    Error('Variable no declarada: ' + VarRef.Name, VarRef.SrcPos);
    Exit;
  end;
  //Enlaza a la declaración
  VarRef.Declaration := TVarDecl(Sym.Declaration);
end;
procedure TSemanticAnalyzer.VisitNumberLiteral(NumLit: TNumberLiteral);
begin
  // Nada que verificar, los literales son siempre correctos
end;
procedure TSemanticAnalyzer.VisitBooleanLiteral(BoolLit: TBooleanLiteral);
begin
  // Nada que verificar
end;
procedure TSemanticAnalyzer.VisitStringLiteral(StrLit: TStringLiteral);
begin
  // Nada que verificar
end;
procedure TSemanticAnalyzer.VisitBinaryOp(BinOp: TBinaryOp);
var
  LeftType, RightType: TTypeDef;
begin
  // Analizar operandos
  VisitNode(BinOp.Left);
  VisitNode(BinOp.Right);

  LeftType := GetTypeOf(BinOp.Left);
  RightType := GetTypeOf(BinOp.Right);

  // Verificar operadores
  case BinOp.Op of
    '+', '-', '*', '/', 'div', 'mod':
    begin
      if not IsNumericType(LeftType) then
        Warning('Operador aritmético con tipo no numérico', BinOp.Left.SrcPos);
      if not IsNumericType(RightType) then
        Warning('Operador aritmético con tipo no numérico', BinOp.Right.SrcPos);
    end;
    'and', 'or', 'not':
    begin
      // Verificar tipos booleanos
    end;
    '=', '<>', '<', '>', '<=', '>=':
    begin
      // Verificar compatibilidad
      if (LeftType <> nil) and (RightType <> nil) then
      begin
        if not AreTypesCompatible(LeftType, RightType) then
          Warning('Comparación de tipos incompatibles', BinOp.SrcPos);
      end;
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
    '+', '-':
      if not IsNumericType(OpType) then
        Warning('Operador unario con tipo no numérico', UnaryOp.Operand.SrcPos);
    'not':
      if (OpType <> nil) and (OpType.TypeName <> 'BOOLEAN') then
        Warning('NOT aplicado a tipo no booleano', UnaryOp.Operand.SrcPos);
  end;
end;
procedure TSemanticAnalyzer.VisitFunctionCall(FuncCall: TFunctionCall);
{Visita la llamada a un procedimiento o función, que se ha identificado como tal en el
análisis sintáctico.}
var
  Sym: TSymbol;
  i: Integer;
  ProcDecl: TProcDecl;
  ParamType: TTypeDef;
  ArgType: TTypeDef;
  Param: TVarDecl;
  Parent: TASTNode;
begin
  // Buscar la función/procedimiento
  Sym := FCurrentScope.LookupRecursive(FuncCall.Name);
  if Sym = nil then begin
    Error('Identificador no declarado: ' + FuncCall.Name, FuncCall.SrcPos);
    Exit;
  end;
  if (Sym.Kind <> skFunction) and (Sym.Kind <> skProcedure) then begin
    Error(FuncCall.Name + ' no es una función o procedimiento', FuncCall.SrcPos);
    Exit;
  end;
  //Enlaza referencia a la declaración, si existe
  if not Sym.IsIntrinsic then begin
    //Debe haber declaración
    if Sym.Declaration.NodeType = ntProcDecl then begin
      //Su declaración figura como procedimiento o función
      FuncCall.Declaration := TProcDecl(Sym.Declaration); //Enlaza a declaración
    end else begin  //Figura como otra cosa
      Error('Declaración inválida para: ' + FuncCall.Name, FuncCall.SrcPos);
      Exit;
    end;
  end;
  //Completa atributos
  FuncCall.IsProcedure := (Sym.Kind = skProcedure);   //Aquí se puede saber si es proc. o función.
  FuncCall.IsIntrinsic := Sym.IsIntrinsic;            //Y si es del sistema.
  //Verifica argumentos
  if Sym.IsIntrinsic then begin
    //Verificación flexible: aceptan cualquier número de argumentos
    //y cualquier tipo (dentro de lo razonable)
    for i := 0 to FuncCall.Arguments.Count - 1 do begin
      VisitNode(FuncCall.Arguments[i]);
      // No verificamos tipos estrictos
    end;
  end else begin
    //Proc./Funciones normales
    if Sym.Parameters <> nil then begin
      if FuncCall.Arguments.Count <> Sym.Parameters.Count then
        Error('Número incorrecto de argumentos para ' + FuncCall.Name + ' (esperaba ' +
              IntToStr(Sym.Parameters.Count) + ', tiene ' +
              IntToStr(FuncCall.Arguments.Count) + ')', FuncCall.SrcPos);

      //Verifica tipos de argumentos
      for i := 0 to Min(FuncCall.Arguments.Count, Sym.Parameters.Count) - 1 do begin
        VisitNode(FuncCall.Arguments[i]);
        ArgType := GetTypeOf(FuncCall.Arguments[i]);

        Param := TVarDecl(Sym.Parameters[i]);
        if Param.TypeName <> '' then
          ParamType := ResolveType(Param.TypeName)
        else if Param.TypeDef <> nil then
          ParamType := ResolveTypeDef(Param.TypeDef)
        else
          ParamType := nil;

        if not AreTypesCompatible(ParamType, ArgType) then
          Error('Tipo de argumento incompatible para parámetro ' + IntToStr(i+1) + ' de ' +
                FuncCall.Name, FuncCall.Arguments[i].SrcPos);
      end;
    end else begin
      //Sin parámetros declarados, verifica que no haya argumentos
      if FuncCall.Arguments.Count > 0 then
        Error(FuncCall.Name + ' no acepta argumentos', FuncCall.SrcPos);
    end;
  end;

  //Si es procedimiento, verificar que se use como sentencia
  if FuncCall.IsProcedure then begin
    //Verificar contexto: ¿está en una sentencia o en una expresión?
    Parent := FuncCall.Parent;
    if Parent = Nil then
      //No se identifica al padre
    else if Parent.NodeType = ntBlock then
      // OK: está en una sentencia
    else if Parent.NodeType = ntAssignment then
      Error('El procedimiento ' + FuncCall.Name + ' no puede usarse como expresión', FuncCall.SrcPos)
    else if Parent.NodeType = ntBinaryOp then
      Error('El procedimiento ' + FuncCall.Name + ' no puede usarse como expresión', FuncCall.SrcPos)
    else if Parent.NodeType = ntIfStatement then
      Error('El procedimiento ' + FuncCall.Name + ' no puede usarse como condición', FuncCall.SrcPos)
  end;
end;
procedure TSemanticAnalyzer.VisitFieldAccess(FieldAccess: TFieldAccess);
var
  RecordType: TTypeDef;
  Sym: TSymbol;
  FoundField: Boolean;
  i: Integer;
  FieldDecl: TVarDecl;
begin
  // Analizar la variable registro
  VisitNode(FieldAccess.RecordVar);
  RecordType := GetTypeOf(FieldAccess.RecordVar);

  if RecordType = nil then
  begin
    Error('No se puede determinar el tipo del registro', FieldAccess.RecordVar.SrcPos);
    Exit;
  end;

  // Buscar el campo en el registro
  FoundField := False;
  if RecordType is TRecordTypeDef then
  begin
    for i := 0 to TRecordTypeDef(RecordType).Fields.Count - 1 do
    begin
      if TRecordTypeDef(RecordType).Fields[i] is TVarDecl then
      begin
        FieldDecl := TVarDecl(TRecordTypeDef(RecordType).Fields[i]);
        if FieldDecl.Name = FieldAccess.FieldName then
        begin
          FoundField := True;
          Break;
        end;
      end;
    end;
  end;

  if not FoundField then
    Error('Campo no encontrado en el RECORD: ' + FieldAccess.FieldName, FieldAccess.SrcPos);
end;
procedure TSemanticAnalyzer.VisitPointerDeref(PointerDeref: TPointerDeref);
var
  PtrType: TTypeDef;
begin
  VisitNode(PointerDeref.Pointer);
  PtrType := GetTypeOf(PointerDeref.Pointer);

  if PtrType <> nil then
  begin
    if not (PtrType is TPointerTypeDef) then
      Error('^ solo puede aplicarse a punteros', PointerDeref.SrcPos);
  end;
end;
procedure TSemanticAnalyzer.VisitArrayIndex(ArrayIndex: TArrayIndex);
var
  ArrayType, IdxType: TTypeDef;
  i: Integer;
begin
  // Analizar la variable arreglo
  VisitNode(ArrayIndex.ArrayVar);
  ArrayType := GetTypeOf(ArrayIndex.ArrayVar);

  if ArrayType = nil then begin
    Error('No se puede determinar el tipo del arreglo', ArrayIndex.ArrayVar.SrcPos);
    Exit;
  end;

  // Verificar que sea un arreglo
  if not (ArrayType is TArrayTypeDef) then begin
    Error('[] solo puede aplicarse a arreglos', ArrayIndex.SrcPos);
    Exit;
  end;

  // Verificar número de índices
  if ArrayIndex.Indices.Count <> TArrayTypeDef(ArrayType).IndexRanges.Count then
    Error('Número incorrecto de índices para el arreglo (esperaba ' +
          IntToStr(TArrayTypeDef(ArrayType).IndexRanges.Count) + ', tiene ' +
          IntToStr(ArrayIndex.Indices.Count) + ')', ArrayIndex.SrcPos);

  // Analizar índices
  for i := 0 to ArrayIndex.Indices.Count - 1 do begin
    VisitNode(ArrayIndex.Indices[i]);
    IdxType := GetTypeOf(ArrayIndex.Indices[i]);
    if not IsOrdinalType(IdxType) then
      Warning('El índice debe ser de tipo ordinal', ArrayIndex.Indices[i].SrcPos);
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
// Manejo de ámbitos
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

  if not (RecordType is TRecordTypeDef) then
    Exit;

  // Crear nuevo ámbito para WITH
  NewScope := TScope.Create(FCurrentScope);
  if FCurrentScope <> nil then
    FCurrentScope.AddChild(NewScope);

  // Registrar los campos del registro como símbolos
  for i := 0 to TRecordTypeDef(RecordType).Fields.Count - 1 do
  begin
    if TRecordTypeDef(RecordType).Fields[i] is TVarDecl then
    begin
      FieldDecl := TVarDecl(TRecordTypeDef(RecordType).Fields[i]);
      Sym := TSymbol.Create(FieldDecl.Name, skField);
      Sym.DataType := ResolveType(FieldDecl.TypeName);
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
  if FInWith and (FWithScope <> nil) then
  begin
    FCurrentScope := FWithScope.FParent;
    FWithScope.Free;
    FWithScope := nil;
    FInWith := False;
  end;
end;
// Manejo de errores
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
function TSemanticAnalyzer.GetCurrentLocation: TSrcPos;
begin
  Result.idCtx := 0;
  Result.row := 0;
  Result.col := 0;
end;
// Utilidades
function TSemanticAnalyzer.IsInFunction: Boolean;
begin
  Result := (FCurrentProcedure <> nil) and FCurrentProcedure.IsFunction;
end;
function TSemanticAnalyzer.IsInProcedure: Boolean;
begin
  Result := (FCurrentProcedure <> nil) and not FCurrentProcedure.IsFunction;
end;
function TSemanticAnalyzer.GetCurrentFunction: TProcDecl;
begin
  if IsInFunction then
    Result := FCurrentProcedure
  else
    Result := nil;
end;
function TSemanticAnalyzer.GetCurrentProcedure: TProcDecl;
begin
  if IsInProcedure then
    Result := FCurrentProcedure
  else
    Result := nil;
end;
//Métodos principales
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
procedure TSemanticAnalyzer.SetUnitManager(AManager: TObject);
begin
  FUnitManager := AManager;
end;
//Inicialización
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
  Param.TypeName := 'ARRAY_OF_CONST';
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
  Param.TypeName := 'ARRAY_OF_CONST';
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
  Param.TypeName := 'ARRAY_OF_CONST';
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
  Param.TypeName := 'ARRAY_OF_CONST';
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
end.
