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

  { TSemanticAnalyzer }

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
    function GetFundamentalType(TypeDef: TTypeDef): TTypeDef;
    function ResolveTypeRef(TypeRef: TTypeRef): TTypeDef;
    function GetTypeOf(Expr: TExpression): TTypeDef;
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
    procedure VisitProcDecl(ProcFunctDecl: TProcFunctDecl);
    //Definiciones de tipos
    procedure VisitTypeDef(TypeDef: TTypeDef);
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
    procedure VisitFunctionCall(FuncCall: TFunctionCall);
    procedure VisitFieldAccess(FieldAccess: TFieldAccess);
    procedure VisitPointerDeref(PointerDeref: TPointerDeref);
    procedure VisitArrayIndex(ArrayIndex: TArrayRef);
    procedure VisitArrayLiteral(ArrayLit: TArrayLiteral);
    procedure VisitRecordLiteral(RecordLit: TRecordLiteral);
    procedure VisitPointerLiteral(PointerLit: TPointerLiteral);
  private  //Visitantes de nodos principales
    procedure VisitNode(Node: TASTNode);
    procedure VisitDeclarations(Decls: TASTNodeList);
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
    if Param1.TypeRef.Name <> Param2.TypeRef.Name then Exit(False);
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
      Sym.DataType := ResolveType(FieldDecl.TypeRef.Name);
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
function TSemanticAnalyzer.GetFundamentalType(TypeDef: TTypeDef): TTypeDef;
{Devuelve el tipo final de una definición de tipo. Funciona de la siguiente forma:
- Para los tipos de sistema (integer, byte, ...) devuelve el mismo tipo.
- Para las definiciones INLINE o estructurados (array of .., o record ... end) devuelve
el mismo tipo.
- En el caso de un tipo alias, debe devolver el tipo final (un tipo de sistema o una
definición INLINE).
}
var
  AliasTypeDef: TAliasTypeDef;
begin
  //Si es un alias, resolver el tipo base
  if TypeDef.NodeType = ntAliasTypeDef then begin
    AliasTypeDef := TAliasTypeDef(TypeDef);
    Result := ResolveType(AliasTypeDef.BaseType);
  end else begin
    Result := TypeDef;
  end;
end;
function TSemanticAnalyzer.ResolveTypeRef(TypeRef: TTypeRef): TTypeDef;
{Resuelve el tipo de un objeto TTypeRef en la tabla de símbolos. También actualiza el
campo "Declaration" del TTypeRef.
Si no logra resolver el tipo, devuelve NIL.}
var
  SymType: TSymbol;
begin
  if TypeRef.Name <> '' then begin
    //Busca el tipo por nombre
    SymType := FCurrentScope.LookupRecursive(UpperCase(TypeRef.Name));
    if (SymType <> nil) and (SymType.Kind = skType) then begin
      Result := SymType.DataType;
      //Si es un alias, resolvemos el tipo base
      if Result.NodeType = ntAliasTypeDef then begin
        Result := GetFundamentalType(Result);  //*** Podría optimizarse
      end;
      TypeRef.Definit := Result;
    end else begin
      //Error('Tipo desconocido: ' + TypeRef.Name, TypeRef.SrcPos);
      Result := Nil;
    end;
  end else if TypeRef.TypeDef <> nil then begin
    //El tipo es INLINE. Está creado en TypeRef.TypeDef
    Result := TypeRef.TypeDef;
    TypeRef.Definit := Result;
  end else begin   //No debería pasar
    //Error('Tipo no especificado.', TypeRef.SrcPos);
    Result := Nil;
  end;
end;
function TSemanticAnalyzer.GetTypeOf(Expr: TExpression): TTypeDef;
{Devuelve el tipo de una expresión.}
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
      if Sym <> nil then begin
        Result := Sym.DataType;
        // Enlazar la referencia a su declaración
        TVariableRef(Expr).Declaration := TVarDecl(Sym.Declaration);
      end else begin
        Result := nil;
      end;
    end;
    ntBinaryOp: begin
      //Por ahora, se usará el tipo del operando izquierdo, pero, formalmente, debe haber
      //un análisis más complejo.
      Result := GetTypeOf(TBinaryOp(Expr).Left);
    end;
    ntUnaryOp: begin
      Result := GetTypeOf(TUnaryOp(Expr).Operand);
    end;
    ntProcFunctCall: begin
      Sym := FCurrentScope.LookupRecursive(TFunctionCall(Expr).Name);
      if Sym <> nil then begin
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
    ntFieldAccess: begin
      //El tipo de un campo es el tipo declarado en el registro
      //Obtiene el tipo del registro (la parte izquierda de <Registro>.<campo>)
      FieldAccess := TFieldAccess(Expr); //Objeto TFieldAccess
      RecordVarType := GetTypeOf(FieldAccess.RecordVar);
      if RecordVarType = nil then begin
        Result := nil;
        Exit;
      end;
      //Validar que sea un RECORD
      if RecordVarType.NodeType <> ntRecordTypeDef then begin
        Result := nil;
        Exit;
      end;
      //Busca el campo en el registro
      Fields := TRecordTypeDef(RecordVarType).Fields;
      Result := nil;
      for i := 0 to Fields.Count - 1 do begin
        if Fields[i].NodeType = ntVarDecl then begin
          FieldDecl := TVarDecl(Fields[i]);
          if CompareText(FieldDecl.Name, FieldAccess.FieldName)=0 then begin
            //Encontró el campo, lee su tipo
            Result := FieldDecl.TypeRef.Definit;  //Ya debe estar actualizado
            Break;
          end;
        end;
      end;
      //Si no se encontró el campo, "Result" queda en NIL.
    end;
    ntArrayRef: begin
      //El tipo de un arreglo es el tipo de sus elementos
      ArrayVarType := GetTypeOf(TArrayRef(Expr).ArrayVar);  //Obtiene el tipo del arreglo
      if ArrayVarType = nil then Exit(nil);      //Valida que exista
      if ArrayVarType.NodeType <> ntArrayTypeDef then Exit(nil);  //Valida que sea arreglo
      ArrayType := TArrayTypeDef(ArrayVarType);    //Convierte a TArrayTypeDef
      // Resuelve el tipo de los elementos
      Result := ArrayType.ElemTypeRef.Definit;
    end;
    ntPointerLiteral:
      Result := ResolveType('POINTER');
    else
      Result := nil;
  end;
end;
function TSemanticAnalyzer.AreTypesCompatible(T1, T2: TTypeDef): Boolean;
  function GetBaseType(TypeDef: TTypeDef): TTypeDef;
  begin
    if TypeDef.NodeType = ntSubranTypeDef then begin
      Result := TSubranTypeDef(TypeDef).BaseType;
    end else begin
      Result := TypeDef;
    end;
  end;
var
  Base1, Base2: TTypeDef;
begin
  if (T1 = nil) or (T2 = nil) then
    Exit(False);

  if T1 = T2 then
    Exit(True);

  // Obtener tipos base
  Base1 := GetBaseType(T1);
  Base2 := GetBaseType(T2);
  if (Base1 = nil) or (Base2 = nil) then
    Exit(False);

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
    if (Base1.TypeName = 'CHAR') and (Base2.TypeName = 'CHAR') then
      Exit(True);
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
  TypeDef: TTypeDef;
begin
  // Verificar duplicado
  if FCurrentScope.Lookup(VarDecl.Name) <> nil then begin
    Error('Variable duplicada: ' + VarDecl.Name, VarDecl.SrcPos);
    Exit;
  end;
  //Resuelve tipo
  TypeDef := ResolveTypeRef(VarDecl.TypeRef);
  if TypeDef = Nil then
     Error('Tipo desconocido: ' + VarDecl.TypeRef.Name, VarDecl.TypeRef.SrcPos);
  //Crea símbolo
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
    TypeDef := ResolveTypeRef(ConstDecl.TypeRef);
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
  // Crear símbolo
  if ProcFunctDecl.IsFunction then
    Sym := TSymbol.Create(ProcFunctDecl.Name, skFunction)
  else
    Sym := TSymbol.Create(ProcFunctDecl.Name, skProcedure);
  Sym.Declaration := ProcFunctDecl;
  Sym.IsForward := ProcFunctDecl.IsForward;
  // Registrar parámetros (solo para validación, no se declaran en el ámbito global)
  if ProcFunctDecl.Parameters <> nil then begin
    Sym.Parameters := TASTNodeList.Create(False);   //No debe liberar los nodos parámetro, porque ya los hace el AST.
    for i := 0 to ProcFunctDecl.Parameters.Count - 1 do begin
      Param := TVarDecl(ProcFunctDecl.Parameters[i]);
      Sym.Parameters.Add(Param);
    end;
  end;
  //Tipo de retorno para funciones
  if ProcFunctDecl.IsFunction then begin
    Sym.ReturnType := ResolveTypeRef(ProcFunctDecl.ReturnTypeRef);
    if Sym.ReturnType = nil then
      Error('Tipo de retorno desconocido para: ' + ProcFunctDecl.Name, ProcFunctDecl.ReturnTypeRef.SrcPos);
  end;
  FCurrentScope.Declare(Sym);
end;
procedure TSemanticAnalyzer.RegisterTypeDecl(TypeDecl: TTypeDecl);
var
  Sym: TSymbol;
begin
  if TypeDecl.Name = '' then
    Exit; // Tipo anónimo (inline)
  // Verificar duplicado
  if FCurrentScope.Lookup(TypeDecl.Name) <> nil then begin
    Error('Tipo duplicado: ' + TypeDecl.Name, TypeDecl.SrcPos);
    Exit;
  end;
  //Registra e nombre del tipo
  Sym := TSymbol.Create(TypeDecl.Name, skType);
  Sym.DataType := TypeDecl.Definit;
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
    VarType := VarDecl.TypeRef.Definit;
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
procedure TSemanticAnalyzer.VisitProcDecl(ProcFunctDecl: TProcFunctDecl);
var
  i: Integer;
  Param: TVarDecl;
  Sym, SelfSym, FieldSym: TSymbol;
  OldProcedure: TProcFunctDecl;
  ParamType: TTypeDef;
  Field: TASTNode;
begin
  if ProcFunctDecl.IsForward then Exit;
  OldProcedure := FCurrentProcedure;
  FCurrentProcedure := ProcFunctDecl;
  EnterScope;
  try
    // Registrar parámetros en el ámbito local
    if ProcFunctDecl.Parameters <> nil then begin
      for i := 0 to ProcFunctDecl.Parameters.Count - 1 do begin
        Param := TVarDecl(ProcFunctDecl.Parameters[i]);
        // Resolver tipo del parámetro
        ParamType := ResolveTypeRef(Param.TypeRef);
        if ParamType = nil then
          Error('Tipo desconocido para parámetro: ' + Param.Name, Param.SrcPos);
        //Registra parámetro
        Sym := TSymbol.Create(Param.Name, skParameter);
        Sym.DataType := ParamType;
        Sym.Declaration := Param;
        FCurrentScope.Declare(Sym);
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
          FieldSym.DataType := ResolveType(TVarDecl(Field).TypeRef.Name);
          FieldSym.Declaration := Field;
          FCurrentScope.Declare(FieldSym);
        end;
      end;
    end;
    // Analizar declaraciones locales
    if ProcFunctDecl.Declarations <> nil then begin
      RegisterDeclarations(ProcFunctDecl.Declarations);
    end;
    // Analizar el cuerpo
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
  VisitNode(TypeDecl.Definit);
end;
//Definiciones de tipos
procedure TSemanticAnalyzer.VisitTypeDef(TypeDef: TTypeDef);
{Visita declaraciones de tipos sismples.
**** Notar la similitud con ResolveTypeDef()}
var
  BaseType: TTypeDef;
  AliasTypeDef: TAliasTypeDef;
begin
  // Ya fue registrada en RegisterTypeDecl
  // Verificar definiciones recursivas
  if TypeDef.NodeType = ntAliasTypeDef then begin
    AliasTypeDef := TAliasTypeDef(TypeDef);
    if AliasTypeDef.BaseType <> '' then begin
      BaseType := ResolveType(AliasTypeDef.BaseType);
      if BaseType = nil then
        Error('Tipo base desconocido: ' + AliasTypeDef.BaseType, TypeDef.SrcPos);
    end;
  end;
end;
procedure TSemanticAnalyzer.VisitArrayTypeDef(ArrayType: TArrayTypeDef);
var
  ElemTypeDef: TTypeDef;
  i: Integer;
  Range: TArrayRange;
begin
  //Verifica tipo de elementos
  ElemTypeDef := ResolveTypeRef(ArrayType.ElemTypeRef);
  if ElemTypeDef = nil then begin
    Error('Tipo de elemento desconocido: ' + ArrayType.ElemTypeRef.Name, ArrayType.ElemTypeRef.SrcPos);
  end else begin
    //Verificar si ElemTypeDef tiene subtipos que deben visitarse también.
    if ArrayType.ElemTypeRef.IsInline then begin
      //SOlo analiza las definiciones INLINE.
      VisitNode(ElemTypeDef);  //Analiza tipos anidados
    end;
  end;

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
  FieldTypeDef: TTypeDef;
begin
  //Analiza los campos del registro
  for astNode in RecordType.Fields do begin
    if astNode.NodeType = ntVarDecl then begin
      Field := TVarDecl(astNode);
      FieldTypeDef := ResolveTypeRef(Field.TypeRef);
      if FieldTypeDef = nil then begin
          Error('Tipo desconocido para el campo: ' + Field.Name, Field.SrcPos);
      end else begin
        if Field.TypeRef.IsInline then begin
          //SOlo analiza las definiciones INLINE.
          VisitNode(FieldTypeDef);  //Analiza tipos anidados
        end;
      end;
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
          FieldTypeDef := ResolveTypeRef(Field.TypeRef);
          if FieldTypeDef = nil then
              Error('Tipo desconocido para el campo: ' + Field.Name, Field.SrcPos);
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
  // Verificar tipo apuntado
  if PointerType.TargetTypeName <> '' then
  begin
    TargetType := ResolveType(PointerType.TargetTypeName);
    if TargetType = nil then
      Error('Tipo apuntado desconocido: ' + PointerType.TargetTypeName, PointerType.SrcPos);
  end;
end;
{$endregion}
{$region "Visitantes de sentencias"}
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
      ReturnType := Func.ReturnTypeRef.Definit;
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
    if Sym.Declaration.NodeType = ntProcFunctDecl then begin
      //Su declaración figura como procedimiento o función
      FuncCall.Declaration := TProcFunctDecl(Sym.Declaration); //Enlaza a declaración
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
        ParamType := ResolveTypeRef(Param.TypeRef);
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

  //Busca el campo en el registro
  FoundField := False;
  if RecordType.NodeType = ntRecordTypeDef then begin
    for i := 0 to TRecordTypeDef(RecordType).Fields.Count - 1 do begin
      if TRecordTypeDef(RecordType).Fields[i].NodeType = ntVarDecl then begin
        FieldDecl := TVarDecl(TRecordTypeDef(RecordType).Fields[i]);
        if FieldDecl.Name = FieldAccess.FieldName then begin
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
    if not (PtrType.NodeType = ntPointerTypeDef) then
      Error('^ solo puede aplicarse a punteros', PointerDeref.SrcPos);
  end;
end;
procedure TSemanticAnalyzer.VisitArrayIndex(ArrayIndex: TArrayRef);
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
  if not (ArrayType.NodeType = ntArrayTypeDef) then begin
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
{$endregion}
{$region "Visitantes de nodos principales}
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
    ntVarDecl: VisitVarDecl(TVarDecl(Node));
    ntConstDecl: VisitConstDecl(TConstDecl(Node));
    ntProcFunctDecl: VisitProcDecl(TProcFunctDecl(Node));
    ntTypeDecl: VisitTypeDecl(TTypeDecl(Node));

    //Definiciones de tipos
    ntSimpleTypeDef : VisitTypeDef(TTypeDef(Node));
    ntSubranTypeDef : VisitTypeDef(TTypeDef(Node));
    ntEnumTypeDef   : VisitEnumTypeDef(TEnumTypeDef(Node));
    ntArrayTypeDef  : VisitArrayTypeDef(TArrayTypeDef(Node));
    ntRecordTypeDef : VisitRecordTypeDef(TRecordTypeDef(Node));
    ntPointerTypeDef: VisitPointerTypeDef(TPointerTypeDef(Node));
    ntAliasTypeDef  : VisitTypeDef(TTypeDef(Node));
    ntProcedTypeDef : VisitTypeDef(TTypeDef(Node));

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
    ntProcFunctCall: VisitFunctionCall(TFunctionCall(Node));
    ntFieldAccess: VisitFieldAccess(TFieldAccess(Node));
    ntPointerDeref: VisitPointerDeref(TPointerDeref(Node));
    ntArrayRef: VisitArrayIndex(TArrayRef(Node));
    ntArrayLiteral: VisitArrayLiteral(TArrayLiteral(Node));
    ntRecordLiteral: VisitRecordLiteral(TRecordLiteral(Node));
    ntPointerLiteral: VisitPointerLiteral(TPointerLiteral(Node));
  end;
end;
procedure TSemanticAnalyzer.VisitDeclarations(Decls: TASTNodeList);
begin
  // Las declaraciones ya se registraron en RegisterDeclarations
  // Aquí solo se analizan los detalles adicionales
  if Decls = nil then
    Exit;

  // Analizar declaraciones de tipo
  RegisterDeclarations(Decls);
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

  {Analizar las declaraciones de tipos para registrar sus símbolos (como los valores de
  los enumerados).}
  for Node in Prog.Declarations do begin
    if Node.NodeType = ntTypeDecl then begin
        VisitNode(TTypeDecl(Node).Definit);
    end;
  end;

  //Analizar los cuerpos de los procedimientos/funciones
  for Node in Prog.Declarations do begin
    if Node.NodeType = ntProcFunctDecl then begin
      if not TProcFunctDecl(Node).IsForward then
        VisitProcDecl(TProcFunctDecl(Node));
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
        VisitProcDecl(TProcFunctDecl(Node));
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
  Param.TypeRef := TTypeRef.Create('ARRAY_OF_CONST', DummyPos);
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
  Param.TypeRef := TTypeRef.Create('ARRAY_OF_CONST', DummyPos);
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
  Param.TypeRef := TTypeRef.Create('ARRAY_OF_CONST', DummyPos);
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
  Param.TypeRef := TTypeRef.Create('ARRAY_OF_CONST', DummyPos);
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
