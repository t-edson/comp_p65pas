{Objeto creado para la gestión de unidades. Realiza las siguiente validaciones en las
unidades:
- Evita la carga múltiple de la misma unidad.
- Ordena las unidades según sus dependencias.
- Gestiona el ciclo de vida de las unidades (carga, análisis, liberación).
- Detecta dependencias circulares .
}
unit UnitManager;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, AstPascal, ParserPas, alexiaLex;

type
  TUnitState = (
    usNotLoaded,   //La unidad aún no se ha cargado
    usLoading,     //La unidad está en proceso de carga
    usLoaded,      //La unidad está completamente cargada
    usFailed       //La unidad falló al cargarse
  );

  TCompiledUnit = class
  private
    FUnitName: string;
    FState: TUnitState;
    FAST: TUnit;
    FDependencies: TStringList;
    FUsedBy: TStringList;
    FPath: string;
    FOrder: Integer;
  public
    constructor Create(const AUnitName: string);
    destructor Destroy; override;

    property UnitName: string read FUnitName;
    property State: TUnitState read FState write FState;
    property AST: TUnit read FAST write FAST;
    property Dependencies: TStringList read FDependencies;
    property UsedBy: TStringList read FUsedBy;
    property Path: string read FPath write FPath;
    property Order: Integer read FOrder write FOrder;
  end;

  TUnitManager = class
  private
    FUnits: TStringList;                 // Nombre → TCompiledUnit
    FLoadingStack: TStringList;          // Para detectar dependencias circulares
    FSearchPaths: TStringList;           // Directorios de búsqueda
    FMainPath: string;                   // Directorio del programa principal
    FMessageManager: TMessageManager;

    Parser: TParserPas;              //Referencia al parser

    function GetUnit(const AUnitName: string): TCompiledUnit;
    function ResolvePath(const AUnitName: string): string;
    procedure CollectDependencies(const AUnitName: string);
    procedure BuildDependencyGraph;
    function TopologicalSort: TStringList;
    function HasCircularDependency: Boolean;
    procedure DetectCircularDependency(const AUnitName: string);
    procedure AddError(const Msg: string);
  public
    function LoadUnit(const AUnitName: string): TUnit;
    function LoadUnits(const AUnitNames: TStringList): TUnitList;
    procedure AddSearchPath(const APath: string);
    procedure SetMainPath(const APath: string);
    procedure Clear;
    property Units[const AUnitName: string]: TCompiledUnit read GetUnit;
  public   //Inicialización
    constructor Create(AMessageManager: TMessageManager; Aparser: TParserPas);
    destructor Destroy; override;
  end;

implementation
// TCompiledUnit
constructor TCompiledUnit.Create(const AUnitName: string);
begin
  FUnitName := AUnitName;
  FState := usNotLoaded;
  FAST := nil;
  FDependencies := TStringList.Create;
  FDependencies.Sorted := True;  // Para evitar duplicados
  FDependencies.Duplicates := dupIgnore;
  FUsedBy := TStringList.Create;
  FUsedBy.Sorted := True;
  FUsedBy.Duplicates := dupIgnore;
  FPath := '';
  FOrder := -1;
end;
destructor TCompiledUnit.Destroy;
begin
  FAST.Free;
  FDependencies.Free;
  FUsedBy.Free;
  inherited;
end;
// TUnitManager
procedure TUnitManager.Clear;
var
  i: Integer;
begin
  for i := 0 to FUnits.Count - 1 do
    FUnits.Objects[i].Free;
  FUnits.Clear;
  FLoadingStack.Clear;
end;
procedure TUnitManager.AddSearchPath(const APath: string);
begin
  FSearchPaths.Add(APath);
end;
procedure TUnitManager.SetMainPath(const APath: string);
begin
  FMainPath := APath;
end;
function TUnitManager.GetUnit(const AUnitName: string): TCompiledUnit;
//Obtener una unidad por su nombre
var
  Index: Integer;
begin
  if FUnits.Find(AUnitName, Index) then
    Result := TCompiledUnit(FUnits.Objects[Index])
  else
    Result := nil;
end;
function TUnitManager.ResolvePath(const AUnitName: string): string;
//Resuelve la ruta del archivo de la unidad.
var
  i: Integer;
  SearchPath: string;
  Candidate: string;
  CompUnit: TCompiledUnit;
begin
  // 1. Verificar si ya hay una ruta registrada
  CompUnit := GetUnit(AUnitName);
  if (CompUnit <> nil) and (CompUnit.Path <> '') then
    Exit(CompUnit.Path);

  // 2. Buscar en los directorios de búsqueda
  for i := 0 to FSearchPaths.Count - 1 do begin
    SearchPath := FSearchPaths[i];

    Candidate := IncludeTrailingPathDelimiter(SearchPath) + AUnitName + '.pas';
    if FileExists(Candidate) then
      Exit(Candidate);

    Candidate := IncludeTrailingPathDelimiter(SearchPath) + AUnitName + '.pp';
    if FileExists(Candidate) then
      Exit(Candidate);
  end;

  // 3. Buscar en el directorio del programa principal
  if FMainPath <> '' then begin
    Candidate := IncludeTrailingPathDelimiter(FMainPath) + AUnitName + '.pas';
    if FileExists(Candidate) then
      Exit(Candidate);
  end;

  // 4. No se encontró
  Result := '';
end;
procedure TUnitManager.CollectDependencies(const AUnitName: string);
// CollectDependencies - Recoger dependencias de una unidad
var
  CompUnit: TCompiledUnit;
  i: Integer;
begin
  CompUnit := GetUnit(AUnitName);
  if CompUnit = nil then
    Exit;

  // Las dependencias ya deberían estar en el AST
  if CompUnit.AST <> nil then begin
    for i := 0 to CompUnit.AST.InterfaceUses.Count - 1 do
      CompUnit.Dependencies.Add(CompUnit.AST.InterfaceUses[i].UnitName);

    for i := 0 to CompUnit.AST.ImplementationUses.Count - 1 do
      CompUnit.Dependencies.Add(CompUnit.AST.ImplementationUses[i].UnitName);
  end;
end;
// BuildDependencyGraph
procedure TUnitManager.BuildDependencyGraph;
var
  i, j: Integer;
  CompUnit: TCompiledUnit;
  DepUnit: TCompiledUnit;
  DepName: string;
  Visited: TStringList;
  RecursionStack: TStringList;

  procedure DetectCycle(const AUnitName: string; const Stack: TStringList);
  var
    idx, k: Integer;
    Cycle: string;
    CurrUnit: TCompiledUnit;
  begin
    // Verificar si ya estamos procesando esta unidad
    idx := Stack.IndexOf(AUnitName);
    if idx >= 0 then
    begin
      // Construir mensaje de ciclo
      Cycle := '';
      for k := idx to Stack.Count - 1 do
      begin
        if Cycle <> '' then
          Cycle := Cycle + ' → ';
        Cycle := Cycle + Stack[k];
      end;
      Cycle := Cycle + ' → ' + AUnitName;
      AddError('Dependencia circular detectada: ' + Cycle);
      raise Exception.Create('Circular dependency');
    end;

    // Si ya fue visitada, salir
    if Visited.IndexOf(AUnitName) >= 0 then
      Exit;

    Visited.Add(AUnitName);
    Stack.Add(AUnitName);

    // Recorrer dependencias
    CurrUnit := GetUnit(AUnitName);
    if CurrUnit <> nil then
    begin
      for k := 0 to CurrUnit.Dependencies.Count - 1 do
        DetectCycle(CurrUnit.Dependencies[k], Stack);
    end;

    Stack.Delete(Stack.Count - 1);
  end;

begin
  // 1. Limpiar las relaciones existentes
  for i := 0 to FUnits.Count - 1 do
  begin
    CompUnit := TCompiledUnit(FUnits.Objects[i]);
    CompUnit.Dependencies.Clear;
    CompUnit.UsedBy.Clear;
  end;

  // 2. Recoger dependencias de cada unidad
  for i := 0 to FUnits.Count - 1 do begin
    CompUnit := TCompiledUnit(FUnits.Objects[i]);

    if CompUnit.AST <> nil then begin
      // Dependencias de INTERFACE
      for j := 0 to CompUnit.AST.InterfaceUses.Count - 1 do begin
        DepName := CompUnit.AST.InterfaceUses[j].UnitName;
        if not CompUnit.Dependencies.IndexOf(DepName)<>-1 then begin
          CompUnit.Dependencies.Add(DepName);

          DepUnit := GetUnit(DepName);
          if DepUnit <> nil then
            DepUnit.UsedBy.Add(CompUnit.UnitName);
        end;
      end;

      // Dependencias de IMPLEMENTATION
      for j := 0 to CompUnit.AST.ImplementationUses.Count - 1 do begin
        DepName := CompUnit.AST.ImplementationUses[j].UnitName;
        if not CompUnit.Dependencies.IndexOf(DepName)<>-1 then begin
          CompUnit.Dependencies.Add(DepName);

          DepUnit := GetUnit(DepName);
          if DepUnit <> nil then
            DepUnit.UsedBy.Add(CompUnit.UnitName);
        end;
      end;
    end;
  end;

  // 3. Verificar dependencias circulares
  Visited := TStringList.Create;
  RecursionStack := TStringList.Create;
  try
    for i := 0 to FUnits.Count - 1 do begin
      CompUnit := TCompiledUnit(FUnits.Objects[i]);
      if Visited.IndexOf(CompUnit.UnitName) < 0 then
        DetectCycle(CompUnit.UnitName, RecursionStack);
    end;
  finally
    Visited.Free;
    RecursionStack.Free;
  end;

  // 4. Verificar que todas las dependencias existen
  for i := 0 to FUnits.Count - 1 do begin
    CompUnit := TCompiledUnit(FUnits.Objects[i]);
    for j := 0 to CompUnit.Dependencies.Count - 1 do begin
      DepName := CompUnit.Dependencies[j];
      if GetUnit(DepName) = nil then
        AddError('Unidad no encontrada: ' + DepName + ' (requerida por ' + CompUnit.UnitName + ')');
    end;
  end;
end;
function TUnitManager.LoadUnit(const AUnitName: string): TUnit;
// LoadUnit - Cargar una unidad (punto de entrada principal)
var
  CompUnit: TCompiledUnit;
  Path, DepName: String;
  i: Integer;
  astUnit: TUnit;
begin
  // 1. Verificar si ya está cargada
  CompUnit := GetUnit(AUnitName);
  if CompUnit <> nil then begin
    if CompUnit.State = usLoaded then begin
      //Ya está cargada, no necesita cargarse de nuevo
      Exit(CompUnit.AST)
    end else if CompUnit.State = usLoading then begin
      AddError('Dependencia circular detectada: ' + AUnitName);
      Exit(nil);
    end else if CompUnit.State = usFailed then begin
      Exit(nil);
    end;
  end;

  // 2. Resolver la ruta del archivo
  Path := ResolvePath(AUnitName);
  if Path = '' then begin
    AddError('Unidad no encontrada: ' + AUnitName);
    if CompUnit <> nil then
      CompUnit.State := usFailed;
    Exit(nil);
  end;

  // 3. Crear o actualizar el registro de la unidad
  if CompUnit = nil then begin
    CompUnit := TCompiledUnit.Create(AUnitName);
    FUnits.AddObject(AUnitName, CompUnit);
  end;

  CompUnit.State := usLoading;
  CompUnit.Path := Path;

  // 4. Marcar como en proceso (para detectar ciclos)
  FLoadingStack.Add(AUnitName);

  // 5. Parsear la unidad
  parser.ParseUnitFile(Path, astUnit);   //Puede generar errores
  if Parser.HayError then begin
    CompUnit.State := usFailed;
    FLoadingStack.Delete(FLoadingStack.IndexOf(AUnitName));
    Exit(nil);
  end else begin
    CompUnit.AST := astUnit;
  end;

  // 6. Recoger dependencias
  CollectDependencies(AUnitName);

  // 7. Cargar todas las dependencias recursivamente
  for i := 0 to CompUnit.Dependencies.Count - 1 do begin
    DepName := CompUnit.Dependencies[i];
    if LoadUnit(DepName)<>Nil then begin
      CompUnit.State := usFailed;
      FLoadingStack.Delete(FLoadingStack.IndexOf(AUnitName));
      Exit(nil);
    end;
  end;

  // 8. Marcar como cargada
  CompUnit.State := usLoaded;
  FLoadingStack.Delete(FLoadingStack.IndexOf(AUnitName));

  Result := CompUnit.AST;
end;
function TUnitManager.LoadUnits(const AUnitNames: TStringList): TUnitList;
var
  i, idx: Integer;
  untName: string;
  LoadedUnit: TUnit;
  SortedUnits, UnitMap: TStringList;
  CompUnit: TCompiledUnit;
begin
  Result := TUnitList.Create;

  // 1. Cargar cada unidad
  for i := 0 to AUnitNames.Count - 1 do
  begin
    untName := AUnitNames[i];
    LoadedUnit := LoadUnit(untName);

    if LoadedUnit <> nil then
      Result.Add(LoadedUnit)
    else
      AddError('No se pudo cargar la unidad: ' + untName);
  end;

  // 2. Si hay errores, limpiar y salir
  if FMessageManager <> nil then
    if FMessageManager.nErrors > 0 then
    begin
      Result.Free;
      Result := nil;
      Exit;
    end;

  // 3. Construir el grafo de dependencias
  BuildDependencyGraph;

  // 4. Ordenar las unidades topológicamente
  SortedUnits := TopologicalSort;
  if SortedUnits = nil then begin
    Result.Free;
    Result := nil;
    Exit;
  end;

  // 5. Reordenar el resultado según el orden topológico
  // Crear un índice de unidades por nombre
  UnitMap := TStringList.Create;
  try
    for i := 0 to Result.Count - 1 do
      UnitMap.AddObject(Result[i].unitName, Result[i]);

    Result.Clear;

    for i := 0 to SortedUnits.Count - 1 do begin
      untName := SortedUnits[i];
      idx := UnitMap.IndexOf(untName);
      if idx >= 0 then
        Result.Add(TUnit(UnitMap.Objects[idx]));
    end;

  finally
    UnitMap.Free;
    SortedUnits.Free;
  end;

  // 6. Asignar órdenes de compilación
  for i := 0 to Result.Count - 1 do begin
    CompUnit := GetUnit(Result[i].unitName);
    if CompUnit <> nil then
      CompUnit.Order := i;
  end;
end;
function TUnitManager.HasCircularDependency: Boolean;
// HasCircularDependency - Detectar dependencias circulares
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FUnits.Count - 1 do
  begin
    FLoadingStack.Clear;
    try
      DetectCircularDependency(TCompiledUnit(FUnits.Objects[i]).UnitName);
    except
      on E: Exception do
      begin
        AddError('Dependencia circular detectada en: ' +
                 TCompiledUnit(FUnits.Objects[i]).UnitName);
        Result := True;
        Break;
      end;
    end;
  end;
end;
procedure TUnitManager.DetectCircularDependency(const AUnitName: string);
// DetectCircularDependency - Algoritmo de detección de ciclos
var
  CompUnit: TCompiledUnit;
  i: Integer;
begin
  // Verificar si ya estamos procesando esta unidad
  if FLoadingStack.IndexOf(AUnitName) >= 0 then
    raise Exception.Create('Dependencia circular: ' + AUnitName);

  CompUnit := GetUnit(AUnitName);
  if CompUnit = nil then
    Exit;

  FLoadingStack.Add(AUnitName);

  for i := 0 to CompUnit.Dependencies.Count - 1 do
    DetectCircularDependency(CompUnit.Dependencies[i]);

  FLoadingStack.Delete(FLoadingStack.IndexOf(AUnitName));
end;
function TUnitManager.TopologicalSort: TStringList;
// TopologicalSort - Ordenar unidades por dependencias
var
  i: Integer;
  Visited: TStringList;
  Sorted: TStringList;

  procedure Visit(const AUnitName: string);
  var
    CompUnit: TCompiledUnit;
    Dep: string;
  begin
    if Visited.IndexOf(AUnitName) >= 0 then
      Exit;

    Visited.Add(AUnitName);
    CompUnit := GetUnit(AUnitName);

    if CompUnit <> nil then
    begin
      for Dep in CompUnit.Dependencies do
        Visit(Dep);
    end;

    Sorted.Add(AUnitName);
  end;

begin
  Visited := TStringList.Create;
  Sorted := TStringList.Create;

  try
    for i := 0 to FUnits.Count - 1 do
      Visit(FUnits[i]);

    // Invertir orden (las dependencias primero)
    Result := TStringList.Create;
    for i := Sorted.Count - 1 downto 0 do
      Result.Add(Sorted[i]);

  finally
    Visited.Free;
    Sorted.Free;
  end;
end;
procedure TUnitManager.AddError(const Msg: string);
// AddError - Añadir un error al gestor de mensajes
begin
  Parser.GenError(Msg);
end;
constructor TUnitManager.Create(AMessageManager: TMessageManager; Aparser: TParserPas);
begin
  FUnits := TStringList.Create;
  FUnits.Sorted := True;  // Para búsqueda rápida
  FLoadingStack := TStringList.Create;
  FSearchPaths := TStringList.Create;
  FMainPath := '';
  FMessageManager := AMessageManager;
  Parser := Aparser;
end;
destructor TUnitManager.Destroy;
begin
  Clear;
  FUnits.Free;
  FLoadingStack.Free;
  FSearchPaths.Free;
  inherited;
end;

end.
//549
