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
  Classes, SysUtils, AstPascal, ParserPas, CompGlobals, alexiaLex;

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
    FUnitPath: string;
    FState: TUnitState;
    FAST: TUnit;
    FUsedBy: TStringList;
  public
    property UnitName: string read FUnitName;
    property UnitPath: string read FUnitPath write FUnitPath;
    property State: TUnitState read FState write FState;
    property AST: TUnit read FAST write FAST;
    property UsedBy: TStringList read FUsedBy;
  public   //Inicialización.
    constructor Create(const AUnitName: string);
    destructor Destroy; override;
  end;

  TUnitManager = class
  private
    FUnits: TStringList;                 // Nombre → TCompiledUnit
    FMessageManager: TMessageManager;
    Parser: TParserPas;              //Referencia al parser
    function GetUnit(const UnitPath: string): TCompiledUnit;
    function ResolvePath(const AUnitName: string): string;
    procedure AddError(const Msg: string);
  public
    procedure LoadUnit(const AUnitName: string);
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
  FUsedBy := TStringList.Create;
  FUsedBy.Sorted := True;
  FUsedBy.Duplicates := dupIgnore;
  FUnitPath := '';
end;
destructor TCompiledUnit.Destroy;
begin
  FAST.Free;
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
end;
function TUnitManager.GetUnit(const UnitPath: string): TCompiledUnit;
{Devuelve la unidad que tiene la rura "UnitPath". Si no existe esa unidad, devuelve FALSE.}
var
  Index: Integer;
begin
  if FUnits.Find(UnitPath, Index) then
    Result := TCompiledUnit(FUnits.Objects[Index])
  else
    Result := nil;
end;
function TUnitManager.ResolvePath(const AUnitName: string): string;
{Obtiene la ruta completa del archivo de la unidad "AUnitName". Si no encuentra al archivo
en ninguna de las rutas esperadas, devuelve una cadena vacía.}
var
  Candidate, uPath: string;
begin
  //Primero busca en la carpte del archivo actual.
  uPath := ExtractFileDir(Parser.curFile) + DirectorySeparator + AUnitName  + '.pas';
  if FileExists(uPath) then begin
    Exit(uPath);
  end;
  //No encontró. Busca en los directorios de unidades
  for uPath in unitPaths do begin
    Candidate := uPath + AUnitName + '.pas';
    if FileExists(Candidate) then Exit(Candidate);
//    Candidate := uPath + AUnitName + '.pp';
//    if FileExists(Candidate) then Exit(Candidate);
  end;
  //No se encontró
  Exit('');
end;
procedure TUnitManager.LoadUnit(const AUnitName: string);
//Cargar una unidad (punto de entrada principal)
var
  CompUnit: TCompiledUnit;
  UnitPath: String;
  astUnit: TUnit;
begin
  //Resuelve la ruta del archivo de unidad.
  UnitPath := ResolvePath(AUnitName);
  if UnitPath = '' then begin
    AddError('Unidad no encontrada: ' + AUnitName);
    Exit;
  end;
  //Verifica si la unidad ya está cargada.
  CompUnit := GetUnit(UnitPath);
  if CompUnit = nil then begin
    //No existe
    CompUnit := TCompiledUnit.Create(AUnitName);
    FUnits.AddObject(UnitPath, CompUnit);
  end else begin
    //Ya existe la unidad.
    if CompUnit.State = usLoaded then begin
      //Ya está cargada, no necesita cargarse de nuevo
      Exit;   //Devuelve el AST
    end else if CompUnit.State = usLoading then begin
      //Se está analizando actualmente.
      AddError('Dependencia circular detectada: ' + AUnitName);
      Exit;
    end else if CompUnit.State = usFailed then begin
      //Se analizó pero falló.
      Exit;
    end;
  end;
  //Actualiza el registro de la unidad
  CompUnit.State := usLoading;
  CompUnit.UnitPath := UnitPath;
  //Parsea la unidad
  astUnit := Nil;
  parser.ParseUnitFile(UnitPath, astUnit);   //Puede generar errores
  if Parser.HayError then begin
    CompUnit.State := usFailed;
    astUnit.Free;
    Exit;
  end;
  CompUnit.AST := astUnit;
  ///Marcar como cargada
  CompUnit.State := usLoaded;
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
  FMessageManager := AMessageManager;
  Parser := Aparser;
end;
destructor TUnitManager.Destroy;
begin
  Clear;
  FUnits.Free;
  inherited;
end;

end.
//549
