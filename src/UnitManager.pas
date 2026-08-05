{Objeto creado para la gestión de unidades. Realiza las siguiente validaciones en las
unidades:
- Evita la carga múltiple de la misma unidad.
- Ordena las unidades según sus dependencias.
- Gestiona el ciclo de vida de las unidades (carga, análisis, liberación).
- Detecta dependencias circulares.
Por Tito Hinostroza 2026.
}
unit UnitManager;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, AstPascal, ParserPas, CompGlobals, LazLogger, alexiaLex;

type
  //Estados que puede tener una unidad durante el ciclo de compilación.
  TUnitState = (
    usNotLoaded,   //La unidad aún no se ha cargado. Estado temporal. Prescindible.
    usLoading,     //La unidad está en proceso de carga
    usLoaded,      //La unidad está completamente cargada
    usFailed       //La unidad falló al cargarse
  );
  {Objeto que modela a una unidad analizada ("parseada") o en proceso de análisis
  ("parseo").}
  TCompiledUnit = class
  private
    FUnitName: string;
    FUnitPath: string;
    FState   : TUnitState;
    FAST     : TUnit;
    FOrder   : Integer;
  public
    property UnitName: string read FUnitName;
    property UnitPath: string read FUnitPath write FUnitPath;
    property State: TUnitState read FState write FState;
    property AST: TUnit read FAST write FAST;
    property Order: Integer read FOrder write FOrder;  //Orden para la compilación
  public   //Inicialización.
    constructor Create(const AUnitName: string);
    destructor Destroy; override;
  end;
  {Gestor de las unidades. Es el punto de dentrada para agregar las unidades y
  gestionarlas. Su lista Units guarda la referencia a todas las unidades leídas.
  El modo de trabajo consiste ir agregando, una a una, las unidades que se listan en la
  sección USES de un programa o unidad. Previamente se debe llamar a Clear() para
  preparar el proceso.}
  TUnitManager = class
  private
    FUnits: TStringList;             //Lista de TCompiledUnit
    FMessageManager: TMessageManager;
    Parser: TParserPas;              //Referencia al parser
    OrderIdx: Integer;               //Contador para el orden.
    function GetUnit(const UnitPath: string): TCompiledUnit;
    function ResolvePath(const AUnitName: string): string;
  public
    property Units: TStringList read FUnits;
    procedure LoadUnit(const AUnitName: string);
  public   //Inicialización
    procedure Clear;
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
  FUnitPath := '';
end;
destructor TCompiledUnit.Destroy;
begin
  FAST.Free;
  inherited;
end;
// TUnitManager
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
    Parser.GenError('Unidad no encontrada: ' + AUnitName);
    Exit;
  end;
  //Verifica si la unidad ya está cargada.
  CompUnit := GetUnit(UnitPath);
  if CompUnit = nil then begin
    //No existe. Crea la nueva unidad.
    CompUnit := TCompiledUnit.Create(AUnitName);
    CompUnit.State := usLoading;
    CompUnit.UnitPath := UnitPath;
    //Agrega la unidad en "FUnits".
    {Se agrega antes de parsear para que se puedan detectar las dependencias circulares}
    FUnits.AddObject(UnitPath, CompUnit);
    //Parsea la unidad.
    astUnit := Nil;
    parser.ParseUnitFile(UnitPath, astUnit);  //Pueden generarse llamadas recursivas a LoadUnit().
    //Valida el error.
    if parser.HayError then begin
      CompUnit.State := usFailed;
      astUnit.Free;
      Exit;
    end;
    CompUnit.AST := astUnit;     //Actualiza el AST
    CompUnit.Order := OrderIdx;  //Actualiza orden de compilación
    Inc(OrderIdx);
    ///Marcar como cargada
    CompUnit.State := usLoaded;
  end else begin
    //Ya existe la unidad.
    if CompUnit.State = usLoaded then begin
      //Ya está cargada, no necesita cargarse de nuevo
      Exit;   //Devuelve el AST
    end else if CompUnit.State = usLoading then begin
      //Se está analizando actualmente.
      Parser.GenError('Dependencia circular detectada: ' + AUnitName);
      Exit;
    end else if CompUnit.State = usFailed then begin
      //Se analizó pero falló.
      Exit;
    end;
  end;
end;
//Inicialización
procedure TUnitManager.Clear;
var
  i: Integer;
begin
  for i := 0 to FUnits.Count - 1 do
    FUnits.Objects[i].Free;
  FUnits.Clear;
  OrderIdx := 1;
end;
constructor TUnitManager.Create(AMessageManager: TMessageManager; Aparser: TParserPas);
begin
  //Lista de unidades
  FUnits := TStringList.Create;
  FUnits.Sorted := True;  // Para búsqueda rápida
  FMessageManager := AMessageManager;
  //Guarda referencia al parser
  Parser := Aparser;
end;
destructor TUnitManager.Destroy;
begin
  Clear;
  FUnits.Free;
  inherited;
end;

end.

