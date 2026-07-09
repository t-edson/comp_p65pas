{Opciones del compilador. Aquí se incluyen las propiedades del compilador que se pueden
cambiar por línea de comandos o por directivas. La mayoría de estas propiedades son
relativas al hardware.
Se definen en una clase independiente para poder pasar la referencia a estas propiedades
a otros objetos adicionales del compilador y mejorar así la modularidad.}
unit CompOptions;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, CompGlobals;
type
  TCpuMode = (cpu6502, cpu65C02);

  { TCompOptions }
  TCompOptions = class
  private
    parsList: TStringList;     //Lista para leer parámetros.
  public   //File options
    mainFile    : string;    //Archivo inicial que se compila.
    hexFile     : string;    //Nombre de archivo de salida.
    function hexFilePath: string;
    procedure generateHexFileName;
    function mainFilePath: string;
    function ExpandRelPathToMain(FileName: string): string;
    procedure setHexFile(newHexFile: string);
  public   //Syntax Options.
    syntaxMode  : (modPascal, modPicPas);
    enabDirMsgs : boolean;   //Bandera para permitir generar mensajes desde las directivas.
    function modeStr: string;
  public   //Compiling Options. Set by directives.
    comp_level  : TCompileLevel;//Compilation level.
    GeneralORG  : integer;      //Dirección general de origen de código
    bootloader  : TBootloader;  //Bootloader code for the compiled binary.
    loaderBytes : array of integer; //Custom Bootloader bytes.
    str_nullterm: boolean;      //Flag to activate the Null-terminated string for literals.
    ForToRepeat : boolean;      //Convert FOR loop to REPEAT loop.
  public   //CPU parameters. Estas propiedades deberán leerse en la síntesis.
    cpuMode  : TCpuMode;
    Model    : string;       //Modelo de PIC
    frequen  : integer;      //Frecuencia del reloj
    MaxFreq  : integer;      //Máxima frecuencia del reloj en Hz.
    iRam     : integer;      //Puntero a la memoria RAM, para escribir cuando se ensambla o compila código.
  public   //Optimization options
    OptReuProVar: boolean;   //Optimiza reutilizando variables locales de procedimientos.
    OptRetProc  : boolean;   //Optimiza el último exit de los procedimientos.
    RemUnOpcod  : boolean;   //Removes unnecessary ASM instructions generated.
  public   //Assembler options
    asmOutType  : byte;      //Assembler ouput style: 0->Normal Assembler. 1->BASIC Poke's
    asmIncComm  : boolean;   //Includes Comments in ASM text
    //incDetComm  : boolean;   //Incluir Comentarios detallados.
    IncVarDec   : boolean;   //Includes variables information section.
    ExcUnused   : boolean;   //Excludes unused variables in variable section.
    IncVarName  : boolean;   //Includes variables name in ASM operands.
    IncAddress  : boolean;   //Includes address before ASM instructions.
  public
    hcCommands  : TStringList;  //Lista para comandos de configuración de hardware.
  public   //Incialización
    procedure SetDefault;
    procedure ReadParameters(const pars: string);
    constructor Create;
    destructor Destroy; override;
  end;

implementation
{$region "Files"}
function TCompOptions.hexFilePath: string;
begin
  Result := ExpandRelPathTo(mainFile, hexfile); //Convierte a ruta absoluta
end;
procedure TCompOptions.generateHexFileName;
{Genera el nombre del archivo de salida a partir del nombre del programa}
begin
  hexfile  := ChangeFileExt(mainFile, '.prg');     //Obtiene nombre
  hexfile  := hexFilePath;   //Expande nombre si es necesario
end;
function TCompOptions.mainFilePath: string;
begin
  Result := mainFile;
end;
function TCompOptions.ExpandRelPathToMain(FileName: string): string;
{Convert a relative path to absolute path, considering the base path is "mainFile".}
begin
  Result := ExpandRelPathTo(mainFile, FileName);
end;
procedure TCompOptions.setHexFile(newHexFile: string);
var
  filPath: String;
begin
  filPath := ExpandRelPathTo(mainFile, newHexFile);  //Completa ruta, si es relativa
  hexfile := filPath;
end;
{$endregion}
function TCompOptions.modeStr: string;
begin
  case syntaxMode of
  modPascal: Result := 'modPascal';
  modPicPas: Result := 'modPicPas';
  else
    Result := 'Unknown';
  end;
end;
procedure TCompOptions.SetDefault;
{FIja las opciones por defecto para el compilador}
begin
  comp_level  := clComplete;
  ForToRepeat := true;
  enabDirMsgs := true;
  OptReuProVar:= false;   //Optimiza reutilizando variables locales de procedimientos.
  OptRetProc  := false;   //Optimiza el último exit de los procedimientos.
  RemUnOpcod  := false;

  asmOutType  := 0;  //Normal Assembler
  asmIncComm  := false;
  IncVarDec   := false;
  IncVarName  := false;
  IncAddress  := false;
  //Default settings for Directive settings.
  syntaxMode  := modPicPas;   //Por defecto en sintaxis nueva
  bootloader  := bldJMP;

  str_nullterm:= false;
  cpuMode     := cpu6502;
end;

procedure TCompOptions.ReadParameters(const pars: string);
{Lee los parámetros de línea de comandos de "pars" y configura las opciones del
compilador.}
var
  txt, tmp: string;
begin
  //Default settings for Command line Options
  SetDefault;
  //Load parameters in a list
  parsList.Text := trim(pars);
  unitPaths.Clear;
  for txt in parsList do begin
    if length(txt)<2 then continue;
    if          copy(txt,1,2) = '-C' then begin  //---Compiling options
      case txt of
      //Compiler level
      '-Cn' : comp_level := clNull;
      '-Ca' : comp_level := clAnalys;
      '-Cao': comp_level := clAnalOptim;
      '-C'  : comp_level := clComplete;
      //Compiler settings
      '-Cf' : ForToRepeat := false;
      end;
    end else if copy(txt,1,2) = '-O' then begin  //---Optimization options
      case txt of
      '-Ov' : OptReuProVar := true;
      '-Or' : OptRetProc   := true;
      '-Ou' : RemUnOpcod   := true;
      end;
    end else if copy(txt,1,2) = '-A' then begin  //---Assembler options
      case txt of
      '-A0': asmOutType := 0;    //Output in normal Assembler.
      '-A1': asmOutType := 1;    //Output in BASIC POKE's loader.
      '-Ac': asmIncComm := true; //Include commnents in ASM output.
      '-Av': IncVarDec  := true; //Include variables information section.
      '-Au': ExcUnused  := true; //Exclude unused variables in variable section.
      '-An': incVarName := true; //Include nombres de variables en las instrucciones.
      '-Aa': IncAddress := true; //Include memory address in instructions.
      end;
    end else if copy(txt,1,2) = '-F' then begin  //File names and paths
      if copy(txt,1,3) = '-Fu' then begin  //Add unit path
        tmp := copy(txt,4,length(txt));
        if tmp='' then continue;
        if tmp[1]='"' then delete(tmp,1,1);
        if tmp[length(tmp)]='"' then delete(tmp,length(tmp),1);
        unitPaths.Add(tmp);
      end else if copy(txt,1,3) = '-Fo' then begin  //Set output file
        tmp := copy(txt,4,length(txt));
        if tmp='' then continue;
        if tmp[1]='"' then delete(tmp,1,1);
        if tmp[length(tmp)]='"' then delete(tmp,length(tmp),1);
        setHexFile(tmp);
      end;
    end else if txt = '-Dn' then begin  //Disable directive messages
      enabDirMsgs := false;
    end else begin         //Other.

    end;
  end;
end;
constructor TCompOptions.Create;
begin
  parsList := TStringList.Create;
  hcCommands:= TStringList.Create;
end;
destructor TCompOptions.Destroy;
begin
  hcCommands.Destroy;
  parsList.Destroy;
  inherited Destroy;
end;

end.

