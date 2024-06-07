{Globals definitions for the compiler P65^Pas.
                         By Tito Hinostroza 2024
}
unit CompGlobals;
{$mode ObjFPC}{$H+}
interface
uses  Classes, SysUtils, EpikTimer, LazLogger;

const
  NOM_PROG = 'P65Pas';   //Program name
  VER_PROG = '1.0.0-Beta';

type
  { TAdicDeclar }
  {Define aditional declaration settings for variable. Depends on target CPU architecture.
  Each compiler will support only what fit to its architecture.}
  TAdicDeclar = (
    decNone,   //Normal declaration. Will be mapped in RAM according compiler decision.
    decAbsol,  //Mapped in ABSOLUTE address
    decZeroP,  //Mapped in Zero page
    decDatSec, //Mapped at the Data section (Normal)
    decRegis,  //Mapped at Work Register (WR)
    decRegisA, //Mapped at A register
    decRegisX, //Mapped at X register
    decRegisY  //Mapped at Y register
  );

type //Globals System functions identifiers
  TSysFunID = (
  SFI_BREAK,
  SFI_CONTINUE,
  SFI_EXIT0,       //exit() with no parameters
  SFI_EXIT1,       //exit() with 1 parameter
  SFI_ORD,
  SFI_PRED,
  SFI_SUCC,
  SFI_HALT,
  SFI_CHR,
  SFI_VAL,
  SFI_ROUND,
  SFI_TRUNC,
  SFI_INC,
  SFI_DEC,
  SFI_ABS,
  SFI_ADDR,
  SFI_LOW,
  SFI_HIGH,
  SFI_BYTE,
  SFI_INT,
  SFI_STR,
  SFI_BOOLEAN,
  SFI_WORD,
  SFI_DWORD
  );


var
  //Esta propiedad tal vez deba estar junto a las demás opciones del compilador.
  unitPaths   : TStringList; //Lista de rutas donde buscar unidades.
/////////////// Campos para manejo del diccionario //////////
var
  curLanguage: string;  //identificador del lenguaje

procedure StartCountElapsed;
procedure EndCountElapsed(msg: string);

function Trans(const strEn, strEs, strQu, strDe, strUk, strRu, strFr: string): string;
//////////////////////////////////////////////////////
function NombDifArc(nomBase: String): String;
procedure AddLine(var baseStr: string; newStr: string);

implementation
var
  ET : TEpikTimer;

procedure StartCountElapsed;
begin
  ET.Clear;
  ET.Start;
end;
procedure EndCountElapsed(msg: string);
begin
  ET.Stop;
  debugln(msg + IntToStr(round(ET.Elapsed*1000))+'ms');
end;

function Trans(const strEn, strEs, strQu, strDe, strUk, strRu, strFr: string): string;
  function ClearLangId(str: string): string;
  {Limpia la cadena del caracter identificador de lenguaje, de la forma:
  #en=
  que se puede usar al inicio de una cadena.}
  begin
     if str='' then exit('');
     if length(str)<4 then exit(str);
     if (str[1] = '#') and (str[4] = '=') then begin
       delete(str, 1, 4);
       exit(str);
     end else begin
       exit(str);
     end;
  end;
begin
  case LowerCase(curLanguage) of
  'en': begin
     Result := ClearLangId(strEn);
  end;
  'es': begin
     Result := ClearLangId(strEs);
     if Result = '' then Result := ClearLangId(strEn);
  end;
  'qu': begin
     Result := ClearLangId(strQu);
     if Result = '' then Result := strEs;
  end;  //por defecto
  'de': begin
     Result := ClearLangId(strDe);
     if Result = '' then Result := ClearLangId(strEn);
  end;  //por defecto
  'uk': begin
     Result := ClearLangId(strUk);
     if Result = '' then Result := ClearLangId(strEn); //por defecto
  end;
  'ru': begin
     Result := ClearLangId(strRu);
     if Result = '' then Result := ClearLangId(strEn); //por defecto
  end;
  'fr': begin
     Result := ClearLangId(strFr);
     if Result = '' then Result := ClearLangId(strEn); //por defecto
  end;
  else  //Por defecto Inglés
    Result := ClearLangId(strEn);
  end;
end;
function NombDifArc(nomBase: String): String;
{Genera un nombre diferente de archivo, tomando el nombre dado como raiz.}
const MAX_ARCH = 10;
var i : Integer;    //Número de intentos con el nombre de archivo de salida
    cadBase : String;   //Cadena base del nombre base
    extArc: string;    //extensión

  function NombArchivo(i: integer): string;
  begin
    Result := cadBase + '-' + IntToStr(i) + extArc;
  end;

begin
   Result := nomBase;  //nombre por defecto
   extArc := ExtractFileExt(nomBase);
   if ExtractFilePath(nomBase) = '' then exit;  //protección
   //quita ruta y cambia extensión
   cadBase := ChangeFileExt(nomBase,'');
   //busca archivo libre
   for i := 0 to MAX_ARCH-1 do begin
      If not FileExists(NombArchivo(i)) then begin
        //Se encontró nombre libre
        Exit(NombArchivo(i));  //Sale con nombre
      end;
   end;
   //todos los nombres estaban ocupados. Sale con el mismo nombre
End;
procedure AddLine(var baseStr: string; newStr: string);
{Agrega una nueva línea a una cadena, verificando primero si está vacía}
begin
  if length(baseStr)=0 then baseStr := newStr
  else baseStr := baseStr + LineEnding + newStr;
end;

initialization

  ET := TEpikTimer.Create(nil);  //Used for precision time measure
  unitPaths   := TStringList.Create;

finalization
  unitPaths.Destroy;
  ET.Destroy;
end.

