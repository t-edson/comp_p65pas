{Globals definitions for the compiler P65^Pas.
                         By Tito Hinostroza 2024
}
unit CompGlobals;
{$mode ObjFPC}{$H+}
interface
uses  Classes, SysUtils, StrUtils, EpikTimer, LazLogger;

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

type  //Type categories and declaration styles
  //Type categories
  TCatType = (
    tctAtomic,  //Basic types as Byte, Word or Char.
    tctArray,   //Array of some other type.
    tctPointer, //Pointer to other type.
    tctObject   //Record with several fields
  );
  {Types categories define the way a type is structured.

  ==== ATOMIC ====
  We say a type is atomic, when it cannot be expressed as a construction of other type.
  For example: CHAR or BYTE types. WORD type should be atomic too. Although a WORD can be
  expressed as an OBJECT. Here in P65Pas we define WORD as atomic.
  Declaraction for atomic types are:
  TYPE
    mytype = byte;
    mytype2 = char;
    mytype3 = mytype;  //Because "mytype" is tomic too.

  ==== ARRAY ====
  Array of some other type (atomic or not).
  Declaration for array types are:
  TYPE
    artype = ARRAY[10] OF byte;
    otherarray = artype;  //Because artype is array
    alsoarray = ARRAY OF noAtomicType;

  As an alternative notation we can use is:
  TYPE
    artype = [10]byte;

  ==== POINTER ====
  Pointer to some other type (atomic or not).
  Declaration for pointer types are:
  TYPE
    ptrtype = POINTER TO byte;
    otherptr = ptrtype;  //Because ptrtype is pointer
    alsoptr = POINTER TO noAtomicType;

  As an alternative notation we can use is:
  TYPE
    artype = ^byte;

  }

  //Type declaration style.
  TTypDeclarStyle = (
    ttdDirect,  {Like:
                      TYPE mytype = byte;
                      TYPE mytype2 = mytype;  //"mytype" could be ARRAY/POINTER/OBJECT
                }
    ttdDeclar   {Like:
                      TYPE mytype = ARRAY[30] OF char;
                      TYPE refchar = POINTER TO char; }
  );

  //Groups of data types.
  TTypeGroup=(
    t_integer,  //Signed integer numbers
    t_uinteger, //Unsigned integer numbers
    t_float,    //Float numbers
    t_boolean,  //Booleans
    t_string,   //String of chars
    t_enum  ,   //Enumerated. { TODO : Check if needed }
    t_object    //Object (contain fields)
  );

  { TConsValue }
  {Structure to store all the possible values for a constant.
  Must have fields for all basic types defined in "TTypeGroup" and for composed
  values}
  TConsValue = object
  const
    CONS_ITEM_BLOCK = 20;
  public
    ValInt  : Int64;    //For values t_integer y t_uinteger
    ValFloat: extended; //For values t_float
    ValBool : boolean;  //For values t_boolean
    ValStr  : string;   //For values t_string
  public //Arrays
    items   : array of TConsValue;  //Ítems list
    nItems  : integer;  //Number of items
    curSize : integer;
    procedure InitItems;
    procedure AddConsItem(const c: TConsValue);
    procedure CloseItems;
  public  //Access to ValInt
    function LByte: byte; inline;  //Returns low byte of integer value.
    function HByte: byte; inline;  //Returns high byte of integer value.
    function EByte: byte; inline;
    function UByte: byte; inline;
    function valuesAsString: string;
  end;

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

{ TConsValue }
procedure TConsValue.InitItems;
begin
  nItems := 0;
  curSize := CONS_ITEM_BLOCK;   //Block size
  setlength(items, curSize);  //initial size
end;
procedure TConsValue.AddConsItem(const c: TConsValue);
begin
  items[nItems] := c;
  inc(nItems);
  if nItems >= curSize then begin
    curSize += CONS_ITEM_BLOCK;   //Increase size by block
    setlength(items, curSize);  //make space
  end;
end;
procedure TConsValue.CloseItems;
begin
  setlength(items, nItems);
end;
function TConsValue.LByte: byte;
begin
  Result := LO(word(valInt));
end;
function TConsValue.HByte: byte;
begin
  Result := HI(word(valInt));
end;
function TConsValue.EByte: byte;
begin
  Result := (valInt >> 16) and $FF;
end;
function TConsValue.UByte: byte;
begin
  Result := (valInt >> 24) and $FF;
end;
function TConsValue.valuesAsString: string;
{Returns a string containing the abstract of values stored.}
begin
  Result := 'int=' + IntToStr(ValInt) + ',bool=' + IfThen(ValBool,'T','F');
end;

initialization

  ET := TEpikTimer.Create(nil);  //Used for precision time measure
  unitPaths   := TStringList.Create;

finalization
  unitPaths.Destroy;
  ET.Destroy;
end.

