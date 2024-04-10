{Unidad que agrega campos necesarios a la clase TCompilerBase, para la generación de
código con el PIC16F.}
unit GenCodBas_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, CPUCore, P65c02utils, CompBase, ParserDirec, CompGlobals,
  AstElemP65, LexPas, MirList, StrUtils, LazLogger;
const
  STACK_SIZE = 8;      //tamaño de pila para subrutinas en el PIC
  MAX_REGS_AUX_BYTE = 6;   //cantidad máxima de registros a usar
  MAX_REGS_AUX_BIT = 4;    //cantidad máxima de registros bit a usar
  MAX_REGS_STACK_BYTE = 8; //cantidad máxima de registros a usar en la pila
  MAX_REGS_STACK_BIT = 4;  //cantidad máxima de registros a usar en la pila

type
  { TGenCodBas }
  TGenCodBas = class(TParserDirecBase)
  private

  public     //Initialization
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TGenCodPic }
//Register and temporal variables requirement.



//Inicialización
constructor TGenCodBas.Create;
var
  srcPosNull: TSrcPos;
begin
  inherited Create;
  ID := 16;  //Identifica al compilador PIC16
end;
destructor TGenCodBas.Destroy;
begin
  inherited Destroy;
end;

end.
//1873
