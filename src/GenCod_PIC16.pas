{
Implementación del Generador de Código del compilador.
Esta implementación no permitirá recursividad, por las limitaciones de recursos de los
dispositivos más pequeños.
El compilador está orientado a uso de registros (solo hay uno) y memoria RAM. No se
manejan estructuras en pila.
Solo se manejan datos de tipo boolean, byte y word, y operaciones sencillas.
}
{La arquitectura definida aquí contempla:

Un registro de trabajo A, de 8 bits (el acumulador del PIC).
Dos registros auxiliares X e Y.
Tres registros de trabajo adicionales  U,E y H de 8 bits cada uno (Creados a demanda).

La forma de trabajo por tipos es:

TIPO BOOLEAN:
* Se almacenan en un byte. Cualquier valor diferente de cero se considera TRUE.
* Los resultados se devuelven en el bit Z, del registro SR.
TIPO CHAR Y BYTE:
* Se almacenan en un byte.
* Los resultados se devuelven en el registro acumulador A
TIPO WORD:
* Se almacenan en un 2 bytes.
* Los resultados se devuelven en los registros (H,A).

Opcionalmente, si estos registros ya están ocupados, se guardan primero en la pila, o se
usan otros registros auxiliares.

Despues de ejecutar alguna operación booleana que devuelva una expresión, se
actualizan las banderas: BooleanBit y BooleanInverted, que implican que:
* Si BooleanInverted es TRUE, significa que la lógica de C o Z está invertida.
* La bandera BooleanBit, indica si el resultado se deja en C o Z.

Por normas de Xpres, se debe considerar que:
* Todas las BOR reciben sus dos parámetros en las variables p1^ y p2^.
* El resultado de cualquier expresión se debe dejar indicado en el objeto "res".

Si el objeto "res" es constante, almacena directamente sus valores en:
* "valInt" para tipos enteros y enteros sin signo.
* "valBool" para el tipo booleano.
* "valStr" para el tipo string.

Las rutinas de operación, deben devolver su resultado en "res".
Para mayor información, consultar la doc. técnica.
 }
unit GenCod_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,
  P65c02utils, CPUCore, GenCodBas_PIC16,
  CompBase, CompGlobals, AstElemP65, LexPas, MirList;
type
    { TGenCod }
    TGenCod = class(TGenCodBas)
    private
    protected
      procedure Cod_StartProgram;
      procedure Cod_EndProgram;

    end;

implementation
var
  sifByteMulByte, sifByteDivByte, sifByteModByte: TEleFunDec;
  sifWordDivWord, sifWordModWord, sifWordShlByte: TEleFunDec;
function GetAssignTarget(fun: TEleExpress; out target: TEleExpress): boolean;
var
  setFunct: TEleExpress;
begin
  setFunct := TEleExpress(fun.Parent);
  if setFunct = nil then exit(false);
  if setFunct.opType <> otFunct then exit(false);
  if setFunct.fundec.getset <> gsSetInSimple then exit(false);
  target := TEleExpress(setFunct.elements[0]);  //Parameter C := A + B
  exit(true);
end;
////////////rutinas obligatorias
procedure TGenCod.Cod_StartProgram;
//Codifica la parte inicial del programa
begin
  //Code('.CODE');   //inicia la sección de código
end;
procedure TGenCod.Cod_EndProgram;
//Codifica la parte final del programa
begin
  //Code('END');   //inicia la sección de código
end;




end.

//8560
