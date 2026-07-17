program muestra; 

procedure IncrementarConASM(var valor: integer); assembler;
asm
  lda valor      ; Cargar valor en acumulador
  clc            ; Limpiar carry
  adc #1         ; Sumar 1
  sta valor      ; Guardar resultado
end;

const 
  PUNTO_ORIGEN: record
    x: integer;
    y: integer;
    z: integer;
  end = (x: 0; y: 0; z: 0);
var a: byte;
    b,c: array[0..2] of byte;
//procedure MostrarMensaje; forward;
begin
//  a := 'aaa';
//  case a of 
//  'a': hola;
//  1: aaa;
//  y := aaa.bbb[5];
//  y := bbb.aaa(1);  *** Falta implementar
//  y := aaa[i];
//  empleado.Evaluaciones[1] := 5;
//  x := -1 + 2;
//  aaa(12) := 1;
//  x := (2 + 1) and 123;
//  a := b[1][1];
//  x[i][2] := 1;
//  x := fun(1);
//  a[i] := b[j];
//  a := 'abc';
   aaa('aaa');
end. 



