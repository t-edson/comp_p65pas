program muestra; 
var a: byte absolute aaa;
    b,c: array[0..2] of byte;
//procedure MostrarMensaje; forward;
{$MSGBOX "He\tllo"}
begin
  a := #$41'''def';
  a := 'aaa'#65;
  a := 'aaa'#32'aaa';  
  asm clc end;
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
//   aaa('aaa');
end. 



