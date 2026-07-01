program muestra; 
type
  typeName = array[1..2, 0..5,9..23] of char;
  aaa = ^byte;
procedure Sumar(a, b: integer; var resultado: integer);
var a,b,c: byte;
begin
  resultado := a + b;
end;
  
begin
  a := @aaa;
  if a > b then aaa 
  else bbb;
//  case a of 
//  'a': hola;
//  1: aaa;
//  1,2,3: hola;
//  aaa: asdsadsa;
//  aa..bb: asdas;
//  end;
//  y := aaa.bbb[5];
//  y := bbb.aaa(1);  *** Falta implementar
//  y := aaa[i];
//  y := fun(i);

//  empleado.Evaluaciones[1] := 5;
//  x := -1 + 2;
//  aaa(12) := 1;
//  x := (2 + 1) and 123;
//  a := b[1][1];
//  x[i][2] := 1;
//  x := fun(1);
//  a[i] := b[j];
//  a := 'abc';
end. 



