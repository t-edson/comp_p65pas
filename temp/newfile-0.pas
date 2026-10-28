program muestra; 
const
  MAXIMO = 100;
type
  //TConjunto = set of byte;
  //TColores = (rojo, azul, amarillo, verde, morado);
  //TSubrango = azul..verde;
//  TIndice = 1..MAXIMO;
//  TDia = (Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo);
//  TTabla = array['a'..'z'] of integer;
  TTipoValor =  (vtEntero, vtReal, vtString, vtBooleano);

  TValor = record
    case tipo: TTipoValor of
      vtEntero: (valor_entero: integer);
      vtReal: (valor_real: real);
  end;

var
  Valores: TValor;
  Valor: TTipoValor;
begin
    Valores.tipo := vtEntero;
//  i := MAXIMO;
//  con := [1, 2, 3];
//  col := Rojo;
//arr3[0]:=3;
//  writeln('PROGRAMA');
//  asm clc end;
//  y := bbb.aaa(1);  *** Falta implementar
//  empleado.Evaluaciones[1] := 5;
//  x := -1 + 2;
//  aaa(12) := 1;
//  x := (2 + 1) and 123;
//  a := b[1][1];
//  x[i][2] := 1;
//  x := fun(1);
//  a[i] := b[j];
end. 



