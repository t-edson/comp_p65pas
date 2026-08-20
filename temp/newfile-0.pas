program muestra; 
//uses unit1, unit2;
type
  TMyRecord = record
    valor: Integer;
    procedure TMyRecord.IncrementarValor;
    begin
      valor := valor + 1;  // Self implícito
    end;
  end;  
begin
  writeln('PROGRAMA');

//  asm clc end;
//  y := 1;
//  y := 1 + y*2;
//  i := 0;
//  b := w;
//  y := aaa[i];
//  y := aaa.bbb[5];
//MostrarMensaje;
//  y := bbb.aaa(1);  *** Falta implementar
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



