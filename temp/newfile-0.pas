var
//  x: byte = 1;
  objeto: object
            a: byte;
          end;
  p: ^byte;
  w: word;
//  z: byte absolute x+1;
//procedure proc1(xpar: byte): char;
//var x: byte;
//begin
//  x := 1;
//  exit('1');
//end; 

begin
  w := @p;
//  objeto.a := 1;
  (p+3)^ := 3;
//  inc(x);
//  proc1(2+1);
//  y := 'zzz';
//  x := consName+1;
//  x := 1+2;
//  z := 12;
end. 
