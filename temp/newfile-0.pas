const
  consName = 1+2; 
var
  x: byte=3+1;
  y: [3]char = 'ABC';
  z: byte absolute x;
  zz: byte absolute x;
procedure proc1(xpar: byte): char;
begin
  x := 1;
  exit('1');
end; 

begin
  inc(x);
//  proc1(2+1);
  y := 'zzz';
  x := consName+1;
end. 





