var
  x: byte;
  y: byte;
procedure proc1(x,y: byte): char;
begin
  x := 1;
  exit('1');
end; 

begin
  proc1(2,3);
  x := 5+1;
end. 





