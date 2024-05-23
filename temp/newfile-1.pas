program test;
//{$MODE PASCAL}
{$ORG $0801}
type 
  Tpos = object
    x: byte;
  end;
var
  b: byte;
  w: word;
  xx: Array[10] of Byte;
  ww: Array[10] of word;
var 
  xbyte : byte;
  location, ptr: ^byte;

procedure proc1(m: byte);
var
   b: Byte;
begin
   for b:= 0 to 9 do 
      xx[b]:= b;
   end;
end;

begin
//  xbyte := 2;
  w := xbyte;
  b  := 10;
//  w := w + 5;
//  location^ := 5;
  proc1(3);
  inc(b);
//  xbyte := ord('A')-64;
//  w := @xbyte;
//  if w = 12 then
//    xx[5] := 3;
//  else
//    ww[5] := 3;
//  end; 
//  while b<10 do
//    b := b + 1;
//  end; 
  for b:=0 to 25 do
    w := 1;
  end; 
//  repeat 
//	 b := 1;
//  until b=5;  
end.

















