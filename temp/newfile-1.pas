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

//procedure proc1;
//var
//   b: Byte;
//begin
//   for b:= 0 to 9 do begin
//      xx[b]:= b;
//   end;
//end;

begin
//  xbyte := 2;
//  w := xbyte;
//  w := w + 5;
//  location^ := 5;
  if w = 12 then
    xx[5] := 3;
  else
    ww[5] := 3;
  end; 
//  proc1;
//  xbyte := ord('A')-64;
//  w := @xbyte;
end.

















