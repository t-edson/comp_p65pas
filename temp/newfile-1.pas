program test;
{$MODE PASCAL}
{$ORG $0801}
type 
  Tpos = object
    x: byte;
  end;
var
  xx: Array[10] of Byte;
  ww: Array[10] of word;
  w: word;
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
xx[5] := 3;
ww[5] := 3;
//  proc1;
//  xbyte := ord('A')-64;
//  location := @xbyte;
//  xbyte := 2;
//  location^ := 5;
//  w := xbyte;
//  w := w + 5;
end.

















