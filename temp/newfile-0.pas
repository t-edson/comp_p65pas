type
  TPtrByte = ^byte;
var 
  a3: array[3] of byte;
  x: byte;
  p: ^byte;
  w, ad1: word;
begin
//  w := @x;
//  x := 1+5;
//  x := a3[2];
//  x := a3[x+1];
//  p^:= 3;
  a3[2] := 5;
  w := w mod 1000;
end.

