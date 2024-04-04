//type
//  TPtrByte = ^byte;
//var 
//  SCREEN    : [1000]byte absolute $0400; 
//  BITMAP    : [1]byte    absolute $2000;  //Size doesn't matter
//  BORDERCOL : byte       absolute $d020;
//  VIC_MEMORY: byte       absolute $d018;
//
const 
  CONS2 = 2+3;    
  CARRAY2 = [1,1+2,$03];
//  VIC_BMM =  %00100000;
//  VIC_DEN =  %00010000;
//  VIC_RSEL = %00001000; 
//  BLUE = 6;
//
// Draw a line on the bitmap
//procedure line(x0, y0, x1, y1: word);
//var
//  xd, yd: word;
//begin
//  xd := x1-x0;
//  if y0<y1 then
//    yd := y1-y0;
//    if yd<xd then
//      if xd<yd>>1 then
//          inc(xd);
//          yd := yd - xd;
//      end;
//    else
//      yd := 0;
//    end;
//  end;
//end;

var 
  a3: array[3] of byte;
  x: byte;
  p: ^byte;
  w: word;
begin
//  w := @x;
  x := 1+5;
//  x := a3[2];
//  x := a3[x+1];
//  p^:= 3;
//  a3[2] := 5;
//  _getitem(@a3, 0, 1)
//    x += 4;
//    add2 := @BITMAP+1;
//    VIC_MEMORY := $10 or $08;
//    if ad1 < word(5) then
//      VIC_MEMORY := 1;
//    elsif false then
//      x := 0;
//    elsif ad1 = 0 then
//      x := 5;
//    else
//      x := 50;
//    end;
//inc(x);
//    while x <= 100 do 
////        line(word(150)-x, word(0+x) >> 1, word(50+x), word(50));
////        line(word(225)-x << 1,word(0+x),word(25+x) << 1 , word(100));
////        line(300-(x*3),word(0+x)+(x >> 1),0+(x * 3) , word(150));
//        x += 4;
//    end;
//    repeat until false;
end.
// Compiled in 64 msec. average.
