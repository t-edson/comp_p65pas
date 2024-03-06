//P65Pas adaptation of code for Kickc compiler. 
//Original: https://www.youtube.com/watch?v=CWlCidFRz3I
uses Commodore64;
type
  TPtrByte = ^byte;
var 
  SCREEN    : [1000]byte absolute $0400; 
  BITMAP    : [1]byte    absolute $2000;  //Size doesn't matter
  BORDERCOL : byte       absolute $d020;
  D011      : byte       absolute $d011;
  VIC_MEMORY: byte       absolute $d018;

const 
  VIC_BMM =  %00100000;
  VIC_DEN =  %00010000;
  VIC_RSEL = %00001000; 
  BLUE = 6;
var
  bitmask: []byte = [128, 64, 32, 16, 8, 4, 2, 1];

procedure plot(x, y: word);
var 
  location: ^byte;
  tmp: word;
  tmp2: byte;
begin
  location := @BITMAP;
  location += x and $fff8;
  location += y.low and 7;
  //location += (y >> 3) * 320;
  tmp := y >> 3;
  location += tmp << 8;
  location += tmp << 6;
  //location^ = location^ or bitmask[x and 7];
  tmp2 := bitmask[x and 7];
  location^ := location^ or tmp2;
end;

procedure line_xdyi(x, y, x1, xd, yd: word);
var 
  e: word;
begin
  e := yd >> 1;
  repeat
      plot(x,y);
      inc(x);
      e :=  e + yd;
      if xd<e then
          inc(y);
          e := e - xd;
      end;
  until x = x1+1;
end;

procedure line_xdyd(x, y, x1, xd, yd: word);
var
  e: word;
begin
  e := yd>>1;
  repeat
      plot(x,y);
      inc(x);
      e := e + yd;
      if xd < e then
          dec(y);
          e := e - xd;
      end;
  until x = x1+1;
end;

procedure line_ydxi(y, x, y1, yd, xd: word);
var
  e: word;
begin
  e := xd>>1;
  repeat
      plot(x,y);
      inc(y);
      e := e + xd;
      if yd < e then
          inc(x);
          e := e - yd;
      end;
  until y = y1+1;
end;

procedure line_ydxd(y, x, y1, yd, xd: word);
var
  e: word;
begin
  e := xd>>1;
  repeat
      plot(x,y);
      inc(y);
      e := e + xd;
      if yd < e then
          dec(x);
          e := e - yd;
      end;
  until y = y1+1;
end;

// Draw a line on the bitmap
procedure line(x0, y0, x1, y1: word);
var
  xd, yd: word;
begin
    if(x0<x1) then
        xd := x1-x0;
        if y0<y1 then
            yd := y1-y0;
            if yd<xd then
                line_xdyi(x0, y0, x1, xd, yd);
            else
                line_ydxi(y0, x0, y1, yd, xd);
            end;
        else
            yd := y0-y1;
            if yd<xd then
                line_xdyd(x0, y0, x1, xd, yd);
            else
                line_ydxd(y1, x1, y0, yd, xd);
            end;
        end;
    else 
        xd := x0-x1;
        if y0<y1 then
            yd := y1-y0;
            if yd<xd then
                line_xdyd(x1, y1, x0, xd, yd);
            else
                line_ydxd(y0, x0, y1, yd, xd);
            end;
        else
            yd := y0-y1;
            if yd<xd then
                line_xdyi(x1, y1, x0, xd, yd);
            else
                line_ydxi(y1, x1, y0, yd, xd);
            end;
        end;
    end;
end;

// Fill some memory with a value
procedure fill(start: TPtrByte; size: word; val: byte);
var 
   end_, addr: TPtrByte;
begin
    end_ := start + size;
    for addr := start to end_ do
        addr^ := val;
    end;
end;

var 
  add1, add2: TPtrByte;
//  ad1, ad2: word;
  x: byte;
const
  ad1 = $0400;
  ad2 = $2000;
begin
    add1 := @SCREEN;
    fill(add1, 40*25,$16);
    add2 := @BITMAP;
    fill(add2, 1000*8,0);
    BORDERCOL := BLUE;
    D011 := VIC_BMM or VIC_DEN or VIC_RSEL or 3;
//    VIC_MEMORY := byte( ((@SCREEN and $3fff)>>6) or ((@BITMAP and $3fff)>>10) );
    VIC_MEMORY := $10 or $08;
    x := 0;
    while x <= 100 do 
        line(word(150)-x, word(0+x) >> 1, word(50+x), word(50));
        line(word(225)-x << 1,word(0+x),word(25+x) << 1 , word(100));
        line(300-(x*3),word(0+x)+(x >> 1),0+(x * 3) , word(150));
        x += 4;
    end;
    repeat until false;
end.
// Compiled in 64 msec. average.
