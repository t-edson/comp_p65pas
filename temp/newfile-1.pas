//uses Commodore64;
var
  var1: byte;
const //Atomic types
  //Simple constant
  CONS1 = 2;      
  //Constant with expression
  CONS2 = 2+3;    
  //Constant from constant
  CONS3 = CONS1;  
  //Constant with expression including contant
  CONS4 = CONS1+5;  
  //Constant with expression including contant
  CONS5 = CONS1+5+3; 
  //Constant from variable address
  CONS6 = @var1;
  //Constant from variable address and expression
  CONS7 = 5+@var1+3;
const //Constant with types
  CT1 : byte = 123;
  CT2 : word = word(123);
  CT3 = true;  //Implicit type
  CT4 : boolean = false;
const //Structured types
  CARRAY1 = [$00,$7e,$00,$03];
  CARRAY2 = [CONS1,CONS6+2,$03];
//  CARRAY3 = [CONS1,@var1,CONS6+2,$03];
  
var 
//  SCREEN    : [1000]byte absolute $0400; 
//  BITMAP    : [1]byte    absolute $2000;  //Size doesn't matter
//  VIC_MEMORY: byte       absolute $d018;
//  w: word;  
  x: byte;
  w: word;
begin
//    VIC_MEMORY := byte( ((@SCREEN and $3fff)>>6) or ((@BITMAP and $3fff)>>10) );
//    VIC_MEMORY := $10 or $08;
//    w := ((@SCREEN and $3fff)>>6);
  x := CONS1;
  x := CONS2;
  x := CONS3;
  x := CONS4;
  x := CONS5;
  w := CONS6;
  w := CONS7;
  w := @x;
end.
