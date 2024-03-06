{Rutina de verificación para operaciones de comparación con datos 
de tipo WORD. }
uses Commodore64;
procedure good;
begin
  CHROUT('O');
  CHROUT('K');
//  CHROUT(chr(13));
  CHROUT(',');
end;
procedure bad;
begin
  CHROUT('E');
  CHROUT('R');
  CHROUT('R');
  CHROUT('O');
  CHROUT('R');
  CHROUT(chr(13));
end;
var
  a, b: word;
	vbyte: byte;
	vword: word;
begin
  //////////////////////////////////////////////////////////
	//////////////  Operación Igualdad ///////////////////
  //////////////////////////////////////////////////////////
	//coConst_Const
  if word(0) = word(0) then good else bad end;
  if word(0) = word(1) then bad else good end;
  if word(255) = word(255) then good else bad end;
  if 65535 = 65535 then good else bad end;

  if word(0) <> word(0) then bad else good end;
  if word(255) <> word(0) then good else bad end;
  if 65535 <> 65535 then bad else good end;

  //coConst_Variab
  //Se prueba más detalladamente, porque hay varios casos que se optimizan
  a := 10;
  if word(10) = a then good else bad end;
  if word(0) = a then bad else good end;
  if word(10) <> a then bad else good end;
  if word(255) <> a then good else bad end;
  if 256 <> a then good else bad end;
  a := 1000;
  if 1000 = a then good else bad end;
  if word(0) = a then bad else good end;
  if word(1000) <> a then bad else good end;
  if $FFFF <> a then good else bad end;
  if 256 <> a then good else bad end;
  a := 257;  //caso para optimizar
  if 257 = a then good else bad end;
  
  //coConst_Expres
  a := 10; 
  if word(11) = a+1 then good else bad end;
  a := 1000; 
  if 2000 = a+1000 then good else bad end;
  
  //coVariab_Const
  a := 10;
  if a = word(10) then good else bad end;
  a := 1000;
  if a = 1000 then good else bad end;
  
  //coVariab_Variab
  a := 0;
  b := 255;
  if a = a then good else bad end;
  if a = b then bad else good end;
  if a <> a then bad else good end;
  a := 65535;
  b := 65535;
  if a = a then good else bad end;
  if a = b then good else bad end;
  if b = a then good else bad end;
  if a <> a then bad else good end;
  a := 65535;
  b := 1;
  if a = b then bad else good end;
  if b = a then bad else good end;
  if a <> a then bad else good end;

  //coVariab_Expres
  a := 10; b := 5; 
  if a = b+5 then good else bad end;
  a := 1000; 
  if a = b+995 then good else bad end;

  //coExpres_Const
  a := 10; 
  if a+1 = word(11) then good else bad end;
  a := 1000; 
  if a+1 = 1001 then good else bad end;
  
  //coExpres_Variab 
  a := 1000; b := 5; 
  if b+995 = a then good else bad end;

	//coExpres_Expres
  a := 10; b := 5; 
  if a+1 = a+1 then good else bad end;
  if a+1 = a+2 then bad else good end;
  a := 1000; b := 5; 
  if a+1 = a+1 then good else bad end;
  if a+1 = a+2 then bad else good end;
  if a+1 = (a+1)+(b+2) then bad else good end;

  //////////////////////////////////////////////////////////
	//////////////////  Operación Mayor /////////////////////
  //////////////////////////////////////////////////////////

	//coConst_Const
  if word(0) > word(0) then bad else good end;
  if word(10) > word(9) then good else bad end;

	//coConst_Variab
  a := 0;  
  if word(0) > a then bad else good end;
  a := 1000;  
  if 1000 > a then bad else good end;
  if 1001 > a then good else bad end;

  //coConst_Expres
  a := 0;  
  if word(1) > (a+1) then bad else good end;
  a := 999;  
  if 1000 > (a+1) then bad else good end;
  if 1001 > (a+1) then good else bad end;

  //coVariab_Const
  a := 0;  
  if a > word(0) then bad else good end;
  a := 1;  
  if a > word(0) then good else bad end;
  a := 255;
  if a > word(254) then good else bad end;
  a := 1000;  
  if 1000 > a then bad else good end;
  if 1001 > a then good else bad end;

  //coVariab_Variab
  a := 0; b := 0; 
  if a > b then bad else good end;
  a := 256; b := 255; 
  if a > b then good else bad end;
  if b > a then bad else good end;
  a := 1000; b := 1000; 
  if a > b then bad else good end;
  a := 1001; b := 1000; 
  if a > b then good else bad end;

  //coVariab_Expres
  a := 1; b := 0; 
  if a > (b+1) then bad else good end;
  a := 256; b := 254; 
  if a > (b+1) then good else bad end;
  if a > (b+2) then bad else good end;
  a := 1001; b := 1000; 
  if a > (b+1) then bad else good end;
  a := 1001; b := 999; 
  if a > (b+1) then good else bad end;

  //coExpres_Const
  a := 0;  
  if (a+1) > word(1)   then bad else good end;
  if (a+2) > word(1)   then good else bad end;
  a := 999;  
  if (a+1) > 1000 then bad else good end;
  if (a+2) > 1000 then good else bad end;

  //coExpres_Variab
  a := 0; b := 1;
  if (a+1) > b   then bad else good end;
  if (a+2) > b   then good else bad end;
  a := 999; b := 1000;
  if (a+1) > b then bad else good end;
  if (a+2) > b then good else bad end;

  //coExpres_Expres
  a := 0; b := 0; 
  if (a+1) > (b+1) then bad else good end;
  a := 254; b := 254; 
  if (a+1) > (b+1) then bad else good end;
  if (a+2) > (b+1) then good else bad end;
  a := 1000; b := 1000; 
  if (a+1) > (b+2) then bad else good end;
  if (a+1) > (b+1) then bad else good end;
  if (a+2) > (b+1) then good else bad end;

  //////////////////////////////////////////////////////////
	//////////////  Operación suma ///////////////////
  //////////////////////////////////////////////////////////
	//coConst_Const
  if word(0) + word(1) = 1 then good else bad end;
  if 32768 + 1000 = 33768 then good else bad end;
	//coConst_Variab
  a := 100; 
  if 257 + a = 357 then good else bad end;
  //coConst_Expres
  a := 100; 
  if 256 + (a+1) = 357 then good else bad end;
  //coVariab_Const
  a := 100; 
  if a + 257 = 357 then good else bad end;
	//coVariab_Variab
  a := 0; b := 256;
  if a + b = 256 then good else bad end;
  a := 1000; b := 256;
  if a + b = 1256 then good else bad end;
  //coVariab_Expres
  a := 300; 
  if a + (a+1) = 601 then good else bad end;
  //coExpres_Const
  a := 100; 
  if (a+1) + 256 = 357 then good else bad end;
  //coExpres_Variab
  a := 300; b := 1 ;
  if (a+1) + b = 302 then good else bad end;
  a := 100; b := 500; 
  if a + b + 5 = 605 then good else bad end;
	//coExpres_Expres
  a := 100; b := 500; 
  if (a+500) + (b + 500) = 1600 then good else bad end;

  //////////////////////////////////////////////////////////
	//////////////  Operación suma word-byte ///////////////////
  //////////////////////////////////////////////////////////

	//coConst_Const
  if word(0) + 1 = 1 then good else bad end;
  if 65534 + 1 = 65535 then good else bad end;
	//coConst_Variab
  vbyte := 100; 
  if 257 + vbyte = 357 then good else bad end;
  //coConst_Expres
  vbyte := 100; 
  if 256 + (vbyte+1) = 357 then good else bad end;
  //coVariab_Const
  a := 100; 
  if a + 25 = word(125) then good else bad end;
	//coVariab_Variab
  a := 0; vbyte := 1;
  if a + vbyte = word(1) then good else bad end;
  a := 1000; vbyte := 1;
  if a + vbyte = 1001 then good else bad end;
  //coVariab_Expres
  a := 300; vbyte := 5; 
  if a + (vbyte+1) = 306 then good else bad end;
  //coExpres_Const
  a := 100; 
  if (a+1) + 5 = word(106) then good else bad end;
  //coExpres_Variab
  a := 300; vbyte := 1 ;
  if (a+1) + vbyte = 302 then good else bad end;
	//coExpres_Expres
  a := 100; vbyte := 100; 
  if (a+500) + (vbyte + 1) = 701 then good else bad end;

  //////////////////////////////////////////////////////////
	//////////////  Operación suma byte-word ///////////////////
  //////////////////////////////////////////////////////////

	//coConst_Const
  if 1 + word(0) = 1 then good else bad end;
  if 1 + 65534  = 65535 then good else bad end;
	//coConst_Variab
  vword := 257; 
  if 100 + vword = 357 then good else bad end;
  //coConst_Expres
  vword := 256; 
  if 100 + (vword + 1) = 357 then good else bad end;
  //coVariab_Const
  vbyte := 100; 
  if vbyte + 350 = 450 then good else bad end;
	//coVariab_Variab
  vbyte := 0; vword := 1;
  if vbyte + vword = word(1) then good else bad end;
  vbyte := 1; vword := 1000;
  if vbyte + vword = 1001 then good else bad end;
  //coVariab_Expres
  vbyte := 5; vword := 300; 
  if vbyte + (vword+1) = 306 then good else bad end;
  //coExpres_Const
  vbyte := 100; 
  if (vbyte+1) + 500 = 601 then good else bad end;
  //coExpres_Variab
  vbyte := 1; vword := 300;
  if (vbyte+1) + vword = 302 then good else bad end;
	//coExpres_Expres
  vbyte := 100; vword := 100; 
  if (vbyte+100) + (vword + 500) = 800 then good else bad end;

  //////////////////////////////////////////////////////////
	//////////////  Resta word-word ///////////////////
  //////////////////////////////////////////////////////////
	//coConst_Const
  if word(2) - word(1) = 1 then good else bad end;
  if word(1000) - word(500) = 500 then good else bad end;
  //coConst_Variab
  a := 2; 
  if word(10) - a = word(8) then good else bad end;
  if word(1000) - a = 998 then good else bad end;
  //coConst_Expres
  if word(10) - (a+1) = word(7) then good else bad end;
  if word(1000) - (498 + a) = 500 then good else bad end;
  //coVariab_Const
  if a - word(1) = word(1) then good else bad end;
  a := 1000;
  if a - 300 = 700 then good else bad end;
	//coVariab_Variab
  a := 2; b := 1;
  if a - b = word(1) then good else bad end;
  a := 1000; b := 999;
  if a - b = word(1) then good else bad end;
  a := 1000; b := 0;
  if a - b = 1000 then good else bad end;
  //coVariab_Expres  
  if a - (b+1) = 999 then good else bad end;
  //coExpres_Const  
  if (a+1000) - 1250 = 750 then good else bad end;
  //coExpres_Variab  
  if (a+1000) - b = 2000 then good else bad end;
  //coExpres_Expres  
  if (a+1000) - (b+1000) = 1000 then good else bad end;

  //////////////////////////////////////////////////////////
	//////////////  Multip word-word->word ///////////////////
  //////////////////////////////////////////////////////////
//	//coConst_Const
//  if word(2) umulword word(1) = word(2) then good else bad end;
//  if word(100) umulword 500 = 50000 then good else bad end;
  
  asm RTS end; 
end.
