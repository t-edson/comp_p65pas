{ ==================================
 Demo program - Graphics (c64)
  Created: 12/2/2022
  Artwork: Errazking, 2014
  URL:     https://csdb.dk/release/?id=148957
   ================================== }
program HateAndWar_1914;
uses Commodore64;

var
  ScreenControl1  : byte absolute $D011;
  ScreenControl2  : byte absolute $D016;
  MemorySetup     : byte absolute $D018;
  BorderColor     : byte absolute $D020;
  BackgroundColor : byte absolute $D021;
  
  Characters : array of byte = ({$BIN2CSV 1914.dat});
  Colors     : array of byte = ({$BIN2CSV 1914.col});
  Dummy      : array[4130] of byte;                   // makes Pixels starts at $2000
  Pixels     : array of byte = ({$BIN2CSV 1914.bin}); // no need to copy
  
  vram_Char  : array[1000] of byte absolute $400;
  vram_Color : array[1000] of byte absolute $D800;
  
  i: word;
  
begin
  BorderColor     := $00;
  BackgroundColor := $00;
  
  for i:= 0 to 999 do
    vram_Char[i] := Characters[i];
    vram_Color[i] := Colors[i];
  end;
  
  ScreenControl1  := $3B;  // enable bitmap graphics mode
  ScreenControl2  := $18;  // enable multicolor mode
  MemorySetup     := $18;  // bitmap memory starts at $2000

  repeat  
  until 1=0;  // keeps the screen intact
  
  Dummy[0] := Pixels[0];  // to stop optimizer from removing the arrays 
end.
