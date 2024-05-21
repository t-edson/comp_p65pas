const
  SPRITE1 = [$00,$00,$00,$00,$fe,$00,$03,$ff];
var 
  sprPucp: [8]byte;
begin
  sprPucp := SPRITE1;  //Copy date from screen
  asm 
	rts 
  end; 
end.
