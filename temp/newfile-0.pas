var
  a: byte;
begin
//  a := 10;
  if a = 0 then a:=0 end;
  asm
  lda #1 
	rts 
  end; 
end.
