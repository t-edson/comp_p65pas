uses 
  Commodore64;

var
  IRGVEC: pointer absolute $0314; 
  SCREENPOS1: byte absolute $0400;
 
procedure disableInterrupts;  {Macro?}
begin
  asm 
	  sei ; disable interrupts    
  end   
end; 

procedure enableInterrupts; {Macro?}
begin
  asm 
    cli ; enable interrupts
  end   
end; 

//(* Calling jmp from inside a procedure call borks out
procedure callDefaultInterrupt; {Macro?}
begin
  asm 
    jmp $EA31 ; jump into KERNAL's standard interrupt service routine to handle keyboard scan, cursor display etc. 	 
  end   
end; 
//*)
  
procedure myirqexample;
begin
  SCREENPOS1 := SCREENPOS1 + 1;
  asm 
    jmp $EA31 ; jump into KERNAL's standard interrupt service routine to handle keyboard scan, cursor display etc. 	 
  end  
end;  
  
procedure init;
begin
  disableInterrupts;
  IRGVEC := addr(myirqexample);
  enableInterrupts;
end;  
  
begin

  init;

  asm 
	  rts ; is not auto generated for main? 
  end 
  
end.
