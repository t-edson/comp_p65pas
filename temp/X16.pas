{ =============================
  Pascal for Commander X16
  Produces .prg file with BASIC stub for autostart
  ==============================}

{$PROCESSOR CPU65C02}
{$ORG $0801}   // Initial address - same as BASIC
//{$BOOTLOADER C64} // Generate the BASIC code: 10 SYS 2062
{$BOOTLOADER $0C, $08, $0A, $00, $9E, 'COD_5A', $00, $00, $00}

{$CLEAR_STATE_RAM} // If we clears, we'll need to define all RAM map
{$SET_STATE_RAM '0000-9EFF:GPR'} // Memory that we can use
{$SET_STATE_RAM '0000-0021:SFR'} // zp for Kernel 
{$SET_STATE_RAM '0080-00FF:SFR'} // zp for Basic
{$SET_STATE_RAM '0100-01FF:SFR'} // Stack
{$SET_STATE_RAM '0200-03FF:SFR'} // Kernel, Basic
{$SET_DATA_ADDR '0022-007F'}  // Some bytes from Zero page. 


unit X16;

interface

const
  // C64 Legacy Kernel functions
  __ScInit = $FF81;
  __IoInit = $FF84;
  __RamTas = $FF87;
  __Restor = $FF8A;
  __Vector = $FF8D;
  __SetMsg = $FF90;
  __LstnSA = $FF93;
  __TalkSA = $FF96;
  __MemBot = $FF99;
  __MemTop = $FF9C;
  __ScnKey = $FF9F;
  __SetTmo = $FFA2;
  __IecIn  = $FFA5;
  __IECOut = $FFA8;
  __UnTalk = $FFAB;
  __UnLstn = $FFAE;
  __Listen = $FFB1;
  __Talk   = $FFB4;
  __ReadST = $FFB7;
  __SetLfs = $FFBA;
  __SetNam = $FFBD;
  __Open   = $FFC0;
  __Close  = $FFC3;
  __ChkIn  = $FFC6;
  __ChkOut = $FFC9;
  __ClrChn = $FFCC;
  __ChrIn  = $FFCF;
  __ChrOut = $FFD2;
  __Load   = $FFD5;
  __Save   = $FFD8;
  __SetTim = $FFDB;
  __RdTim  = $FFDE;
  __Stop   = $FFE1;
  __GetIn  = $FFE4;
  __ClAll  = $FFE7;
  __UdTim  = $FFEA;
  __Screen = $FFED;
  __Plot   = $FFF0;
  __IoBase = $FFF3;
  
  // X16 Kernel functions
  KBDBUF_Peek                = $FEBD;
  KBDBUF_Get_Modifiers       = $FEC0;
  KBDBUF_Put                 = $FEC3;
  I2C_Read_Byte              = $FEC6;
  I2C_Write_Byte             = $FEC9;
  MONITOR                    = $FECC;
  ENTROPY_GET                = $FECF;
  KEYMAP                     = $FED2;
  CONSOLE_Set_Paging_Message = $FED5;
  CONSOLE_Put_Image          = $FED8;
  CONSOLE_Init               = $FEDB;
  CONSOLE_Put_Char           = $FEDE;
  CONSOLE_Get_Char           = $FEE1;
  MEMORY_FILL                = $FEE4;
  MEMORY_COPY                = $FEE7;
  MEMORY_CRC                 = $FEEA;
  MEMORY_DECOMPRESS          = $FEED;
  SPRITE_Set_Image           = $FEF0;
  SPRITE_Set_Position        = $FEF3;
  FB_init                    = $FEF6;  // FB = Frame Buffer
  FB_get_info                = $FEF9;
  FB_set_palette             = $FEFC;
  FB_cursor_position         = $FEFF;
  FB_cursor_next_line        = $FF02;
  FB_get_pixel               = $FF05;
  FB_get_pixels              = $FF08;
  FB_set_pixel               = $FF0B;
  FB_set_pixels              = $FF0E;
  FB_set_8_pixels            = $FF11;
  FB_set_8_pixels_opaque     = $FF14;
  FB_fill_pixels             = $FF17;
  FB_filter_pixels           = $FF1A;
  FB_move_pixels             = $FF1D;
  GRAPH_Init                 = $FF20;
  GRAPH_clear                = $FF23;
  GRAPH_set_window           = $FF26;
  GRAPH_set_colors           = $FF29;
  GRAPH_draw_line            = $FF2C;
  GRAPH_draw_rect            = $FF2F;
  GRAPH_move_rect            = $FF32;
  GRAPH_draw_oval            = $FF35;
  GRAPH_draw_image           = $FF38;
  GRAPH_set_font             = $FF3B;
  GRAPH_get_char_size        = $FF3E;
  GRAPH_put_char             = $FF41;
  MACPTR                     = $FF44; // Read multiple bytes from the peripheral bus
  ENTER_BASIC                = $FF47;
  CLOSE_ALL                  = $FF4A;
  CLOCK_SET_DATE_TIME        = $FF4D;
  CLOCK_GET_DATE_TIME        = $FF50;
  JOYSTICK_SCAN              = $FF53;
  JOYSTICK_GET               = $FF56;
  LKUPLA                     = $FF59;
  LKUPSA                     = $FF5C;
  SCREEN_Mode                = $FF5F;
  SCREEN_Set_Charset         = $FF62;
  PFKEY                      = $FF65;
  MOUSE_CONFIG               = $FF68;
  MOUSE_GET			             = $FF6B;
  JSRFAR                     = $FF6E;
  MOUSE_SCAN                 = $FF71;
  FETCH                      = $FF74;
  STASH                      = $FF77;
  PRIMM                      = $FF7D;

  YM_Reg            = $9F40;
  YM_Data           = $9F41;

  // I2C Devices
  I2C_DEVICE_SMC    = $42;  // System Management Controller
  I2C_DEVICE_RTC    = $6F;  // Real Time Clock
  
  // Emulator Registers
  GIF_ctrl          = $9FB5;

  // ROM/RAM Banks
  KERNAL_ROM_BANK   = 0;
  KEYBOARD_ROM_BANK = 1;
  CBDOS_ROM_BANK    = 2;
  GEOS_ROM_BANK     = 3;
  BASIC_ROM_BANK    = 4;
  MONITOR_ROM_BANK  = 5;
  CHARSET_ROM_BANK  = 6;
  KERNAL_RAM_BANK   = 0;

  // Banked Addresses
  RAM_WIN           = $A000;
  RAM_WIN_SIZE      = $2000;
  ROM_WIN           = $C000;

  // Disk I/O Device constant
  DEVICE_SD         = 1;
  DEVICE_HOST       = 8;
  DEVICE_DISK       = DEVICE_HOST;
  
  // BASIC Vectors
  BASIC_PANIC       = $C000;
  BASIC_INIT        = $C003;
  
  // Interrupt Vectors
  IRQ_VECTOR        = $0314;
  BRK_VECTOR        = $0316;
  NMI_VECTOR        = $0318;
  FET_VECTOR        = $03AF;

  // Routine Vectors
  IOPEN_VECTOR      = $031A;
  ICLOSE_VECTOR     = $031C;
  ICHKIN_VECTOR     = $031E;
  ICKOUT_VECTOR     = $0320;
  ICLRCH_VECTOR     = $0322;
  IBASIN_VECTOR     = $0324;
  IBSOUT_VECTOR     = $0326;
  ISTOP_VECTOR      = $0328;
  IGETIN_VECTOR     = $032A;
  ICLALL_VECTOR     = $032C;
  ILOAD_VECTOR      = $0330;
  ISAVE_VECTOR      = $0332;
  
var
  RAM_BANK: byte absolute $00;
  ROM_BANK: byte absolute $01;

  // Kernel registers, used by kernel functions
  r0:  word absolute $02;
  r1:  word absolute $04;
  r2:  word absolute $06;
  r3:  word absolute $08;
  r4:  word absolute $0A;
  r5:  word absolute $0C;
  r6:  word absolute $0E;
  r7:  word absolute $10;
  r8:  word absolute $12;
  r9:  word absolute $14;
  r10: word absolute $16;
  r11: word absolute $18;
  r12: word absolute $1A;
  r13: word absolute $1C;
  r14: word absolute $1E;
  r15: word absolute $20;

  
// Usefull Pascal Warper functions
procedure LoadFile(FileName: word; NameLen: byte; Address: word);          // Loads in RAM
procedure vLoad(FileName: word; NameLen: byte; Bank: byte; Address: word); // Loads in VRAM
procedure bLoad(FileName: word; NameLen: byte; Bank: byte);                // Loads in Banked RAM

procedure ShowMouse;
procedure HideMouse;

  
// Usefull Macros  
{$DEFINE WAIT=asm JSR __ChrIn end;}

{$DEFINE SEI=asm SEI end;}
{$DEFINE CLI=asm CLI end;}
{$DEFINE RTS=asm RTS end;}


type 
  pointer = word;
  
  procedure Locate(x: byte  registerX; y: byte  registerY);
  
  //////////// KERNAL FUNCTIONS //////////
 
  //Initialize VIC; restore default input/output to keyboard/screen; clear screen; set PAL/NTSC switch and interrupt timer.
  //procedure SCINIT;
  //Initialize CIA's, SID volume; setup memory configuration; set and start interrupt timer.
  //procedure IOINIT;
  //Clear Screen
  //procedure CLRSCR(col: byte);
  //Clear Screen
  procedure CLRSCR;
  //Read byte from default input (for keyboard, read a line from the screen). (If not keyboard, must call OPEN and CHKIN beforehands.)
  procedure CHRIN: char;
  //Write byte to default output. (If not screen, must call OPEN and CHKOUT beforehands.)
  procedure CHROUT(c: char registerA);
  //Read byte from default input. 
  procedure GETIN: byte;
  // Set the software timer
  procedure SETTIM(Low:byte registerA; Middle: byte registerX; High: byte registerY);
  // Get the software timer in AXY: Low, Mid, High
  procedure RDTIM;
  
  //////////// BASIC FUNCTIONS //////////
  //Output a word Number in ASCII Decimal Digits
  //procedure LINPRT(n:word);
  //procedure LINPRT(n:byte);
  //procedure STROUT(str: pointer);
  //procedure RANDOM: byte;

  /////////// DIRECT ACCESS TO SCREEN ////////
  //procedure PutChar(x: byte; y: byte registerY; c: char): word;
  
  procedure SetNam(NameLength: byte registerA; NameAddrLo: byte registerX; NameAddrHi: byte registerY);
  procedure SetLFS(LogicalNumber: byte registerA; DeviceNumber: byte registerX; SecondAddress: byte registerY);
  procedure Load(Oper: byte registerA; AddressLo: byte registerX; AddressHi: byte registerY);

implementation

procedure SetNam(NameLength: byte registerA; NameAddrLo: byte registerX; NameAddrHi: byte registerY);
begin
  asm 
    JSR __SetNam
  end 
end; 

procedure SetLFS(LogicalNumber: byte registerA; DeviceNumber: byte registerX; SecondAddress: byte registerY);
begin
  asm 
    JSR __SetLFS
  end 
end; 

procedure Load(Oper: byte registerA; AddressLo: byte registerX; AddressHi: byte registerY);
begin
  asm 
    JSR __Load
  end 
end; 



{
  procedure SCINIT;
  begin
    asm
    JSR $FF81
    end
  end; 
  
  procedure IOINIT;
  begin
    asm 
    JSR $FF84  
    end 
  end; 
  procedure CLRSCR(col: byte);
  begin
    asm
    LDA col
    STA $0286
    JSR $E544
    end
  end;
  }
  procedure SETTIM(Low:byte registerA; Middle: byte registerX; High: byte registerY);
  begin
    asm 
      JSR __SETTIM 
    end; 
  end;
  
  procedure RDTIM; 
  begin
    asm 
      JSR __RDTIM 
    end; 
  end;
  
  procedure CLRSCR;
  begin
    asm
      JSR __ClrChn
      JSR __ScInit;
    end
  end;
  
  procedure CHROUT(c: char registerA);
  begin
    asm 
      JSR __ChrOut  ;argument already in A register
    end 
  end; 
  
  procedure Locate(x: byte registerY; y: byte registerX);
  begin
    asm 
      CLC
      JSR __Plot  
    end 
  end; 
  
  
  
  procedure CHRIN: char;
  begin
    asm
      JSR __ChrIn  ;return char in A register
    end
  end; 
   
  procedure GETIN: byte;
  {Read byte from default input. (If not keyboard, must call OPEN and CHKIN beforehands.)
  Input: <none>
  Output: A = Byte read.}
  begin
    asm 
      JSR __GetIn 
    end 
  end; 
  
(*  procedure LINPRT(n:word);
  {Print the word number specified. 
  This routine, first convert the number to Floating Point Notation,
  so it's some slow.}
  begin
    asm
    LDA n.high
    LDX n.low
    JSR $BDCD 	 
    end 
  end; 
  procedure LINPRT(n:byte);
  {Print the byte number specified. 
  This routine, first convert the number to Floating Point Notation,
  so it's some slow.}
  begin
    asm
    LDA #0
    LDX n
    JSR $BDCD 	 
    end 
  end; 
  procedure STROUT(str: pointer);
  {Prints a string delimited by NULL}
  begin
    asm 
    LDA str.low
    LDY str.high
    JSR $AB1E 
    end 
  end; 
  procedure RANDOM: byte;
  {Returns a pseudo-random byte.}
  begin
    asm 
      LDA #0  ;Use internal clcck
      JSR $E09A
      LDA $64 
    end 
  end; 
  procedure PutChar(x: byte; y: byte registerY; c: char): word;
  begin
    asm 
       ;--- Load Y in (H,A)
       LDA #0
       STA __H
       TYA
       ;--- Shift (H,A) 3 times 
       ASL
       ROL __H
       ASL
       ROL __H
       ASL
       ROL __H
       ;--- Save in IX (IX <- Y * 8) 
       STA __IX.low
       LDY  __H
       STY __IX.high
       ;--- Shift (H,A) 2 times: (H,A) <- y*32 
       ASL
       ROL __H
       ASL
       ROL __H
       ;--- Add (H,A) to IX: IX <- IX + y*32 + 400
       CLC
       ADC __IX.low  ;LSB
       STA __IX.low
       LDA __H      ;MSB
       ADC __IX.high
       CLC
       ADC #$04
       STA __IX.high
       ;--- Here we have IX <- Y*40 + $400. 
       LDY x
       LDA c
       CLC  ;Prepara Index Y address mode
       STA (__IX), Y  ; Write in IX + X
    end
  end; 
  *)
  
procedure LoadFile(FileName: word; NameLen: byte; Address: word);
begin
  SetNam(NameLen, FileName.low, FileName.high);
  SetLFS(1, 8, 0);
  Load(0, Address.low, Address.high);
end;

procedure vLoad(FileName: word; NameLen: byte; Bank: byte; Address: word);
begin
  SetNam(NameLen, FileName.low, FileName.high);
  SetLFS(1, 8, 0);  // Use 1 for r34!!!
  Load(Bank + 2, Address.low, Address.high);
end;

procedure bLoad(FileName: word; NameLen: byte; Bank: byte);
begin
  RAM_BANK := Bank;  // flip to desired Bank
  SetNam(NameLen, FileName.low, FileName.high);
  SetLFS(1, 8, 0);  // Use 1 for r34!!!
  Load(0, RAM_WIN.low, RAM_WIN.high);
end;

  

procedure ShowMouse;
begin
  asm 
      LDA #1
      JSR MOUSE_CONFIG 
  end 
end;
 
procedure HideMouse;
begin
  asm 
      LDA #0
      JSR MOUSE_CONFIG 
  end 
end;

  
end.
