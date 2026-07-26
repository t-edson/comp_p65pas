{Unit for parse ASM blocks of P65Pas compiler.
Functionality of parser is defined in the class TParserAsm_6502.
The input for this unit is the ASM code accessed trought the lexer of the compiler.
The output of the lexer is a new node element created in the syntax tree.
By Tito Hinostroza 03/09/2020.
}
unit ParserASM_6502;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, alexiaLex, CompGlobals, P65C02utils, ASTunit,
  LazLogger;
type //Identifcador de tokens
  TASMTokenIdent = (
    txOTHER    ,  //Not identified.
    //Keywords
    txEND      ,  //Keyword "END"
    //Symbols
    //Operators
    txATSYMBOL ,  //Operator "@"
    txPAREN_OP ,  //Symbol "("
    txPAREN_CL ,  //Symbol ")"
    //Operators
    txDOT      ,  //Operator "."
    //Literals
    txLITNUMBER,  //Literal numérico como: 0123
    //Others
    txCOMMENT,    //Comentarios
    txIDENTIF     //Identificadores
  );

type
  { TParserAsm6502 }
  TParserAsm6502 = class
  private
    lex     : TAleLexer;       //Reference to the lexer
    tokIdent: TASMTokenIdent;     //Identificador de token
    msg     : TMessageManager; //Referencia al gestor de mensajes
    labels  : TAsmInstructionList;   //Lista de etiquetas
    curBlock: TAsmBlock;       //Bloque ASM actual.
    curInst : TAsmInstruction;    //Instruction ASM actual.
    function HayError: boolean; inline;
  private  //Mensajes
    procedure GenWarn(txt: string);
    procedure GenError(txt: string);
    procedure GenError(txt: string; const srcPos: TSrcPos);
  private
    procedure AddDirectiveDB;
    procedure AddDirectiveDW;
    procedure AddInstructionLabel(lblName: string; local: Boolean);
    function CaptureOperand(var operand: TAsmOperand): boolean;
    function CaptureParenthes: boolean;
    procedure EndASM;
    function GetFaddressByte(addr: integer): byte;
    function IsLabelDeclared(txt: string; out lblEle: TAsmInstruction): boolean;
    procedure ParseInstructionLabel(local: Boolean);
    procedure ProcASMline;
    procedure ProcInstrASM(idInst: TP6502Inst);
    procedure StartASM;
    procedure AddInstruction(const inst: TP6502Inst; addMode: TP6502AddMode;
      param: integer; const srcDec: TSrcPos);
    procedure AddDirectiveORG(param: word);
  public //Inicialización
    procedure ProcessASMblock(Body: TBlock);
    function DecodeNext: boolean;
    constructor Create(msg0: TMessageManager; lex0: TAleLexer);
    destructor Destroy; override;
  end;

implementation

resourcestring
  ER_EXP_COL_LAB_= 'Expected ":" for label: ';
  ER_EXPEC_PAREN = 'Expected ")"';
  ER_EXP_ADR_VAR = 'Expected address or variable name.';
  ER_EXP_CON_VAL = 'Expected constant or value.';
  ER_NOGETADD_VAR= 'Cannot get address of this Variable';
  ER_NOGETVAL_CON= 'Cannot get value of this constant';
  ER_INV_ASMCODE = 'Invalid ASM Opcode: %s';
  ER_EXPECT_W_F  = 'Expected "w" or "f".';
  ER_SYNTAX_ERR_ = 'Syntax error: "%s"';
  ER_EXPE_NUMBIT = 'Expected number of bit: 0..7.';
  ER_EXPECT_ADDR = 'Expected address.';
  ER_IDENT_EXPEC = 'Identifier expected.';
  WA_ADDR_TRUNC  = 'Address truncated to fit instruction.';

// Mensajes
function TParserAsm6502.HayError: boolean;
begin
  exit(msg.nErrors>0);
end;
procedure TParserAsm6502.GenWarn(txt: string);
{Genera un mensaje de Advertencia, en la posición actual del contexto. }
begin
  msg.warn(lex.GetMsgInfo(txt));
end;
procedure TParserAsm6502.GenError(txt: string);
{Genera un mensaje de error en la posición actual a la posición del contexto actual.}
begin
  msg.error(lex.GetMsgInfoE(txt));
end;
procedure TParserAsm6502.GenError(txt: string; const srcPos: TSrcPos);
{Genera un mensaje de error en la posición indicada.}
begin
  msg.error(lex.GetMsgInfoE(txt, srcPos));
end;

function TParserAsm6502.GetFaddressByte(addr: integer): byte;
{Obtiene una dirección de registro para una isntrucción ASM, truncando, si es necesario,
los bits adicionales.}
begin
  if addr>255 then begin
    addr := addr and $7F;
    //Indica con advertencia
    GenWarn(WA_ADDR_TRUNC);
  end;
  Result := addr;
end;
function TParserAsm6502.IsLabelDeclared(txt: string; out
  lblEle: TAsmInstruction): boolean;
{Indica si un nombre es una etiqueta. Si lo es, devuelve TRUE, y devuelve en lblEle, la
referencia a la instrucción de la etiqueta.}
var
  lbl: TAsmInstruction;
begin
  //No se espera procesar muchas etiquetas
  for lbl in labels do begin  { TODO : ¿No se podría prescindir de "labels2 y usar solamente la lista de todas las instrucciones? }
    if lbl.uname = upcase(txt) then begin
      lblEle := lbl;
      exit(true);
    end;
  end;
  //No encontró
  exit(false);
end;
function TParserAsm6502.CaptureParenthes: boolean;
{Captura el paréntesis ')'. Si no encuentra devuelve error}
begin
  lex.SkipWhitesNoEOL;
  if tokIdent = txPAREN_CL then begin
    lex.Next;   //toma la coma
    exit(true);
  end else begin
    GenError(ER_EXPEC_PAREN);
    exit(false);
  end;
end;
function TParserAsm6502.CaptureOperand(var operand: TAsmOperand): boolean;
{Capture the operand (value, label or address) of an ASM instruction, including
operations if exist.
 Operands can have one of the following formats:
   [ > | < ] "$" [.HIGH | .LOW | @0 | @1 | @2 | @3 | <+|-><numeric literal>]
   [ > | < ] <numeric literal>
   [ > | < ] <identifier> [.HIGH | .LOW | @0 | @1 | @2 | @3 | <+|-><numeric literal>]
If not operand has found, error is generated and returns FALSE.}
  function ScanOperation(out operation: TAsmOperator; out value: word): boolean;
  {Look for one operations, in the current context. Operatiosn valids are:
        .HIGH
        .LOW
        @0
        @1
        @2
        @3
        + <VALUE>
        - <VALUE>.
  If one operations is found:
     * Retunns operation and value in parameters.
     * Returns TRUE in the function.
  If not operations are found, returns FALSE.}
  var
    valueInt: Longint;
  begin
    lex.SkipWhitesNoEOL;
    if (lex.tokType = tkEol) or (tokIdent = txCOMMENT) then begin
      //End of line
      exit(false);
    end;
    if tokIdent = txDOT then begin        //"."
      //Hay precisión de campo
      lex.Next;
      if UpCase(lex.token) = 'LOW' then begin
        operation := aopSelByte;
        value := 0;
        lex.Next;
        exit(true);
      end else if UpCase(lex.token) = 'HIGH' then begin
        operation := aopSelByte;
        value := 1;
        lex.Next;
        exit(true);
      end else begin
        GenError('Field expected after "."');
        exit(false);
      end;
    end else if tokIdent = txATSYMBOL then begin  //"@"
      lex.Next;
      if lex.token = '0' then begin
        operation := aopSelByte;
        value := 0;
        lex.Next;
        exit(true);
      end else if lex.token = '1' then begin
        operation := aopSelByte;
        value := 1;
        lex.Next;
        exit(true);
      end else if lex.token = '2' then begin
        operation := aopSelByte;
        value := 2;
        lex.Next;
        exit(true);
      end else if lex.token = '3' then begin
        operation := aopSelByte;
        value := 3;
        lex.Next;
        exit(true);
      end else begin
        GenError('Field expected after "@"');
        exit(false);
      end;
    end else if (lex.token = '+') or (lex.token = '-') then begin
      if lex.token='+' then operation := aopAddValue else operation := aopSubValue;
      //Get operand
      lex.Next;
      lex.SkipWhitesNoEOL;
      if (lex.tokType = tkEol) or (lex.token = ';') then begin
        //End of line
        GenError('Operand expected');
        exit(false);
      end else begin
        //Follows something
        if not TryStrToInt(lex.token, valueInt) then begin
          GenError('Numeric operand expected');
          exit(false);
        end;
        lex.Next;
        value := word(valueInt);
        exit(true);
      end;
    end else begin
      //Other token
      exit(false);
    end;
  end;
  procedure ScanOperations(firstOperation: char);
  {Scan in the current line for ASM operations. If operations are found, they will be
  added as nodes in the current node of the AST.
  "firstOperation" allows to indicate if a position operator, like '>' or '<' has been
  found before de parameter.}
  var
    operation: TAsmOperator;
    value: word;
  begin
    if firstOperation='>' then begin
      //There is an operation
      operand.AddOperation(aopSelByte, 1);
    end else if firstOperation='<' then begin
      //There is an operation
      operand.AddOperation(aopSelByte, 0);
    end;
    while ScanOperation(operation, value) do begin
      //There is an operation
      operand.AddOperation(operation, value);
    end;
  end;
  function TestForPositionOperand: char;
  {Test if a position operand ('>' or '<') exist. If so return the operator,
  otherwise returns ' '.}
  begin
    if lex.token = '>' then begin
      lex.Next;
      exit('>');
    end else if lex.token = '<' then begin
      lex.Next;
      exit('<');
    end else begin
      //Other
      exit(' ');
    end;
  end;
var
  positOper: char;
  lblEle: TAsmInstruction;
begin
  Result := false;
  operand.used := false;
  lex.SkipWhitesNoEOL;
  positOper := TestForPositionOperand();  //Check for ">" or "<"
  if lex.token = '$' then begin
    //Es una dirección relativa
    lex.Next;
    lex.SkipWhitesNoEOL;
    //Creates node "Operand".
    operand.Val := -2;  //To indicates it's $
    //Check for operations
    ScanOperations(positOper);
    if HayError then exit(false);
    operand.used := true;
    exit(true);
  end else if lex.tokType = tkLitNumber then begin
    //Es una dirección numérica
    operand.Val := StrToInt(lex.token);  //Simple number
    lex.Next;
    operand.used := true;
    exit(true);
  end else if lex.tokType = tkIdentifier then begin
    //Es un identificador o una etiqueta. Puede definirse luego.
    operand.Val := -1;        //Indicates to use "operRef"
    operand.Ref := nil;       //Will be later linked.
    operand.Nam := UpCase(lex.token);  //Keep name to find reference.
    lex.Next;
    //Check for operations
    ScanOperations(positOper);
    if HayError then exit(false);
    operand.used := true;
    exit(true);
  end else if tokIdent = txATSYMBOL then begin     //"@"
    //Debe ser la referencia a una etiqueta local: @salto1
    lex.Next;
    if tokIdent <> txIDENTIF then begin
       GenError(ER_IDENT_EXPEC);
       Exit(False);
    end;
    operand.Val := -1;        //Indicates to use "operRef"
    operand.Ref := nil;       //Will be later linked.
    operand.Nam := UpCase(lex.token);  //Keep name to find reference.
    lex.Next;
    //Check for operations
    ScanOperations(positOper);
    if HayError then exit(false);
    operand.used := true;
    exit(true);
  end else begin
    GenError(ER_EXP_CON_VAL);
    exit(false);
  end;
end;
procedure TParserAsm6502.StartASM; //Inicia el procesamiento de código ASM
begin
  labels.Clear;   //limpia etiquetas
end;
procedure TParserAsm6502.EndASM;  //Termina el procesamiento de código ASM
  function CompleteUndefJump(var operand: TAsmOperand): boolean;
  {Completa la instrucción "unsInstruct", buscando en la lista de etiquetas.
  Si no encuentra la etiqueta, devuelve FALSE.}
  var
    lblInstr: TAsmInstruction;
  begin
    for lblInstr in labels do begin  //Ve si la etiqueta existe
      if operand.Nam  = lblInstr.uname  then begin
        //Sí existe la etiqueta.
        operand.Ref := lblInstr;  //Actualiza la referencia a la etiqueta.
        //parser.AddCallerToFromCurr(lblInstr);  //Agrega referencia
        exit(true);  //Encontrado y actualizado.
      end;
    end;
    exit(false);  //No se encontró.
  end;
var
  jmpInst: TAsmInstruction;
begin
{  //Complete operand for instructions with udefined label references.
  {Al final de esta iteración todas las instruciones que incluyan operandos con
  saltos a etiquetas indefinidas, estarán referenciando a la etiqueta correspondiente
  en lugar de solo guardar el nombre de la etiqueta.}
  for jmpInst in curBlock.undefInstrucs do begin
    if jmpInst.operand.nam<>'' then begin    //Has an undefined label
      if not CompleteUndefJump(jmpInst.operand) then begin
        //No se enuentra "jmpInst" en "labels".
        GenError(Format(ER_UNDEF_LABEL_, [jmpInst.operand.nam]), jmpInst.SrcPos);
      end;
    end;
    if jmpInst.operand2.nam<>'' then begin   //Has an undefined label
      if not CompleteUndefJump(jmpInst.operand2) then begin
        //No se enuentra "jmpInst" en "labels".
        GenError(format(ER_UNDEF_LABEL_, [jmpInst.operand2.nam]), jmpInst.SrcPos);
      end;
    end;
  end;
}end;
procedure TParserAsm6502.ProcInstrASM(idInst: TP6502Inst);
{Proccess an 6502 ASM instruction. Instruction must be previously validated and
 identified in "idInst".
 Basically this procedure, add a new TAsmInstruction (including instruction, addresing
 mode and operamd) to the current TAstAsmBlock, that represents a 6502 instruction.
 An instruction ends with the EOL token or the ASM delimiter "END".
 This procedure must not process the EOL token or the "END" delimiter.
}
var
  tok: String;
  addressModes: TP6502AddModes;
  srcInst: TSrcPos;
begin
  addressModes := PIC16InstName[idInst].addressModes;
  srcInst := lex.GetSrcPos;
  //Capture operand
  lex.Next;
  lex.SkipWhitesNoEOL;
  tok := lex.token;
  if (lex.tokType = tkEol) or (tokIdent = txEND) then begin
    //Sin parámetros. Puede ser Implícito o Acumulador
    if aImplicit in addressModes then begin
      //Tiene modo implícito
      AddInstruction(idInst, aImplicit, 0, srcInst);
    end else if aAcumulat in addressModes then begin
      //Tiene modo implícito
      AddInstruction(idInst, aAcumulat, 0, srcInst);
    end else begin
      //An operand must follow.
      GenError(ER_EXP_CON_VAL);
      exit;
    end;
  end else if tok = '#' then begin
    //Direccionamiento Inmediato
    lex.Next;      //Toma "#"
    AddInstruction(idInst, aImmediat, 0, srcInst);
    //Complete the "param" of "curInst".
    if not CaptureOperand(curInst.operand) then begin
      GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
      exit;
    end;
    lex.SkipWhitesNoEOL;
  end else if tok = '(' then begin
    //Direccionamiento Indirecto: (indirect), (indirect,X), (indirect),Y o (aAbsInIdX, X)
    AddInstruction(idInst, aIndirect, 0, srcInst);  //Add the instruction with "aImplicit" temporally. Later will be updated.
    lex.Next;
    if lex.tokType in [tkLitNumber, tkIdentifier] then begin
      if not CaptureOperand(curInst.operand) then begin
        GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
        exit;
      end;
      lex.SkipWhitesNoEOL;
      if lex.token = ',' then begin
        //Can only be (indirect,X)
        lex.Next;  //Take number
        lex.SkipWhitesNoEOL;
        if UpCase(lex.token) <> 'X' then begin
          GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
          exit;
        end;
        lex.Next;  //Take X
        lex.SkipWhitesNoEOL;
        //Only could be aIndirecX or aAbsInIdX
        if aAbsInIdX in addressModes then begin  //Only JMP have this mode and don't have aIndirecX
          curInst.addMode := ord(aAbsInIdX);
        end else begin  //The only option
          curInst.addMode := ord(aIndirecX);
        end;
        //Verify ')'
        if not CaptureParenthes then begin
          GenError(ER_EXPEC_PAREN);
          exit;
        end;
      end else if lex.token = ')' then begin
        //(indirect) or (indirect),Y
        lex.Next;
        lex.SkipWhitesNoEOL;
        if lex.token = ',' then begin
          //Can only be (indirect),Y
          curInst.addMode := ord(aIndirecY);
          lex.Next;  //Toma número
          lex.SkipWhitesNoEOL;
          if UpCase(lex.token) <> 'Y' then begin
            GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
            exit;
          end;
          lex.Next;  //Takes Y
          lex.SkipWhitesNoEOL;
        end else if lex.tokType = tkEol then begin
          //Can only be (indirect)
          //No need to change anything.
        end else begin
          GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
          exit;
        end;
      end else begin
        GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
        exit;
      end;
    end else begin
      GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
      exit;
    end;
  end else begin
    //Puede ser absoluto o página cero, o sus versiones indexadas con X o Y.
    AddInstruction(idInst, aImplicit, 0, srcInst);  //Add the instruction with "aImplicit" temporally. Later will be updated.
    //Complete the "param" of "curInst".
    if not CaptureOperand(curInst.operand) then begin
      GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
      exit;
    end;
    {Get the addressing mode, considering operand is 16bits. If it's 8 bits, the
     addressing mode should be changed when code is generated.}
    lex.SkipWhitesNoEOL;
    //Verify is follows ,X o ,Y
    if lex.token = ',' then begin
      lex.Next;
      lex.SkipWhitesNoEOL;
      if Upcase(lex.token) = 'X' then begin
        lex.Next;
        lex.SkipWhitesNoEOL;
        curInst.addMode := ord(aAbsolutX);
      end else if Upcase(lex.token) = 'Y' then begin
        lex.Next;
        lex.SkipWhitesNoEOL;
        curInst.addMode := ord(aAbsolutY);
      end else begin
        //Could be the 65c02 instruction BBR0 $12, <label>
        if not CaptureOperand(curInst.operand2) then begin
          GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
          exit;
        end;
        //We have an operand.
        curInst.addMode := ord(aZeroPRel);
      end;
    end else begin
      if addressModes = [aRelative] then begin
        //Only accept "aRelative" address. Like BEQ, BNE, ...
        curInst.addMode := ord(aRelative);
      end else if addressModes = [aImplicit] then begin
        //Only accept "aImplicit" address. Like CLC, CLD, ...
        curInst.addMode := ord(aImplicit);
      end else begin
        curInst.addMode := ord(aAbsolute);
      end;
    end;
  end;
end;
procedure TParserAsm6502.ParseInstructionLabel(local: Boolean);
{Parse an instruction that defines a label. It can be:
"<identifier>:" or "@<identifier>:"
If not errors are founds, a new instruction is added to the current ASM block element.
Set "curInst" pointing to the instruction added.}
var
  lblName: String;
begin
  lblName := lex.token;   //guarda nombre de la etiqueta
  lex.Next;
  if lex.token = ':' then begin
    //Definitivamente es una etiqueta
    lex.Next;      //Toma ":"
    //Crea la instrucción de etiqueta
    AddInstructionLabel(lblName, False);
    //Verifica si sigue una instrucción
    lex.SkipWhitesNoEOL;
    ProcASMline;
    exit;
  end else begin
    //Not a label
    GenError(Format(ER_EXP_COL_LAB_, [lblName]));
    exit;
  end;
end;
procedure TParserAsm6502.ProcASMline;
{Process a line of ASM code. That line can be a mnemonic, a label, a comment, ...
 A line of ASM ends with the EOL or with the END reserved word.
 If found END, returns TRUE in "blkEnd".
 After processing a line (with error or not), this procedure leaves the lexer cursor at
 the start of the next line, except when the delimiter END is found. }
var
  idInst: TP6502Inst;
  tok, lbl: String;
  lblEle: TAsmInstruction;
  undefLabel: boolean;
  n: Integer;
begin
  if lex.tokType = tkEol then begin  //Empty instruction
    lex.Next;   //Go to next line
    exit; //Empty line
  end;
  //Proccess the ASM line
  if lex.tokType = tkIdentifier then begin
    //Could be a mnemonic, directive "ORG" or a label.
    tok := Upcase(lex.token);
    if FindOpcode(tok, idInst) then begin
      //It's a mnemonic
      n := msg.nErrors;
      ProcInstrASM(idInst);
      if msg.nErrors>n then begin
        //There were an error in the last instruction
        lex.GotoEOL;   //Move to end of line.
        lex.Next;      //Pass to the start of the next line.
        exit;
      end;
    end else if tok = 'ORG' then begin
      //It's the ORG directive
      lex.Next;
      AddDirectiveORG(0);  //Operand of ORG will be updated with CaptureOperand().
      if not CaptureOperand(curInst.operand) then exit;
      exit;
    end else if tok = 'DB' then begin
      //Define a byte. Could be multiples bytes.
      repeat
        lex.Next;    //Take DB
        AddDirectiveDB;  //Operand of DB will be updated with CaptureOperand().
        if not CaptureOperand(curInst.operand) then exit;
        lex.SkipWhitesNoEOL;
      until lex.token<>',';
      if lex.tokType = tkEol then begin
        //Must follow Eol
        lex.Next;
      end else begin
        GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
        lex.GotoEOL;   //Move to end of line.
        lex.Next;      //Pass to the start of the next line.
      end;
      exit;
    end else if tok = 'DW' then begin
      //Define a byte. Could be multiples bytes.
      repeat
        lex.Next;    //Take DB
        AddDirectiveDW;  //Operand of DB will be updated with CaptureOperand().
        if not CaptureOperand(curInst.operand) then exit;
        lex.SkipWhitesNoEOL;
      until lex.token<>',';
      if lex.tokType = tkEol then begin
        //Must follow Eol
        lex.Next;
      end else begin
        GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
        lex.GotoEOL;   //Move to end of line.
        lex.Next;      //Pass to the start of the next line.
      end;
      exit;
    end else if tokIdent = txIDENTIF then begin
      //Debe ser una etqueta
      ParseInstructionLabel(false);  //Puede generar error
    end else begin
      //Must be a label
      GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
      exit;
    end;
  end else if tokIdent = txATSYMBOL then begin  //Símbolo "@"
    //Debe ser una etiqueta como: @label1:
    lex.Next;
    if tokIdent = txIDENTIF then begin
      //Debe ser una etqueta
      ParseInstructionLabel(True);  //Puede generar error
    end else begin
      //Must be a label
      GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
      exit;
    end;
  end else if lex.tokType = tkComment then begin
    lex.SkipWhitesNoEOL;
  end else begin
    //Something is wrong
    GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
    lex.GotoEOL;   //Move to end of line.
    lex.Next;      //Pass to the start of the next line.
    Exit;
  end;
  //Test if we're at the line end.
  if lex.tokType = tkEol then begin
    lex.Next;      //Pass to the start of the next line.
  end;
end;
procedure TParserAsm6502.AddInstruction(const inst: TP6502Inst;
  addMode: TP6502AddMode; param: integer; const srcDec: TSrcPos);
{Add a new instruction to the current ASM block element. Set "curInst" pointing
to the instruction added.}
begin
  curInst := TAsmInstruction.Create(srcDec);
  curInst.name := '<inst>';
  curInst.addr := -1;   //Indica que la dirección física aún no ha sido fijada.
  curInst.iType := itOpcode;   //Marca como instrucción de salto.
  curBlock.AddInstruction(curInst);
  //Actualiza propiedades de la instrucción
  curInst.opcode := ord(inst);
  curInst.addMode := ord(addMode);
  curInst.operand.Val := param;
end;
procedure TParserAsm6502.AddInstructionLabel(lblName: string; local: Boolean);
{Add a new instruction to the current ASM block element. Set "curInst" pointing
to the instruction added.
If operand of the instruction is expression, it must be added in the child nodes.}
begin
  curInst := TAsmInstruction.Create(lex.GetSrcPos);
  curInst.name := lblName;
  curInst.addr := -1;   //Indica que la dirección física aún no ha sido fijada.
  if local then begin
    curInst.iType := itLocLabel; //Marca como instrucción de salto.
  end else begin
    curInst.iType := itLabel;    //Marca como instrucción de salto.
  end;
  curBlock.AddInstruction(curInst);
  labels.add(curInst);  //Agrega a la lista de etiquetas
end;
procedure TParserAsm6502.AddDirectiveORG(param: word);
begin
  curInst := TAsmInstruction.Create(lex.GetSrcPos);
  curInst.name := 'ORG';
  curInst.addr := -1;   //Indica que la dirección física aún no ha sido fijada.
  curInst.iType := itOrgDir;  //Represents ORG
  curBlock.AddInstruction(curInst);
  curInst.operand.Val := param;
end;
procedure TParserAsm6502.AddDirectiveDB;
begin
  curInst := TAsmInstruction.Create(lex.GetSrcPos);
  curInst.name := 'DB';
  curInst.addr := -1;   //Indica que la dirección física aún no ha sido fijada.
  curInst.iType := itDefByte;  //Represents DB
  curBlock.AddInstruction(curInst);
end;
procedure TParserAsm6502.AddDirectiveDW;
begin
  curInst := TAsmInstruction.Create(lex.GetSrcPos);
  curInst.name := 'DW';
  curInst.addr := -1;   //Indica que la dirección física aún no ha sido fijada.
  curInst.iType := itDefWord;  //Represents DB
  curBlock.AddInstruction(curInst);
end;
//Inicialización
procedure TParserAsm6502.ProcessASMblock(Body: TBlock);
var
  blkEnd: boolean;
begin
  lex.Next;     //Get ASM
  lex.curCtx.OnDecodeNext := @DecodeNext;   //Set a new syntax
  lex.Next;   //Initialize lexer state
  //Create an ASM node
  curBlock := TAsmBlock.Create(lex.GetSrcPos);
  Body.AddStatement(curBlock);
  StartASM;
  curInst := nil;
  lex.SkipWhitesNoEOL;  //Omite espacios iniciales de la línea
  while not lex.atEof and (tokIdent <> txEND) do begin
//    debugln('fil=' + IntTostr(lex.curCtx.row0) + ', col=' + IntToStr(lex.curCtx.col0));
    ProcASMline;
    lex.SkipWhitesNoEOL;  //Omite espacios iniciales de la línea
  end;
  if lex.atEof then begin
    GenError('Unclosed ASM block.');  //Don't stop scanning
  end;
  EndASM;
  //Current token is delimiter END.
  lex.curCtx.OnDecodeNext := nil;   //Restore lexer here, in order to take the "END" with the new lexer and avoid problems of syntax.
  lex.Next;   //Take END with default lexer.
end;
function TParserAsm6502.DecodeNext: boolean;
{Decode the token in the current position, indicated by (frow, fcol), and returns:
 - Token type in "toktyp".
 - Start of next token in (frow, fcol).
 - Value TRUE if the current line has changed.
}
var
  ctx: TContext;
  iden: String;
begin
  ctx := lex.curCtx;
  if ctx._Eof then begin
    ctx.tokType := tkNull;
    tokIdent := txOTHER;
    exit(false);
  end else if ctx._Eol then begin
    ctx.tokType := tkEol;
    tokIdent := txOTHER;
    if ctx._LastLine then begin
      //Cannot advance to a NextChar line. Keep position (EOF)
    end else begin
      //In a common line
      ctx._setRow(ctx.frow+1);
      ctx._setCol(1);
    end;
    exit(true);
  end;
  case ctx.curLine[ctx.fcol] of
  #32, #9: begin
    repeat
      inc(ctx.fcol);
    until ctx._Eol or not(ctx.curline[ctx.fcol] in [#32, #9]);
    //Leaves (ctx.frow, ctx.fcol) in the begin of the next token.
    ctx.tokType := tkSpace;
    tokIdent := txOTHER;
  end;
  '0'..'9': begin
    repeat
      inc(ctx.fcol);
    until ctx._Eol or not(ctx.curline[ctx.fcol] in ['0'..'9','.']);
    ctx.tokType := tkLitNumber;
    tokIdent := txLITNUMBER;
  end;
  '$': begin
    repeat
      inc(ctx.fcol);
    until ctx._Eol or not(ctx.curline[ctx.fcol] in ['0'..'9','A'..'F','a'..'f']);
    ctx.tokType := tkLitNumber;
    tokIdent := txLITNUMBER;
  end;
  '%': begin
    repeat
      inc(ctx.fcol);
    until ctx._Eol or not(ctx.curline[ctx.fcol] in ['0','1']);
    ctx.tokType := tkLitNumber;
    tokIdent := txLITNUMBER;
  end;
  'E','e': begin
    repeat inc(ctx.fcol); until ctx._Eol or not(ctx.curline[ctx.fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(ctx.curLine, ctx.col0, (ctx.fcol-ctx.col0)));
    if iden = 'END' then begin
      ctx.tokType := tkKeyword;
      tokIdent := txEND;
    end else begin
      ctx.tokType := tkIdentifier;
      tokIdent := txIDENTIF;
    end;
  end;
  'A'..'D','F'..'Z','_',
  'a'..'d','f'..'z': begin
    repeat inc(ctx.fcol); until ctx._Eol or not(ctx.curline[ctx.fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    ctx.tokType := tkIdentifier;
    tokIdent := txIDENTIF;
  end;
  '@': begin
    ctx._NextChar;
    ctx.tokType := tkOperator;
    tokIdent := txATSYMBOL;
  end;
  '+','-','*','/','\','=','^','#','>','<',':': begin
    ctx._NextChar;
    ctx.tokType := tkOperator;
    tokIdent := txOTHER;
  end;
  '.': begin
    ctx._NextChar;
    ctx.tokType := tkOperator;
    tokIdent := txDOT;
  end;
  ';': begin
    ctx._NextChar;
    while not ctx._Eol do ctx._NextChar;
    //repeat ctx._NextChar until ctx._Eol;
    ctx.tokType := tkComment;
    tokIdent := txCOMMENT;
  end;
  '(': begin
    ctx._NextChar;
    ctx.tokType := tkSymbol;
    tokIdent := txPAREN_OP;
  end;
  ')': begin
    ctx._NextChar;
    ctx.tokType := tkSymbol;
    tokIdent := txPAREN_CL;
  end;
  ',','[',']': begin
    ctx._NextChar;
    ctx.tokType := tkOthers;
    tokIdent := txOTHER;
  end;
  '''': begin
    repeat inc(ctx.fcol); until ctx._Eol or (ctx.curline[ctx.fcol] = '''');
    if ctx._Eol then begin
      GenError('Unclosed string.');  //Don't stop scanning
    end else begin
      ctx._NextChar;  //Go to next character
    end;
    ctx.tokType := tkString;
    tokIdent := txOTHER;
  end;
  else
    //Unkmown token.
    ctx.tokType := tkNull;  //WARNING: This make the current token will read as empty.
    tokIdent := txOTHER;
    ctx._NextChar;
  end;
  exit(false);
end;
constructor TParserAsm6502.Create(msg0: TMessageManager; lex0: TAleLexer);
begin
  inherited Create;
  msg := msg0;  //Toma referencia al gestor de mensajes
  lex := lex0;  //Toma referencia al lexer
  labels := TAsmInstructionList.Create(false);
end;
destructor TParserAsm6502.Destroy;
begin
  labels.Destroy;
  inherited Destroy;
end;

initialization
finalization
end.

