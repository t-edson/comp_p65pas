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
type //Identifcador de tokens para el lexer
  TASMTokenIdent = (
    txOTHER    ,  //Not identified.
    txEOF      ,  //End of file
    //Keywords
    txEND      ,  //Keyword "END"
    txLOW      ,  //Keyword "LOW"
    txHIGH     ,  //Keyword "HIGH"
    //Symbols
    //Operators
    txATSYMBOL ,  //Operator "@"
    txCOLON    ,  //Symbol ":"
    tXCOMMA    ,  //Symbol ","
    txPAREN_OP ,  //Symbol "("
    txPAREN_CL ,  //Symbol ")"
    txHASH     ,  //Symbol "#"
    //Operators
    txDOT      ,  //Operator "."
    txGREAT    ,  //Operator ">"
    txGREAT_E  ,  //Operator ">="
    txLESS     ,  //Operator "<"
    txLESS_E   ,  //Operator "<="
    txMINUS    ,  //Operator "-"
    txMULT     ,  //Operator "*"
    txNOT_EQ   ,  //Operator "<>"
    txPLUS     ,  //Operator "+"
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
  private  //Métodos auxiliares para el parser
    procedure Next;
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
    procedure ParseASMblock(Body: TBlock);
    procedure ParseAdicVarDec(Items: TASTNodeList; idxVarIni: Integer);
    function DecodeNext: boolean;
    constructor Create(msg0: TMessageManager; lex0: TAleLexer);
    destructor Destroy; override;
  end;

type  //Tipos para declaraciones adicionales de variables
  { TAdicDeclar }
  {Define aditional declaration settings for variable. Depends on target CPU architecture.
  Each compiler will support only what fit to its architecture.}
  TAdicDeclar = (
    decNone,   //Normal declaration. Will be mapped in RAM according compiler decision.
    decAbsol,  //Mapped in ABSOLUTE address
    decZeroP,  //Mapped in Zero page
    decDatSec, //Mapped at the Data section (Normal)
    decRegis,  //Mapped at Work Register (WR)
    decRegisA, //Mapped at A register
    decRegisX, //Mapped at X register
    decRegisY  //Mapped at Y register
  );

  {Description for aditional information in variables declaration: ABSOLUTE ,
  REGISTER,  or initialization. }
  TAdicVarDec = record
    //Absolute or register information.
    hasAdic  : TAdicDeclar;   //Flag. Indicates when variable is register or absolute.
//    absVar   : TAstVarDec;    //Reference to variable, when is ABSOLUTE <variable>
    absAddr  : TExpression;   {Reference to the AST expression that returns the absolute
                              address where the variable should be located. Only valid
                              when: hasAdic = decAbsol.}
    //Initialization information.
    hasInit  : TExpression;   {Reference and Flag. When is not NIL, refers to the
                              expression in the AST where is the initial value.
                              Initial expression must be a child node.}
    //*** También puede dejarse "hasInit" como boolean y crear otro campo "initVal".
    {Although the "absolute address" and the "initial value" can be obtained from the
    children nodes of the variable declaration, the quantity of nodes (1 or 2) and the
    value of the first node (first can be "absolute address" or "initial value"), are not
    fixed. That's why we have references to these nodes (absAddr and hasInit).}
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
  ER_INV_MEMADDR  = 'Invalid memory address.';

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
//Métodos auxiliares para el parser
procedure TParserAsm6502.Next;
{Versión resumida de lex.Next con saltos por espacios}
begin
  lex.Next;
  lex.SkipWhites;
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
    tokIdent := txEOF;
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
    ctx.ScanIdentifier;
    if ctx.MatchToken('END') then begin
      ctx.tokType := tkKeyword;
      tokIdent := txEND;
    end else begin
      ctx.tokType := tkIdentifier;
      tokIdent := txIDENTIF;
    end;
  end;
  'H','h': begin
    ctx.ScanIdentifier;
    if ctx.MatchToken('HIGH') then begin
      ctx.tokType := tkKeyword;
      tokIdent := txHIGH;
    end else begin
      ctx.tokType := tkIdentifier;
      tokIdent := txIDENTIF;
    end;
  end;
  'L','l': begin
    ctx.ScanIdentifier;
    if ctx.MatchToken('LOW') then begin
      ctx.tokType := tkKeyword;
      tokIdent := txLOW;
    end else begin
      ctx.tokType := tkIdentifier;
      tokIdent := txIDENTIF;
    end;
  end;
  'A'..'D','F'..'G','I'..'K','M'..'Z','_',
  'a'..'d','f'..'g','i'..'k','m'..'z': begin
    ctx.ScanIdentifier;
    ctx.tokType := tkIdentifier;
    tokIdent := txIDENTIF;
  end;
  //Operadores
  '+','-','*','/','\','=','^': begin
    ctx._NextChar;
    ctx.tokType := tkOperator;
    tokIdent := txOTHER;
  end;
  '@': begin
    ctx._NextChar;
    ctx.tokType := tkOperator;
    tokIdent := txATSYMBOL;
  end;
  '.': begin
    ctx._NextChar;
    ctx.tokType := tkOperator;
    tokIdent := txDOT;
  end;
  '<': begin
    ctx._NextChar;
    ctx.tokType := tkOperator;
    tokIdent := txLESS;
  end;
  '>': begin
    ctx._NextChar;
    ctx.tokType := tkOperator;
    tokIdent := txGREAT;
  end;
  //Símbolos
  '[',']': begin
    ctx._NextChar;
    ctx.tokType := tkSymbol;
    tokIdent := txOTHER;
  end;
  ':': begin
    ctx._NextChar;
    ctx.tokType := tkSymbol;
    tokIdent := txCOLON;
  end;
  ',': begin
    ctx._NextChar;
    ctx.tokType := tkSymbol;
    tokIdent := tXCOMMA;
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
  '#': begin
    ctx._NextChar;
    ctx.tokType := tkSymbol;
    tokIdent := txHASH;
  end;
  ';': begin
    ctx._NextChar;
    while not ctx._Eol do ctx._NextChar;
    //repeat ctx._NextChar until ctx._Eol;
    ctx.tokType := tkComment;
    tokIdent := txCOMMENT;
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
      if tokIdent = txLOW then begin   //'LOW'
        operation := aopSelByte;
        value := 0;
        lex.Next;
        exit(true);
      end else if tokIdent = txHIGH then begin  //'HIGH'
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
    end else if tokIdent in [txPLUS, txMINUS] then begin  //"+", "-"
      if tokIdent = txPLUS then operation := aopAddValue else operation := aopSubValue;
      //Get operand
      lex.Next;
      lex.SkipWhitesNoEOL;
      if (lex.tokType = tkEol) or (tokIdent = txCOMMENT) then begin
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
    if tokIdent = txGREAT then begin  //">"
      lex.Next;
      exit('>');
    end else if tokIdent = txLESS then begin  //"<"
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
  addressModes: TP6502AddModes;
  srcInst: TSrcPos;
begin
  addressModes := PIC16InstName[idInst].addressModes;
  srcInst := lex.GetSrcPos;
  //Capture operand
  lex.Next;
  lex.SkipWhitesNoEOL;
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
  end else if tokIdent = txHASH then begin    //"#"
    //Direccionamiento Inmediato
    lex.Next;      //Toma "#"
    AddInstruction(idInst, aImmediat, 0, srcInst);
    //Complete the "param" of "curInst".
    if not CaptureOperand(curInst.operand) then begin
      GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
      exit;
    end;
    lex.SkipWhitesNoEOL;
  end else if tokIdent = txPAREN_OP then begin  //"("
    //Direccionamiento Indirecto: (indirect), (indirect,X), (indirect),Y o (aAbsInIdX, X)
    AddInstruction(idInst, aIndirect, 0, srcInst);  //Add the instruction with "aImplicit" temporally. Later will be updated.
    lex.Next;
    if lex.tokType in [tkLitNumber, tkIdentifier] then begin
      if not CaptureOperand(curInst.operand) then begin
        GenError(Format(ER_SYNTAX_ERR_, [lex.token]));
        exit;
      end;
      lex.SkipWhitesNoEOL;
      if tokIdent = tXCOMMA then begin    //","
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
      end else if tokIdent = txPAREN_CL then begin  //")"
        //(indirect) or (indirect),Y
        lex.Next;
        lex.SkipWhitesNoEOL;
        if tokIdent = tXCOMMA then begin  //","
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
    if tokIdent =  tXCOMMA then begin  //","
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
  if tokIdent = txCOLON then begin  //":"
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
      until tokIdent<>tXCOMMA; //","
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
      until tokIdent<>tXCOMMA; //","
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
  end else if tokIdent = txCOMMENT then begin
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
procedure TParserAsm6502.ParseASMblock(Body: TBlock);
{Punto de entrada para analizar un bloque ASM, de la forma ASM ... END.}
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
    if msg.nErrors>=100 then Break;
  end;
  if lex.atEof then begin
    GenError('Unclosed ASM block.');  //Don't stop scanning
  end;
  EndASM;
  //Current token is delimiter END.
  lex.curCtx.OnDecodeNext := nil;   //Restore lexer here, in order to take the "END" with the new lexer and avoid problems of syntax.
  lex.Next;   //Take END with default lexer.
end;
procedure TParserAsm6502.ParseAdicVarDec(Items: TASTNodeList; idxVarIni: Integer);
{Procesa la parte adicional de las declaraciones de variables. Esta parte opcional puede
ser :
ABSOLUTE <dirección>
ABSOLUTE <variable>
REGISTER
Se separa el procesamiento en esta unidad, porque esta parte adicional es muy dependiente
del hardware.}
  function ReadAddres(tok: string): word;
  {Lee una dirección de RAM a partir de una cadena numérica.
  Puede generar error.}
  var
    n: LongInt;
  begin
    //COnvierte cadena (soporta binario y hexadecimal)
    if not TryStrToInt(tok, n) then begin
      //Podría fallar si es un número muy grande
      GenError(ER_INV_MEMADDR);
      {%H-}exit;
    end;
    if HayError then exit(0);
    Result := n;
  end;
{var
  n: integer;
  tokL: String;
  consTyp: TAstTypeDec;
  nItems : integer;
  consIni: TAstExpress;

  aditVar: TAdicVarDec;}
begin
{  aditVar.hasAdic  := decNone;       //Bandera
  aditVar.hasInit  := nil;
  tokL := lowercase(lex.token);
  if (tokL = 'absolute') or (lex.token = '@') then begin
    // Hay especificación de dirección absoluta ////
    aditVar.hasAdic := decAbsol;    //marca bandera
    lex.Next;
    lex.SkipWhites;
    aditVar.absAddr := GetConstValue(varTyp, mainTypCreated);  //Leemos como constante
    if HayError then exit;

  end else if tokL = 'register' then begin    //Register type
    aditVar.hasAdic := decRegis;    //marca bandera
    lex.Next;
    lex.SkipWhites;
  end else if tokL = 'registera' then begin //Register type
    aditVar.hasAdic := decRegisA;    //marca bandera
    lex.Next;
    lex.SkipWhites;
  end else if tokL = 'registerx' then begin  //Register type
    aditVar.hasAdic := decRegisX;    //marca bandera
    lex.Next;
    lex.SkipWhites;
  end else if tokL = 'registery' then begin  //Register type
    aditVar.hasAdic := decRegisY;    //marca bandera
    lex.Next;
    lex.SkipWhites;
  end else if tokL = 'zeropage' then begin   //Zero page
    aditVar.hasAdic := decZeroP;    //Set flag
    lex.Next;
    lex.SkipWhites;
  end;
  //Verifica compatibilidad de tamaños
  if aditVar.hasAdic in [decRegisA, decRegisX, decRegisY] then begin
    //Solo pueden ser de tamaño byte
    if not varTyp.IsByteSize then begin
      GenError('Only byte-size types can be a specific register.');
      exit;
    end;
  end;
  //Puede seguir una sección de inicialización: var: char = 'A';
  ProcComments;
  if lex.token = '=' then begin
    lex.Next;   //lo toma
    ProcComments;
    //Aquí debe seguir el valor inicial constante.
    consIni := GetConstValue(varTyp, mainTypCreated);  //Leemos como constante
    if HayError then exit;
    consTyp := consIni.Typ;
    aditVar.hasInit := consIni;
    //Ya se tiene el valor constante para inicializar variable.
    if aditVar.hasAdic in [decRegis, decRegisA, decRegisX, decRegisY] then begin
      GenError('Cannot initialize REGISTER variables.');
      exit;
    end else if aditVar.hasAdic = decAbsol then begin
      GenError('Cannot initialize ABSOLUTE variables.');
      exit;
    end else if aditVar.hasAdic = decZeroP then begin
      GenError('Cannot initialize ZEROPAGE variables.');
      exit;
    end else if aditVar.hasAdic = decNone then begin
      //Not specified declaration
      {We force to be in Data Section. Otherwise compiler could try to allocate it in
      primary Data section (defined by SET_DATA_ADDR ) and then it won't be able to be
      initialized.}
      aditVar.hasAdic := decDatSec;
    end;
  end else begin
    //No hay asignación inicial.
    aditVar.hasInit := nil;
  end;
  //Validate initialization for dynamic arrays.
  if (varTyp.catType = tctArray) then begin
    if varTyp.isDynam then begin
      //Dynamic array
      if aditVar.hasInit = nil then begin
        //Es un arreglo dinámico. Debió inicializarse.
        GenError(ER_EQU_EXPECTD);
        exit;
      end;
      //Has initialization. Validates.
      if consTyp.catType <> tctArray then begin
        GenError('Expected an array.');
        exit;
      end;
      //Here we assure "varTyp" and "consTyp" are both arrays.
      //Validation for item types.
      if varTyp.itmType <> consTyp.itmType then begin
        //GenError('Item type doesn''t match for initialize array.');
        GenError('Cannot initialize. Expected array of "%s". Got array of "%s".',
                 [varTyp.itmType.name, consTyp.itmType.name]);
        exit;
      end;
      //Both are arrays of the same item type.
//      ast.DeleteTypeNode(varTyp);  //We don't need this type *** Genera error en la síntesis si se elimina.
      varTyp := consTyp;  //Use the same array type declaration.
      exit;
    end;
    if aditVar.hasInit<>nil then begin
      nItems := consTyp.consNitm.value^.ValInt;
      //Validation for category
      if consTyp.catType <> tctArray then begin
        GenError('Expected an array.');
        exit;
      end;
      //both are arrays. Validation for item types.
      if varTyp.itmType <> consTyp.itmType then begin
        GenError('Item type doesn''t match for initialize array.');
        exit;
      end;
      //Validation for size. Must have the same size to simplify creating and calling new types.
      if varTyp.nItems < nItems then begin
        GenError('Too many items to initialize array.');
      end else if varTyp.nItems > nItems then begin
        GenError('Too few items to initialize array.');
      end ;
      //Validate type compatibility
      //First validation
      if consTyp <> varTyp then begin
        GenError('Expected type "%s". Got "%s".', [varTyp.name, consTyp.name]);
        exit;
      end;
    end;
  end else begin  //No array
    if aditVar.hasInit<>nil then begin
      if consTyp <> varTyp then begin
        GenError('Cannot initialize. Expected type "%s". Got "%s".', [varTyp.name, consTyp.name]);
        exit;
      end;
    end;
  end;
  {Ya se validó la pertinencia de la inicialización y ya se tiene el operando de
  inicialización en "consIni". Ahora toca validar la compatibilidad de los tipos.}
  //Por ahora solo se permite inicializar arreglos.
  if aditVar.hasInit<>nil then begin
    if (varTyp.catType = tctArray) then begin
    end else begin
    end;
  end;
}end;
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

