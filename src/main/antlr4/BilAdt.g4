grammar BilAdt;

file : adt EOF;

adt : exp
    | term
    | endian
    | unimplemented
    | list
    | tuple
    | tid;



exp : 'Load' OPEN_PAREN var=exp COMMA exp COMMA endian COMMA NUM CLOSE_PAREN                #expLoad
    | 'Store' OPEN_PAREN var=exp COMMA exp COMMA exp COMMA endian COMMA NUM CLOSE_PAREN     #expStore
    | BINOP OPEN_PAREN exp COMMA exp CLOSE_PAREN                                        #expBinop
    | UOP OPEN_PAREN exp CLOSE_PAREN                                                    #expUop
    | 'Var' OPEN_PAREN name=STRING COMMA type CLOSE_PAREN                                    #expVar
    | 'Int' OPEN_PAREN NUM COMMA NUM CLOSE_PAREN                                        #expIntAdt
    | CAST OPEN_PAREN NUM COMMA exp CLOSE_PAREN                                         #expCast
    | 'Extract' OPEN_PAREN NUM COMMA NUM COMMA exp CLOSE_PAREN                          #expExtract
    | 'Concat' OPEN_PAREN                                                               #expConcat
    | STRING                                                                            #expString
    | NUM                                                                               #expNum
    | REGISTER                                                                          #expRegister
    ;

term : def
     | call
     | jmp
     ;

jmp : call;

endian : ENDIAN OPEN_PAREN CLOSE_PAREN;

// Load(mem, idx, endian, size)
//store : ;
// BINOP(exp1, exp2) -- e.g. PLUS(exp1, exp2)

// UOP(exp) -- e.g. NOT(exp1)

// var(name, type)

// Int(num, size)

// CAST(size, expr) -- e.g. UNSIGNED(size, expr)

// Let(var, val, expr) -- Unimplemented

// Unknown(string, type) -- Unimplemented

// Ite(cond, if_true, if_false) -- Unimplemented

// Extract(hb, lb, exp)
//extract : ;

// Concat(lhs, rhs) -- Unimplemented

type : imm | mem;

imm : 'Imm' OPEN_PAREN NUM CLOSE_PAREN;

mem : 'Mem' OPEN_PAREN NUM COMMA NUM CLOSE_PAREN;

// 'Tid'(number, name)
tid : 'Tid' OPEN_PAREN NUM COMMA STRING CLOSE_PAREN;

// 'Def'(
def : 'Def' OPEN_PAREN tid COMMA adt COMMA exp COMMA exp CLOSE_PAREN; // this is an assignment, they're called defs in the ADT.
call : 'Call' OPEN_PAREN tid COMMA adt COMMA adt COMMA adt CLOSE_PAREN;

list : OPEN_BRACKET sequence CLOSE_BRACKET;

tuple : OPEN_PAREN ((adt COMMA) | (adt (COMMA adt)+))  CLOSE_PAREN;

/* Unimportant ADTs - should be matched last */
unimplemented : SYMBOL OPEN_PAREN sequence CLOSE_PAREN;

sequence : (adt (COMMA adt)*)?;

BINOP : PLUS
      | MINUS
      | TIMES
      | DIVIDE
      | SDIVIDE
      | MOD
      | SMOD
      | LSHIFT
      | RSHIFT
      | ARSHIFT
      | AND
      | OR
      | XOR
      | EQ
      | NEQ
      | LT
      | LE
      | SLT
      | SLE;

UOP : NEG | NOT;

CAST : UNSIGNED | SIGNED | HIGH | LOW;

UNSIGNED : 'UNSIGNED';
SIGNED : 'SIGNED';
HIGH : 'HIGH';
LOW : 'LOW';

// BinOp alternatives
PLUS     : 'PLUS';
MINUS    : 'MINUS';
TIMES    : 'TIMES';
DIVIDE   : 'DIVIDE';
SDIVIDE  : 'SDIVIDE';
MOD      : 'MOD';
SMOD     : 'SMOD';
LSHIFT   : 'LSHIFT';
RSHIFT   : 'RSHIFT';
ARSHIFT  : 'ARSHIFT';
AND      : 'AND';
OR       : 'OR';
XOR      : 'XOR';
EQ       : 'EQ';
NEQ      : 'NEQ';
LT       : 'LT';
LE       : 'LE';
SLT      : 'SLT';
SLE      : 'SLE';

// UnOp alternatives
NOT      : 'NOT';
NEG      : 'NEG';

ENDIAN : LITTLE_ENDIAN | BIG_ENDIAN;
LITTLE_ENDIAN : 'LittleEndian';
BIG_ENDIAN : 'BigEndian';

// Numbers and symbols
SYMBOL : ALPHA+;
ALPHA : [A-Za-z];
NUM : DEC | HEX;
DEC : DIGIT+;
HEX : '0x'? HEXDIGIT+;
REGISTER : 'R' DEC;
DIGIT : [0-9];
HEXDIGIT : [0-9a-fA-F];

// Strings
ESCAPE : '\\' ( '"' | '\\' | 'n' | 'x');
STRING :  '"' ( ESCAPE | ~('"' | '\\' | '\n' | '\r') )+ '"' ;

// Delimiters
OPEN_PAREN : '(';
CLOSE_PAREN : ')';
COMMA : ',';
OPEN_BRACKET : '[';
CLOSE_BRACKET : ']';

// Ignored
NEWLINE : '\r'? '\n' -> skip;
WHITESPACE : ' '+ -> skip;
COMMENT : '//' ~[\r\n]* -> skip;
