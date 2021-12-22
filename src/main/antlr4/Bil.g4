grammar Bil;

// TODO this assumes functions are declared contiguously. If this is not the case we are in trouble
// TODO if this turns out to be the case it might be easier to parse in blocks (where a block is terminated by white space)
bil : progSpec progdecl function+ EOF;
function : sub 
         paramTypes*
         (stmt)*
         ;

/* First line is always this */
progdecl: addr ':' 'program';
/* Beginning of sub */
sub : addr ':' 'sub' functionName '(' param? (',' param)* ')';
/* Listing the parameter types */
paramTypes : addr ':' param '::' inout nat '=' var ;  // param: name from C, var: register
/* Statement */
stmt : addr ':' 
       (assign|call|jmp|cjmp)?
     ;

call : 'call' (('@' functionName)|var) 'with' (returnaddr | 'noreturn') ;

assign : var ':=' exp ;
exp : exp bop exp                                       # expBop
    | literal                                           # expLiteral
    | '(' exp ')'                                       # expBracket
    | uop exp                                           # expUop
    | var                                               # expVar
    | exp 'with' '[' exp ',' ENDIAN ']:' nat '<-' exp   # expStore
    | exp 'with' '[' exp ']' '<-' exp                   # expStore8 // sizes can be ommited if storing a single byte
    | CAST ':' nat '[' exp ']'                          # expCast
    // TODO maybe merge these instead??
    | exp '[' exp ',' ENDIAN ']:' nat                   # expLoad
    | exp '[' exp ']'                                   # expLoad8
    | 'extract' ':' nat ':' nat '[' exp ']'             # expExtract
	| functionName'(' argList ')'						# expFunctionCall
    ;
cjmp : 'when' var 'goto' '%' addr ;
jmp : 'goto' (('%' addr)|('@' var));

argList : exp (',' exp)* ;

var : ID ;
functionName : ID | '.'ID ;
param : ID | '\\.'ID ;
bop : PLUS | MINUS | TIMES | DIVIDE | MODULO | LSL | LSR | ASR | BAND | BOR | BXOR | EQ | NEQ | LT | LE | UNKNOWN_OP ;
uop : NOT ;
inout : 'in out' | 'in' | 'out' ;
returnaddr : 'noreturn' | 'return' '%' addr ;
literal : NUMBER ;
nat : (NAT | NUMBER) ;
addr : NUMBER ;

TRUE: 'TRUE' ;
FALSE: 'FALSE' ;
HIGH : 'HIGH' ;
LOW : 'LOW' ;
CAST : ('pad' | 'extend' | 'high' | 'low') ;
NAT : ('u32' | 'u64') ;
ENDIAN : ('el' | 'be');
ID : (ALPHA|'_'|'#') (ALPHA | NUMBER | '_')* ;
NUMBER : HEX | DECIMAL;
DECIMAL : [0-9]+ ;
HEX : '0x'? ([0-9]|[a-f]|[A-F])+ ;
ALPHA : ([A-Z]|[a-z])+ ;
NEWLINE : '\r'? '\n' -> skip;
WHITESPACE : ' '+ -> skip;
COMMENT : '//' ~[\r\n]* -> skip;
AND : '&&' ;
OR : '||' ;
NEQ_PRED : '!=' ;
EQ_PRED : '==' ;
GT: '>' ;
GE: '>=' ;
MAPSTO : '->' ;
PLUS : '+' ;
MINUS : '-' ;
TIMES : '*' ;
DIVIDE : '/' ;
MODULO : '%' ; 
LSL : '<<' ; // Inferred
LSR : '>>' ; // Inferred
ASR : '>>>' ; // Inferred
BAND : '&' ;
BOR : '|' ;
BXOR : 'xor' ; 
EQ : '=' ;
NEQ : '<>' ;
LT : '<' ;
LE : '<=' ;
NOT : '~' ;
UNKNOWN_OP : '~>>' ;


/**
  The grammar which follows this is used to specify the program, and is not bil code.
*/

progSpec: (lpreds | gammas | lattice | rely)* ;

lpreds : 'L:' lpred (',' lpred)* ;
lpred :  (var MAPSTO pred);
gammas : 'GAMMA:' gamma (',' gamma)* ;
gamma :  (var MAPSTO (LOW | HIGH));
lattice : 'Lattice:' lattice_elem (',' lattice_elem)* ;
lattice_elem : ID '<:' 'ID'* ;
rely: 'Rely:' pred ; 

pred :
    pred predBop pred  # predBinOp
    | '(' pred ')'        # predBracket
    | uop pred            # predUniOp
    // TODO i dont think this is right
    | exp expComp exp     # predExprComp
    | predLit             # predLiteral
    | GAMMA_ID            # gammaVar
    ;

GAMMA_ID : 'Gamma_'ID ;
primeVar : (ID | GAMMA_ID)'\''; // TODO
predLit : TRUE | FALSE ;


predBop : AND | OR | NEQ_PRED | EQ_PRED ;
expComp : NEQ_PRED | EQ_PRED | GE | GT | LE | LT ;

