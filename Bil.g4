grammar Bil;

bil : block+ EOF;
block : sub paramTypes* (stmt|NEWLINE)* endsub NEWLINE* ; 

sub : addr ':' 'sub' functionName '(' param? (',' param)* ')' NEWLINE;
paramTypes : addr ':' param '::' inout nat '=' var NEWLINE;
stmt : addr ':' (assign|call)? NEWLINE;
endsub : addr ':' 'call LR with noreturn' NEWLINE;


assign : var ':=' exp ;
call : 'call' '@'? functionName 'with' returnaddr ;
exp : exp bop exp
    | literal
    | '(' exp ')' 
    | uop exp 
    | var
    | exp 'with' '[' exp ',' ENDIAN ']:' nat '<-' exp
    | CAST ':' nat '[' exp ']'
    | exp '[' exp ',' ENDIAN ']:' nat
    ;

var : ID ;
functionName : ID ;
param : ID ;
bop : PLUS | MINUS | TIMES | DIVIDE | MODULO | LSL | LSR | ASR | BAND | BOR | BXOR | EQ | LT | LE ;
uop : NOT ;
inout : 'in out' | 'in' | 'out' ;
returnaddr : 'noreturn' | 'return' '%' addr ;
addr : (NUMBER | NAT32 | NAT64) ;
literal : (NUMBER | NAT32 | NAT64) ;
nat : (NAT | NAT32 | NAT64) ;

NAT32 : '32' ;
NAT64 : '64' ;
NAT : ('u32' | 'u64') ;
ENDIAN : ('el' | 'be');
NUMBER : HEX | DECIMAL;
CAST : ('pad' | 'extend' | 'high' | 'low') ;
ID : ALPHA (ALPHA | NUMBER | '_')* ;
DECIMAL : [0-9]+ ;
HEX : '0x'? ([0-9]|[a-f]|[A-F])+ ;
ALPHA : ([A-Z]|[a-z])+ ;
NEWLINE : '\r'? '\n' ;
WHITESPACE : ' '+ -> skip;
COMMENT : '//' ~[\r\n]* -> skip;
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

