grammar Bil;

bil : block+ EOF;
block : sub param* (stmt|NEWLINE)* endsub NEWLINE* ; 

sub : NUMBER ':' 'sub' ID '(' ID? (',' ID)* ')' NEWLINE;
endsub : NUMBER ':' 'call LR with noreturn' NEWLINE;
stmt : (NUMBER ':' (assign|call)? NEWLINE);


param : NUMBER ':' ID '::' inout nat '=' var NEWLINE;


assign : var ':=' exp ;
call : 'call' '@'? ID 'with' returnaddr ;
exp : exp bop exp
    | word 
    | '(' exp ')' 
    | uop exp 
    | var
    | exp 'with' '[' exp ',' ENDIAN ']:' nat '<-' exp
    | CAST ':' nat '[' exp ']'
    | exp '[' exp ',' ENDIAN ']:' nat
    ;

var : ID ;
word : number ;
bop : PLUS | MINUS | TIMES | DIVIDE | MODULO | LSL | LSR | ASR | BAND | BOR | BXOR | EQ | LT | LE ;
uop : NOT ;
inout : 'in out' | 'in' | 'out' ;
returnaddr : 'noreturn' | 'return' '%' number ;
number : (NUMBER | NAT32 | NAT64) ;
nat : (NAT | NAT32 | NAT64) ;

NAT32 : '32' ;
NAT64 : '64' ;
NAT : ('u32' | 'u64') ;
ENDIAN : ('el' | 'be');
NUMBER : HEX | DECIMAL;
CAST : ('pad' | 'extend' | 'high' | 'low') ;
ADDR_SEP: ':' ;
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

