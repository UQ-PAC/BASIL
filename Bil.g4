grammar Bil;

bil : line+ EOF;
var : ID ;
sub : ID ;
word : number ;
assign : var ':=' exp ;
line : NUMBER ADDR_SEP exp NEWLINE;
exp : exp bop exp
    | word 
    | assign
    | '(' exp ')' 
    | uop exp 
    | var
    | exp 'with' '[' exp ',' ENDIAN ']:' nat '<-' exp
    | CAST ':' nat '[' exp ']'
    ;

bop : PLUS | MINUS | TIMES | DIVIDE | MODULO | LSL | LSR | ASR | BAND | BOR | BXOR | EQ | LT | LE ;
uop : NOT ;
number : (NUMBER | NAT32 | NAT64) ;
nat : (NAT | NAT32 | NAT64) ;

NAT32 : '32' ;
NAT64 : '64' ;
NAT : ('u32' | 'u64') ;
ENDIAN : ('el' | 'be');
NUMBER : DECIMAL | HEX ;
CAST : ('pad' | 'extend' | 'high' | 'low') ;
ADDR_SEP: ':' ;
ID : ALPHA (ALPHA | NUMBER)* ;
fragment DECIMAL : [0-9]+ ;
fragment HEX : '0x'? ([0-9]|[a-f])+ ;
fragment ALPHA : ([A-Z]|[a-z])+ ;
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

