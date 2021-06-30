grammar Bil;

bil : line+ EOF;
var : ID ;
sub : ID ;
word : LITERAL ;
assign : var ':=' exp ;
line : ADDRESS ADDR_SEP exp WHITESPACE? '\n';
exp : exp bop exp
    | assign
    | '(' exp ')' 
    | uop exp 
    | var
    | word 
    | exp 'with' '[' exp ',' ENDIAN ']:' NAT '<-' exp
    | CAST ':' NAT '[' exp ']'
    | 'call' SUB 'with return' RET_ADDRESS
    | 'sub' sub '(' (arg ',')* ')'
    ;

bop : PLUS | MINUS | TIMES | DIVIDE | MODULO | LSL | LSR | ASR | BAND | BOR | BXOR | EQ | LT | LE ;
uop : NOT ;

NAT : ('u32' | 'u64' | '32' | '64') ;
ENDIAN : ('el' | 'be');
ADDRESS: ([0-9]|[a-f])+ ;
ADDR_SEP: ':' ;
LITERAL : (('0x' ([0-9]|[a-f])+) | [0-9]+) ;
ID : ('X'([A-Z]|[a-z]|[0-9])* | 'mem');
CAST : ('pad' | 'extend' | 'high' | 'low') ;
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
SUB_REFERENCE : ('@' ([a-z]|[A-Z])+) | 'LR';
RET_ADDRESS : '%' ([0-9]|[a-f])+ ;

