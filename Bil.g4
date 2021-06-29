grammar Bil;

var : ID ;
word : LITERAL ;
assign : var ':=' exp ;
exp : 
      exp bop exp 
    | '(' exp ')' 
    | uop exp 
    | var
    | word 
    |exp ' with [' exp ',' ENDIAN ']:' NAT '<-' exp
;

bop : PLUS | MINUS | TIMES | DIVIDE | MODULO | LSL | LSR | ASR | BAND | BOR | BXOR | EQ | LT | LE ;
uop : NOT ;

ID : [A-z]([A-z] | [0-9])* ;
CAST : ('pad' | 'extend' | 'high' | 'low') ;
NAT : ('32' | '64' | 'u32' | 'u64') ;
LITERAL : (('0x' [0-9]+) | [0-9]+) ;
WHITESPACE : ' ' -> skip;
ENDIAN : ('el' | 'be') ;
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


// Parser 
//stmt : (assign | jump | while_ | if_ | ifelse) ;
//seq : '{' stmt+ '}' ;
//jump : 'jmp' exp ;
//while_ : 'while' exp seq ;
//if_ : 'if' exp seq ;
//ifelse : 'if' exp 'else' exp ;
//
//
//
//var : ID ':' type ;
//type : IMM | MEM ;
//
//
//// Lexer 
//
//IMM : 'imm[' [0-9]+ ']' ;
//MEM : 'mem[' [0-9]+','[0-9]+']' ;
//ID : [A-z]+ ;
//WORD : '0x' ([0-9] | [a-f])+;
//
//
//EL : 'el' ; // Little endian
//BE : 'be' ; // Big endian
//
//LOW : 'low' ; // extract lower bits
//HIGH : 'high' ; // extract higher bits
//SIGNED : 'signed' ; // extend with sign bit
//UNSIGNED : 'unsigned' ; // extend with zero









