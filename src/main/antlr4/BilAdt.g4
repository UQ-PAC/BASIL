grammar BilAdt;


item : (SYMBOL OPEN_PAREN sequence CLOSE_PAREN) | OPEN_BRACKET sequence CLOSE_BRACKET | OPEN_PAREN sequence CLOSE_PAREN| STRING | NUM | REGISTER;

sequence : (item (COMMA item)*)?;

NEWLINE : '\r'? '\n' -> skip;
WHITESPACE : ' '+ -> skip;
COMMENT : '//' ~[\r\n]* -> skip;

SYMBOL : ALPHA+;
ALPHA : [A-Za-z];

NUM : DEC | HEX;
DEC : DIGIT+;
HEX : '0x'? HEXDIGIT+;
REGISTER : 'R' DEC;

DIGIT : [0-9];
HEXDIGIT : [0-9a-fA-F];

ESCAPE : '\\' ( '"' | '\\' | 'n' | 'x');
STRING :  '"' ( ESCAPE | ~('"' | '\\' | '\n' | '\r') )+ '"' ;


OPEN_PAREN : '(';
CLOSE_PAREN : ')';
COMMA : ',';


OPEN_BRACKET : '[';
CLOSE_BRACKET : ']';


//program : KW_PROGRAM OPEN_PAREN tid COMMA attrs COMMA subs CLOSE_PAREN;
//
//keyword : KW_PROJECT | KW_PROGRAM | KW_UNDEFINED;
//
//attrs: KW_ATTRS unused;
//
////arg : 'Tid' '(' NUM '"%' NUM '"' ')';
//subs : KW_SUBS unused;
////subs : OPEN_BRACKET (sub (COMMA sub)*)? CLOSE_BRACKET;
//
//tid : KW_TID unused;
//sub : KW_SUB unused; //KW_SUB OPEN_BRACKET  CLOSE_BRACKET;

//unused : OPEN_PAREN (~CLOSE_PAREN | unused)* CLOSE_PAREN;

//ADDR : ('%'|'#') HEXDIGIT+;
//UNUSED_ID : ALPHA (ALPHA | DIGIT)*;
//QUOTE : '"' ;
//LT : '<';
//GT : '>';