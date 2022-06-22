grammar BoogieExpr;

expr : e0;
e0 : e1 | e2 | EQUIV_OP e0;
e1 : e2 | e2 IMPL_OP e1;
e2 : e3 | e3 eOr+ | e3 eAnd+;
eOr : OR_OP e3;
eAnd : AND_OP e3;
e3 : e4 | e4 REL_OP e4;
e4 : e5 | e4 CONCAT_OP e5;
e5 : e6 | e5 ADD_OP e6;
e6 : e7 | e6 MUL_OP e7;
e7 : UN_OP* e8;
e8 : e9 MAP_OP*;

e9 : KW_FALSE
   | KW_TRUE
   | NUMBER
   // BITVEC
   | IDENTIFIER funcApplication
   | KW_OLD LPAREN expr RPAREN
   | LPAREN QOP typeArgs? idsType (COMMA idsType)* QSep trigAttr* expr RPAREN
   | LPAREN expr RPAREN
   ;

idsType : id (COMMA id)* COLON type;

type : typeAtom | mapType | id typeCtorArgs?;
typeAtom : KW_BOOL | KW_INT | KW_BV NUMBER | LPAREN type RPAREN;
mapType : typeArgs? LBRACKET type (COMMA type)* RBRACKET type;
typeCtorArgs : typeAtom typeCtorArgs? | id typeCtorArgs? | mapType;
typeArgs : LANGLE id (COMMA id)* RANGLE;
funcApplication : LPAREN expr* RPAREN;

trigAttr : trigger | attribute;

// Bitvector
bitVector : NUMBER KW_BV NUMBER;

trigger : LCURLY expr+ RCURLY;
attribute : LCURLY COLON IDENTIFIER attrArg* RCURLY;
attrArg : expr | STRING;

COMMA : ':';
COLON : ':';
EQUIV_OP : '<==>';
IMPL_OP : '==>';
OR_OP : '||';
AND_OP : '&&';
REL_OP : '==' | '!=' | '<' | '>' | '<=' | '>=' | '<:';

CONCAT_OP : '++';
ADD_OP : '+' | '-';
MUL_OP : '*' | '/' | '%';
UN_OP : '!' | '-';
QOP : KW_FORALL | KW_EXISTS;
QSEP : '::';

KW_INT : 'int';
KW_FORALL : 'forall';
KW_EXISTS : 'exists';

KW_BV : 'bv';

LPAREN : '(';
RPAREN : ')';
LCURLY : '{';
RCURLY : '}';
LANGLE : '<';
RANGLE: '<';
LBRACKET : '[';
RBRACKET : ']';
KW_FALSE : 'false';
KW_TRUE : 'true';
KW_OLD : 'old';
NUMBER : [0-9]+;

// Technically this can support all non-english unicode letters as well
// and explicitly supports the following symbols [-, ., $, @, ', `, ~, ^, \, ?].

// Strings
ESCAPE : '\\' ( '"' | '\\' | 'n' | 'x' | '.');
STRING :  '"' ( ESCAPE | ~('"' | '\\' | '\n' | '\r') )+ '"' ;

id : IDENTIFIER;