grammar Annotations;

progSpec : (lpreds | gammas | lattice | rely)*;

lpreds : 'L:' lpred (COMMA lpred)*;
lpred : id MAPSTO expr;
gammas : 'GAMMA:' gamma (COMMA gamma)*;
gamma : id MAPSTO id;
lattice: 'Lattice' lattice_elem (COMMA lattice_elem)* ;

// FIXME: Not sure if the 'ID' string is meant to be an ID token, but this was the implementation in the old annotations
//  parser (which was integrated as part of the old bil parser).
lattice_elem : ID '<:' 'ID'*;

rely: 'Rely:' expr;

expr : e0;
e0 : exp=e1 | lhs=e1 EQUIV_OP rhs=e0;
e1 : exp=e2 | lhs=e2 IMPL_OP rhs=e1;
e2 : exp=e3 | exp=e3 eOr+ | exp=e3 eAnd+;
eOr : OR_OP exp=e3;
eAnd : AND_OP exp=e3;
e3 : exp=e4 | lhs=e4 relOp rhs=e4;
e4 : exp=e5 | lhs=e4 CONCAT_OP rhs=e5;
e5 : exp=e6 | lhs=e5 ADD_OP rhs=e6;
e6 : exp=e7 | lhs=e6 MUL_OP rhs=e7;
e7 : UN_OP* exp=e8;
e8 : e9 mapOp*;

e9 : KW_FALSE
   | KW_TRUE
   | NUMBER
   // BITVEC
   | ID funcApplication
   | KW_OLD LPAREN expr RPAREN
   | LPAREN QOP typeArgs? idsType (COMMA idsType)* QSEP trigAttr* expr RPAREN
   | LPAREN expr RPAREN
   ;

mapOp : LBRACKET expr (COMMA expr)* mapUpdate? RBRACKET | LBRACKET NUMBER COLON NUMBER RBRACKET;

mapUpdate : '::=' expr;
idsType : id (COMMA id)* COLON type;

type : typeAtom | mapType | id typeCtorArgs?;
typeAtom : KW_BOOL | KW_INT | KW_BV NUMBER | LPAREN type RPAREN;
mapType : typeArgs? LBRACKET type (COMMA type)* RBRACKET type;
typeCtorArgs : typeAtom typeCtorArgs? | id typeCtorArgs? | mapType;
typeArgs : LANGLE id (COMMA id)* RANGLE;
funcApplication : LPAREN (expr (COMMA expr)*)? RPAREN;

trigAttr : trigger | attribute;

// Bitvector
bitVector : NUMBER KW_BV NUMBER;

trigger : LCURLY expr+ RCURLY;
attribute : LCURLY COLON ID attrArg* RCURLY;
attrArg : expr | STRING;

id : IDENTIFIER;

COMMA : ',';
COLON : ':';
EQUIV_OP : '<==>';
IMPL_OP : '==>';
OR_OP : '||';
AND_OP : '&&';
relOp : '==' | '!=' | LANGLE | RANGLE | '<=' | '>=' | '<:';

CONCAT_OP : '++';
ADD_OP : '+' | '-';
MUL_OP : '*' | '/' | '%';
UN_OP : '!' | '-';
QOP : KW_FORALL | KW_EXISTS;
QSEP : '::';

KW_INT : 'int';
KW_FORALL : 'forall';
KW_EXISTS : 'exists';
KW_BOOL : 'bool';

KW_BV : 'bv';

LPAREN : '(';
RPAREN : ')';
LCURLY : '{';
RCURLY : '}';
LANGLE : '<';
RANGLE: '>';
LBRACKET : '[';
RBRACKET : ']';
KW_FALSE : 'false';
KW_TRUE : 'true';
KW_OLD : 'old';
NUMBER : [0-9]+;
MAPSTO : '->';
// Technically this can support all non-english unicode letters as well
// and explicitly supports the following symbols [-, ., $, @, ', `, ~, ^, \, ?].

// Strings
ESCAPE : '\\' ( '"' | '\\' | 'n' | 'x' | '.');
STRING :  '"' ( ESCAPE | ~('"' | '\\' | '\n' | '\r') )+ '"' ;

// Ignored
NEWLINE : '\r'? '\n' -> skip;
WHITESPACE : ' '+ -> skip;
COMMENT : '//' ~[\r\n]* -> skip;
