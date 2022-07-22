grammar Specifications;

specification: (lPreds | gammaInits | relies | guarantees)*;

lPreds : 'L:' lPred (COMMA lPred)*;
lPred : var=id MAPSTO expr;
gammaInits : 'GAMMA:' gamma (COMMA gamma)*;
gamma : var=id MAPSTO boolLit;
//lattice: 'Lattice:' lattice_elem (COMMA lattice_elem)* ;

relies: 'Rely:' expr (COMMA expr)*;
guarantees: 'Guarantee:' expr (COMMA expr)*;

COMMA : ',';

LPAREN : '(';
RPAREN : ')';
MAPSTO : '->';

TRUE : 'true';
FALSE : 'false';

EQUIV_OP : '<==>';
IMPLIES_OP : '==>';
OR_OP : '||';
AND_OP : '&&';
EQ_OP : '==';
NEQ_OP : '!=';
LT_OP : '<';
LE_OP : '<=';
GT_OP : '>';
GE_OP : '>=';
ADD_OP : '+';
SUB_OP : '-';
MUL_OP : '*';
DIV_OP : 'div';
MOD_OP : 'mod';

id : NON_DIGIT ( NON_DIGIT | DIGIT )* ;
NON_DIGIT : ( [A-Z] | [a-z] | '\'' | '~' | '#' | '$' | '^' | '_' | '.' | '?' | '`') ;
DIGIT : [0-9] ;

expr : impliesExpr ( EQUIV_OP impliesExpr )* ;
impliesExpr : logicalExpr ( IMPLIES_OP impliesExpr )? ;
logicalExpr : relExpr ( AND_OP relExpr ( AND_OP relExpr )* | OR_OP relExpr ( OR_OP relExpr )* )? ;
relExpr : term ( relOp term )? ;
relOp : ( EQ_OP | LT_OP | GT_OP | LE_OP | GE_OP | NEQ_OP ) ;
term : factor ( ( ADD_OP | SUB_OP ) factor )? ;
factor : unaryExpr ( ( MUL_OP | DIV_OP | MOD_OP ) unaryExpr )? ;
unaryExpr : ( SUB_OP unaryExpr | atomExpr );
atomExpr : ( boolLit | nat | id | oldExpr | parenExpr | ifThenElseExpr );
oldExpr : 'old' LPAREN expr RPAREN;
parenExpr : LPAREN expr RPAREN;
ifThenElseExpr : 'if' expr 'then' expr 'else' expr;
nat : (DIGIT)+ ;
boolLit : TRUE | FALSE ;

// Ignored
NEWLINE : '\r'? '\n' -> skip;
WHITESPACE : ' '+ -> skip;
COMMENT : '//' ~[\r\n]* -> skip;