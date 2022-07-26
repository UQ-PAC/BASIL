grammar Specifications;

specification: lPreds? gammaInits? relies? guarantees?;

lPreds : 'L:' lPred (COMMA lPred)*;
lPred : id MAPSTO expr;
gammaInits : 'GAMMA:' gamma (COMMA gamma)*;
gamma : id MAPSTO boolLit;
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
boolLit : TRUE | FALSE;

gammaId : 'Gamma_' id;
id : NON_DIGIT ( NON_DIGIT | DIGIT )*;
NON_DIGIT : ( [A-Z] | [a-z] | '\'' | '~' | '#' | '$' | '^' | '_' | '.' | '?' | '`') ;
DIGIT : [0-9] ;

// based upon boogie grammar: https://boogie-docs.readthedocs.io/en/latest/LangRef.html#grammar
expr : impliesExpr ( EQUIV_OP impliesExpr )* ;
impliesExpr : arg1=logicalExpr ( IMPLIES_OP arg2=impliesExpr )? ;
logicalExpr : relExpr ( AND_OP relExpr ( AND_OP relExpr )* | OR_OP relExpr ( OR_OP relExpr )* )? ;
relExpr : arg1=term ( op=relOp arg2=term )? ;
relOp : EQ_OP | LT_OP | GT_OP | LE_OP | GE_OP | NEQ_OP;
addSubOp : ADD_OP | SUB_OP;
term : arg1=factor ( op=addSubOp arg2=factor )? ;
mulDivModOp: MUL_OP | DIV_OP | MOD_OP;
factor : arg1=unaryExpr ( op=mulDivModOp arg2=unaryExpr )? ;

unaryExpr : atomExpr #atomUnaryExpr
          | SUB_OP unaryExpr #negExpr
          ;

atomExpr : boolLit #boolLitExpr
         | (DIGIT)+ #nat
         | id #idExpr
         | gammaId #gammaIdExpr
         | 'old' LPAREN expr RPAREN #oldExpr
         | LPAREN expr RPAREN #parenExpr
         | 'if' guard=expr 'then' thenExpr=expr 'else' elseExpr=expr #ifThenElseExpr
         ;

// Ignored
NEWLINE : '\r'? '\n' -> skip;
WHITESPACE : ' '+ -> skip;
COMMENT : '//' ~[\r\n]* -> skip;