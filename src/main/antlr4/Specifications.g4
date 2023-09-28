grammar Specifications;

specification: globals? lPreds? relies? guarantees? directFunctions? subroutine* EOF;

globals: 'Globals:' globalDef*;
globalDef: id COLON typeName arraySize?;
lPreds: 'L:' lPred (COMMA lPred)*;
lPred: id MAPSTO expr;
typeName: BVSIZE #bvType
        | LONG #longType
        | SHORT #shortType
        | INT #intType
        | CHAR #charType
        ;
arraySize: '[' size=nat ']';

relies: 'Rely:' expr (COMMA expr)*;
guarantees: 'Guarantee:' expr (COMMA expr)*;

directFunctions: 'DIRECT functions:' directFunction (COMMA directFunction)*;
directFunction: 'memory_load' size=nat endian #memoryLoad
              | 'memory_store' size=nat endian #memoryStore
              | 'gamma_load' size=nat #gammaLoad
              | 'gamma_store' size=nat #gammaStore
              | 'zero_extend' size1=nat UNDERSCORE size2=nat #zeroExtend
              | 'sign_extend' size1=nat UNDERSCORE size2=nat #signExtend
              | BV OPNAME size=nat #bvOp
              ;

subroutine: 'Subroutine:' id requires* ensures*;
requires: 'Requires:' expr #parsedRequires
        | 'Requires DIRECT:' QUOTESTRING #directRequires
        ;
ensures: 'Ensures:' expr #parsedEnsures
       | 'Ensures DIRECT:' QUOTESTRING #directEnsures
       ;

boolLit : TRUE | FALSE;

arrayAccess: id '[' nat ']';
id : ID;
nat: (DIGIT)+ ;
bv: value=nat BVSIZE;
endian: LE | BE;

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
         | bv #bvExpr
         | id #idExpr
         | arrayAccess #arrayAccessExpr
         | OLD LPAREN expr RPAREN #oldExpr
         | LPAREN expr RPAREN #parenExpr
         | IF guard=expr THEN thenExpr=expr ELSE elseExpr=expr #ifThenElseExpr
         ;

QUOTE : '"';
QUOTESTRING : QUOTE (~( '"' | '\n' | '\r'))+ QUOTE;

BV: 'bv';

LONG: 'long';
SHORT: 'short';
INT: 'int';
CHAR: 'char';

TRUE : 'true';
FALSE : 'false';

OLD: 'old';
IF: 'if';
THEN: 'then';
ELSE: 'else';

DIV_OP : 'div';
MOD_OP : 'mod';

BVSIZE: BV DIGIT DIGIT?;

ID : NON_DIGIT ( NON_DIGIT | DIGIT )* ;
NON_DIGIT : ( [A-Z] | [a-z] | '\'' | '~' | '#' | '$' | '^' | '_' | '.' | '?' | '`') ;
DIGIT : [0-9];

COMMA : ',';

LPAREN : '(';
RPAREN : ')';
MAPSTO : '->';

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

COLON: ':';
UNDERSCORE: '_';
LE: '_le';
BE: '_be';



OPNAME : AND
       | OR
       | ADD
       | MUL
       | UDIV
       | UREM
       | SHL
       | LSHR
       | ULT
       | NAND
       | NOR
       | XOR
       | XNOR
       | SUB
       | SREM
       | SDIV
       | SMOD
       | ASHR
       | ULE
       | UGT
       | UGE
       | SLT
       | SLE
       | SGT
       | SGE
       | COMP
       ;

AND : 'and';
OR : 'or';
ADD : 'add';
MUL : 'mul';
UDIV : 'udiv';
UREM : 'urem';
SHL : 'shl';
LSHR : 'lshr';
ULT : 'ult';
NAND : 'nand';
NOR : 'nor';
XOR : 'xor';
XNOR : 'xnor';
SUB : 'sub';
SREM : 'srem';
SDIV : 'sdiv';
SMOD : 'smod';
ASHR : 'ashr';
ULE : 'ule';
UGT : 'ugt';
UGE : 'uge';
SLT : 'slt';
SLE : 'sle';
SGT : 'sgt';
SGE : 'sge';
COMP : 'comp';

// Ignored
NEWLINE : '\r'? '\n' -> skip;
WHITESPACE : ' '+ -> skip;
COMMENT : '//' ~[\r\n]* -> skip;