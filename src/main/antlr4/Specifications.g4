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
directFunction: MEMORY_LOAD_DIRECT #memoryLoad
              | MEMORY_STORE_DIRECT #memoryStore
              | GAMMA_LOAD_DIRECT #gammaLoad
              | GAMMA_STORE_DIRECT #gammaStore
              | ZERO_EXTEND_DIRECT #zeroExtend
              | SIGN_EXTEND_DIRECT #signExtend
              | BVOP_DIRECT #bvOp
              ;

subroutine: 'Subroutine:' id modifies? requires* ensures* relies? guarantees?;
modifies: 'Modifies:' id (COMMA id)*;
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
          | NOT_OP unaryExpr #notExpr
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

BVOP_DIRECT: BV OPNAME DIGIT+;

MEMORY_LOAD_DIRECT: MEMORY_LOAD DIGIT+ ENDIAN;
MEMORY_STORE_DIRECT: MEMORY_STORE DIGIT+ ENDIAN;
GAMMA_LOAD_DIRECT: GAMMA_LOAD DIGIT+;
GAMMA_STORE_DIRECT: GAMMA_STORE DIGIT+;
ZERO_EXTEND_DIRECT: ZERO_EXTEND DIGIT+ UNDERSCORE DIGIT+;
SIGN_EXTEND_DIRECT: SIGN_EXTEND DIGIT+ UNDERSCORE DIGIT+;

fragment MEMORY_LOAD: 'memory_load';
fragment MEMORY_STORE: 'memory_store';
fragment GAMMA_LOAD: 'gamma_load';
fragment GAMMA_STORE: 'gamma_store';
fragment ZERO_EXTEND: 'zero_extend';
fragment SIGN_EXTEND: 'sign_extend';

ENDIAN: LE | BE;

fragment BV: 'bv';

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

BVSIZE: BV DIGIT+;

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
NOT_OP : '!';

COLON: ':';
fragment UNDERSCORE: '_';
fragment LE: '_le';
fragment BE: '_be';

fragment OPNAME : AND
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

fragment AND : 'and';
fragment OR : 'or';
fragment ADD : 'add';
fragment MUL : 'mul';
fragment UDIV : 'udiv';
fragment UREM : 'urem';
fragment SHL : 'shl';
fragment LSHR : 'lshr';
fragment ULT : 'ult';
fragment NAND : 'nand';
fragment NOR : 'nor';
fragment XOR : 'xor';
fragment XNOR : 'xnor';
fragment SUB : 'sub';
fragment SREM : 'srem';
fragment SDIV : 'sdiv';
fragment SMOD : 'smod';
fragment ASHR : 'ashr';
fragment ULE : 'ule';
fragment UGT : 'ugt';
fragment UGE : 'uge';
fragment SLT : 'slt';
fragment SLE : 'sle';
fragment SGT : 'sgt';
fragment SGE : 'sge';
fragment COMP : 'comp';

// Ignored
NEWLINE : '\r'? '\n' -> skip;
WHITESPACE : ' '+ -> skip;
COMMENT : '//' ~[\r\n]* -> skip;