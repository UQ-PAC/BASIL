grammar Specifications;

specification: globals? lPreds? relies? guarantees? directFunctions? subroutine*;

globals: 'Globals:' globalDef*;
globalDef: id COLON typeName arraySize?;
lPreds: 'L:' lPred (COMMA lPred)*;
lPred: id MAPSTO expr;
typeName: ('bv' size=nat) #bvType
        | LONG #longType
        | SHORT #shortType
        | INT #intType
        | CHAR #charType
        ;
arraySize: '[' size=nat ']';
//gammaInits : 'Gamma:' gamma (COMMA gamma)*;
//gamma : id MAPSTO boolLit;
//inits : 'Init:' init (COMMA init)*;
//init : id MAPSTO nat;
//lattice: 'Lattice:' lattice_elem (COMMA lattice_elem)* ;



relies: 'Rely:' expr (COMMA expr)*;
guarantees: 'Guarantee:' expr (COMMA expr)*;

directFunctions: 'DIRECT functions:' directFunction (COMMA directFunction)*;
directFunction: 'memory_load' size=nat endian #memoryLoad
              | 'memory_store' size=nat endian #memoryStore
              | 'gamma_load' size=nat #gammaLoad
              | 'gamma_store' size=nat #gammaStore
              | 'zero_extend' size1=nat '_' size2=nat #zeroExtend
              | 'sign_extend' size1=nat '_' size2=nat #signExtend
              | 'bv' OPNAME size=nat #bvOp
              ;

/*
directFunction: memoryLoad
              | memoryStore
              | gammaLoad
              | gammaStore
              | zeroExtend
              | signExtend
              | bvOp
              ;
memoryLoad: 'memory_load' size=nat endian;
memoryStore: 'memory_store' size=nat endian;
gammaLoad: 'gamma_load' size=nat;
gammaStore: 'gamma_store' size=nat;
zeroExtend: 'zero_extend' size1=nat '_' size2=nat;
signExtend: 'sign_extend' size1=nat '_' size2=nat;
bvOp: 'bv' OPNAME size=nat;
*/

subroutine: 'Subroutine:' id requires* ensures*;
requires: 'Requires:' expr #parsedRequires
        | 'Requires DIRECT:' QUOTESTRING #directRequires
        ;
ensures: 'Ensures:' expr #parsedEnsures
       | 'Ensures DIRECT:' QUOTESTRING #directEnsures
       ;

QUOTE : '"';
QUOTESTRING : QUOTE (~( '"' | '\n' | '\r'))+ QUOTE;
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

arrayAccess: id '[' nat ']';
gammaId : 'Gamma_' id;
id : NON_DIGIT ( NON_DIGIT | DIGIT )*;
NON_DIGIT : ( [A-Z] | [a-z] | '\'' | '~' | '#' | '$' | '^' | '_' | '.' | '?' | '`') ;
DIGIT : [0-9] ;
nat: (DIGIT)+ ;
bv: value=nat 'bv' size=nat;
LONG: 'long';
SHORT: 'short';
INT: 'int';
CHAR: 'char';
COLON: ':';

endian: LE | BE;

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
         | gammaId #gammaIdExpr
         | arrayAccess #arrayAccessExpr
         | 'old' LPAREN expr RPAREN #oldExpr
         | LPAREN expr RPAREN #parenExpr
         | 'if' guard=expr 'then' thenExpr=expr 'else' elseExpr=expr #ifThenElseExpr
         ;

// Ignored
NEWLINE : '\r'? '\n' -> skip;
WHITESPACE : ' '+ -> skip;
COMMENT : '//' ~[\r\n]* -> skip;