grammar Semantics;

// See aslp/libASL/asl.ott for reference grammar Bap-ali-plugin/asli_lifer.ml may also be useful for
// visitors

statements: QUOTE stmt QUOTE (COMMA QUOTE stmt QUOTE)*;

stmt: 'Stmt_Assign' OPEN_PAREN lexpr COMMA expr CLOSE_PAREN # Assign
    | 'Stmt_ConstDecl' OPEN_PAREN type COMMA lvar=ID COMMA expr CLOSE_PAREN    # ConstDecl
    | 'Stmt_VarDecl' OPEN_PAREN type COMMA lvar=ID COMMA expr CLOSE_PAREN # VarDecl
    | 'Stmt_VarDeclsNoInit' OPEN_PAREN type COMMA OPEN_BRACKET lvars CLOSE_BRACKET CLOSE_PAREN # VarDeclsNoInit
    | 'Stmt_Assert' OPEN_PAREN expr CLOSE_PAREN # Assert
    | 'Stmt_TCall' OPEN_PAREN name=ID COMMA OPEN_BRACKET tes=exprs? CLOSE_BRACKET COMMA OPEN_BRACKET args=exprs? CLOSE_BRACKET CLOSE_PAREN # TCall
    | 'Stmt_If' OPEN_PAREN cond=expr COMMA OPEN_BRACKET stmt+ CLOSE_BRACKET COMMA OPEN_BRACKET CLOSE_BRACKET COMMA elseStmt? CLOSE_PAREN # If // theoretically some sort of 'Elsif' could be within the middle brackets?
    | 'Stmt_Throw' OPEN_PAREN str=ID* CLOSE_PAREN # Throw
    ;

lvars: OPEN_PAREN ID CLOSE_PAREN (SCOLON OPEN_PAREN ID CLOSE_PAREN)* ;

exprs: OPEN_PAREN expr CLOSE_PAREN (SCOLON OPEN_PAREN expr CLOSE_PAREN)*;

elseStmt: OPEN_PAREN 'else' stmt+ CLOSE_PAREN;

type: 'Type_Bits' OPEN_PAREN size=expr CLOSE_PAREN # TypeBits
    | 'Type_Constructor' OPEN_PAREN str=ID CLOSE_PAREN # TypeConstructor
    | 'Type_Register' OPEN_PAREN QUOTE size=(DEC | BINARY) QUOTE COMMA regfield (COMMA regfield)* CLOSE_PAREN # TypeRegister
    ;

regfield: OPEN_PAREN OPEN_BRACKET slice CLOSE_BRACKET COMMA id=ID CLOSE_PAREN;

lexpr: 'LExpr_Var' OPEN_PAREN ID CLOSE_PAREN                        # LExprVar
     | 'LExpr_Field' OPEN_PAREN lexpr COMMA field=ID CLOSE_PAREN        # LExprField
     | 'LExpr_Array' OPEN_PAREN lexpr COMMA index=expr CLOSE_PAREN    # LExprArray
     ;

expr: 'Expr_Var' OPEN_PAREN ID CLOSE_PAREN # ExprVar
    | 'Expr_TApply' OPEN_PAREN ID COMMA OPEN_BRACKET tes=exprs? CLOSE_BRACKET COMMA OPEN_BRACKET args=exprs? CLOSE_BRACKET CLOSE_PAREN # ExprTApply
    | 'Expr_Slices' OPEN_PAREN expr COMMA OPEN_BRACKET slices CLOSE_BRACKET CLOSE_PAREN # ExprSlices
    | 'Expr_Field' OPEN_PAREN expr COMMA field=ID CLOSE_PAREN # ExprField
    | 'Expr_Array' OPEN_PAREN array=expr COMMA index=expr CLOSE_PAREN # ExprArray
    | 'Expr_LitInt' OPEN_PAREN QUOTE value=(DEC | BINARY) QUOTE CLOSE_PAREN # ExprLitInt
    | 'Expr_LitBits' OPEN_PAREN QUOTE value=BINARY QUOTE CLOSE_PAREN # ExprLitBits // in future may need to account for case where whitespace is in the binary string
    ;

slice: 'Slice_LoWd' OPEN_PAREN lo=expr COMMA wd=expr CLOSE_PAREN # Slice_LoWd
     | 'Slice_HiLo' OPEN_PAREN hi=expr COMMA lo=expr CLOSE_PAREN # Slice_HiLo
     ;

// multiple of 'slice' - distinct from Expr_Slices
slices: OPEN_PAREN slice CLOSE_PAREN (SCOLON OPEN_PAREN slice CLOSE_PAREN)*;

BINARY: [0-1]+;
DEC: [0-9]+;
ID: [a-zA-Z_][a-zA-Z0-9_.]*;

// Delimiters
OPEN_PAREN: '(';
CLOSE_PAREN: ')';
COMMA: ',';
OPEN_BRACKET: '[';
CLOSE_BRACKET: ']';
QUOTE: '"';
SCOLON: ';';

// Ignored
NEWLINE: ('\r\n' | '\n') -> skip;
WHITESPACE: ' '+ -> skip;
COMMENT: '//' ~[\r\n]* -> skip;