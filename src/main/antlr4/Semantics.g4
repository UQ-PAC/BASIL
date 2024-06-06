grammar Semantics;

// See aslp/libASL/asl.ott for reference grammar Bap-ali-plugin/asli_lifer.ml may also be useful for
// visitors

stmt: 'Stmt_Assign' OPEN_PAREN lexpr COMMA expr CLOSE_PAREN # Assign
    | 'Stmt_ConstDecl' OPEN_PAREN type COMMA lvar=ident COMMA expr CLOSE_PAREN    # ConstDecl
    | 'Stmt_VarDecl' OPEN_PAREN type COMMA lvar=ident COMMA expr CLOSE_PAREN # VarDecl
    | 'Stmt_VarDeclsNoInit' OPEN_PAREN type COMMA OPEN_BRACKET lvars CLOSE_BRACKET CLOSE_PAREN # VarDeclsNoInit
    | 'Stmt_Assert' OPEN_PAREN expr CLOSE_PAREN # Assert
    | 'Stmt_TCall' OPEN_PAREN name=ident COMMA OPEN_BRACKET tes=exprs? CLOSE_BRACKET COMMA OPEN_BRACKET args=exprs? CLOSE_BRACKET CLOSE_PAREN # TCall
    | 'Stmt_If' OPEN_PAREN cond=expr COMMA OPEN_BRACKET thenStmts=stmts? CLOSE_BRACKET COMMA OPEN_BRACKET CLOSE_BRACKET COMMA OPEN_BRACKET elseStmts=stmts? CLOSE_BRACKET CLOSE_PAREN # If // ASLp transforms else-if into nested if/else statements.
    | 'Stmt_Throw' OPEN_PAREN str=ident* CLOSE_PAREN # Throw
    ;

statements: QUOTE stmt QUOTE (COMMA QUOTE stmt QUOTE)*;

lvars: ident (SCOLON ident)*;

exprs: expr (SCOLON expr)*;

stmts: stmt (SCOLON stmt)*;

type: 'Type_Bits' OPEN_PAREN size=expr CLOSE_PAREN # TypeBits
    | 'Type_Constructor' OPEN_PAREN str=ident CLOSE_PAREN # TypeConstructor
    | 'Type_Register' OPEN_PAREN QUOTE size=integer QUOTE COMMA regfield (COMMA regfield)* CLOSE_PAREN # TypeRegister
    ;

regfield: OPEN_PAREN OPEN_BRACKET slice CLOSE_BRACKET COMMA id=ident CLOSE_PAREN;

lexpr: 'LExpr_Var' OPEN_PAREN ident CLOSE_PAREN                        # LExprVar
     | 'LExpr_Field' OPEN_PAREN lexpr COMMA field=ident CLOSE_PAREN        # LExprField
     | 'LExpr_Array' OPEN_PAREN lexpr COMMA index=expr CLOSE_PAREN    # LExprArray
     ;

expr: 'Expr_Var' OPEN_PAREN ident CLOSE_PAREN # ExprVar
    | 'Expr_TApply' OPEN_PAREN ident COMMA OPEN_BRACKET tes=exprs? CLOSE_BRACKET COMMA OPEN_BRACKET args=exprs? CLOSE_BRACKET CLOSE_PAREN # ExprTApply
    | 'Expr_Slices' OPEN_PAREN expr COMMA OPEN_BRACKET slices CLOSE_BRACKET CLOSE_PAREN # ExprSlices
    | 'Expr_Field' OPEN_PAREN expr COMMA field=ident CLOSE_PAREN # ExprField
    | 'Expr_Array' OPEN_PAREN array=expr COMMA index=expr CLOSE_PAREN # ExprArray
    | integer # ExprLitInt
    | bits # ExprLitBits
    ;

// Slice_HiLo only ever appears within Type_Register fields
slice: 'Slice_LoWd' OPEN_PAREN lo=expr COMMA wd=expr CLOSE_PAREN # Slice_LoWd
     | 'Slice_HiLo' OPEN_PAREN hi=expr COMMA lo=expr CLOSE_PAREN # Slice_HiLo
     ;

// multiple of 'slice' - distinct from Expr_Slices
slices: slice (SCOLON slice)*;

ident: QUOTE ID QUOTE;

integer: DEC;
bits: BINARY; // in future may need to account for case where whitespace is in the binary string

BINARY: SQUOTE [0-1]+ SQUOTE;
DEC: [0-9]+;
ID: [a-zA-Z_][a-zA-Z0-9_.]*;

// Delimiters
OPEN_PAREN: '(';
CLOSE_PAREN: ')';
COMMA: ',';
OPEN_BRACKET: '[';
CLOSE_BRACKET: ']';
QUOTE: '"';
SQUOTE: '\'';
SCOLON: ';';

// Ignored
NEWLINE: ('\r\n' | '\n') -> skip;
WHITESPACE: ' '+ -> skip;
COMMENT: '//' ~[\r\n]* -> skip;