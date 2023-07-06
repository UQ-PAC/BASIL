grammar BilAdt; 

semantics : OPEN_CURLY (basic_blk (COMMA basic_blk)*)? CLOSE_CURLY EOF;

basic_blk : QUOTE uuid QUOTE COLON OPEN_BRACKET (instruction (COMMA instruction)*)? CLOSE_BRACKET; 

instruction : OPEN_BRACKET (stmt_string (COMMA stmt_string)*)? CLOSE_BRACKET;

stmt_string : QUOTE stmt QUOTE;

stmt : assignment_stmt
     | call_stmt
     | conditional_stmt
     ;

assignment_stmt : 'Stmt_Assign' OPEN_PAREN lexpr COMMA expr CLOSE_PAREN
                | 'Stmt_ConstDecl' OPEN_PAREN type COMMA METHOD COMMA expr CLOSE_PAREN
                ;

call_stmt : 'Stmt_TCall' OPEN_PAREN (SSYMBOL | METHOD) (COMMA (SSYMBOL | METHOD))* COMMA OPEN_BRACKET (OPEN_PAREN expr CLOSE_PAREN (SCOLON OPEN_PAREN expr CLOSE_PAREN)*)? CLOSE_BRACKET
                     COMMA OPEN_BRACKET (OPEN_PAREN expr CLOSE_PAREN (SCOLON OPEN_PAREN expr CLOSE_PAREN)*)? CLOSE_BRACKET CLOSE_PAREN;

conditional_stmt : 'Stmt_If' OPEN_PAREN expr COMMA OPEN_BRACKET (COMMA stmt)* COMMA? CLOSE_BRACKET COMMA OPEN_BRACKET CLOSE_BRACKET COMMA CLOSE_PAREN;

type : 'Type_Bits' OPEN_PAREN expr CLOSE_PAREN;

lexpr : 'LExpr_Var' OPEN_PAREN (SSYMBOL | METHOD) CLOSE_PAREN
      | 'LExpr_Field' OPEN_PAREN lexpr COMMA SSYMBOL CLOSE_PAREN
      | 'LExpr_Array'OPEN_PAREN (lexpr (COMMA expr)*)? CLOSE_PAREN
      ;

expr : literal_expr 
     | 'Expr_Var' OPEN_PAREN (SSYMBOL | METHOD) CLOSE_PAREN 
     | 'Expr_TApply' OPEN_PAREN METHOD (COMMA METHOD)* COMMA OPEN_BRACKET (OPEN_PAREN expr CLOSE_PAREN (SCOLON OPEN_PAREN expr CLOSE_PAREN)*)? CLOSE_BRACKET
                     COMMA OPEN_BRACKET (OPEN_PAREN expr CLOSE_PAREN (SCOLON OPEN_PAREN expr CLOSE_PAREN)*)? CLOSE_BRACKET CLOSE_PAREN
     | 'Expr_Slices' OPEN_PAREN expr COMMA OPEN_BRACKET OPEN_PAREN slice_expr (COMMA slice_expr)* CLOSE_PAREN CLOSE_BRACKET CLOSE_PAREN
     | 'Expr_Field' OPEN_PAREN expr COMMA SSYMBOL CLOSE_PAREN
     | 'Expr_Array' OPEN_PAREN expr (COMMA expr)* CLOSE_PAREN
     ;


slice_expr : 'Slice_LoWd' OPEN_PAREN literal_expr COMMA literal_expr CLOSE_PAREN;


literal_expr : 'Expr_LitInt' OPEN_PAREN SQUOTE (DEC | BINARY) SQUOTE CLOSE_PAREN
             | 'Expr_LitHex' OPEN_PAREN SQUOTE HEXDIGIT+ SQUOTE CLOSE_PAREN
             | 'Expr_LitBits' OPEN_PAREN SQUOTE BINARY SQUOTE CLOSE_PAREN
             | 'Expr_LitMask' OPEN_PAREN SQUOTE BINARY SQUOTE CLOSE_PAREN
             | 'Expr_LitString' OPEN_PAREN SQUOTE SSYMBOL SQUOTE CLOSE_PAREN
             ;

uuid : (VAR | ALPHA+) EQUALS EQUALS;
BINARY : BIN+;
BIN : [0-1];
DEC : DIGIT+;
SSYMBOL : (ALPHA | '_' | '.')+;
VAR : BASE64DIGIT+;
ALPHA : [A-Za-z];
DIGIT : [0-9];
HEXDIGIT : [0-9a-fA-F];
BASE64DIGIT : [0-9a-zA-Z+/];
METHOD : (BASE64DIGIT | '_' | '.')+; 

// Delimiters
OPEN_PAREN : '(';
CLOSE_PAREN : ')';
COMMA : ',';
OPEN_BRACKET : '[';
CLOSE_BRACKET : ']';
OPEN_CURLY : '{';
CLOSE_CURLY : '}';
SQUOTE : '\'';
QUOTE : '"';
EQUALS : '=';
COLON : ':';
SCOLON : ';';

// // quoteStrings
// ESCAPE : '\\' ( QUOTE | '\\' | 'n' | '.');
// STRING : ( ESCAPE | ~('"' | '\\' | '\n' | '\r') )+ ;

// Ignored
NEWLINE : ('\r\n' | '\n') -> skip;
WHITESPACE : ' '+ -> skip;
COMMENT : '//' ~[\r\n]* -> skip;
