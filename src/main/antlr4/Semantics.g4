grammar Semantics;

// See aslp/libASL/asl.ott for reference grammar Bap-ali-plugin/asli_lifer.ml may also be useful for
// visitors
semantics:
	OPEN_CURLY (basic_blk (COMMA basic_blk)*)? CLOSE_CURLY EOF;

basic_blk:
	QUOTE uuid QUOTE COLON OPEN_BRACKET (
		instruction (COMMA instruction)*
	)? CLOSE_BRACKET;

instruction:
	OPEN_BRACKET (stmt_string (COMMA stmt_string)*)? CLOSE_BRACKET;

stmt_string: QUOTE stmt QUOTE;

stmt: assignment_stmt | call_stmt | conditional_stmt;

assignment_stmt:
	'Stmt_Assign' OPEN_PAREN lexpr COMMA expr CLOSE_PAREN					# Assign
	| 'Stmt_ConstDecl' OPEN_PAREN type COMMA METHOD COMMA expr CLOSE_PAREN	# ConstDecl;

call_stmt:
	'Stmt_TCall' OPEN_PAREN (SSYMBOL | METHOD) (
		COMMA (SSYMBOL | METHOD)
	)* COMMA OPEN_BRACKET (
		OPEN_PAREN expr CLOSE_PAREN (
			SCOLON OPEN_PAREN expr CLOSE_PAREN
		)*
	)? CLOSE_BRACKET COMMA OPEN_BRACKET (
		OPEN_PAREN expr CLOSE_PAREN (
			SCOLON OPEN_PAREN expr CLOSE_PAREN
		)*
	)? CLOSE_BRACKET CLOSE_PAREN;

conditional_stmt:
	'Stmt_If' OPEN_PAREN expr COMMA OPEN_BRACKET (COMMA stmt)* COMMA? CLOSE_BRACKET COMMA //TODO: this will almost certianly fail on complex if statements, so check
		OPEN_BRACKET CLOSE_BRACKET COMMA CLOSE_PAREN;

type: 'Type_Bits' OPEN_PAREN expr CLOSE_PAREN # TypeBits;

lexpr:
	'LExpr_Var' OPEN_PAREN (SSYMBOL | METHOD) CLOSE_PAREN			# LExprVar
	| 'LExpr_Field' OPEN_PAREN lexpr COMMA SSYMBOL CLOSE_PAREN		# LExprField
	| 'LExpr_Array' OPEN_PAREN (lexpr (COMMA expr)*)? CLOSE_PAREN	# LExprArray;

expr:
	'Expr_Var' OPEN_PAREN (SSYMBOL | METHOD) CLOSE_PAREN # ExprVar
	| 'Expr_TApply' OPEN_PAREN METHOD COMMA OPEN_BRACKET (
		OPEN_PAREN targs CLOSE_PAREN (
			SCOLON OPEN_PAREN targs CLOSE_PAREN
		)*
	)? CLOSE_BRACKET COMMA OPEN_BRACKET (
		OPEN_PAREN expr CLOSE_PAREN (
			SCOLON OPEN_PAREN expr CLOSE_PAREN
		)*
	)? CLOSE_BRACKET CLOSE_PAREN # ExprTApply
	| 'Expr_Slices' OPEN_PAREN expr COMMA OPEN_BRACKET OPEN_PAREN slice_expr CLOSE_PAREN
		CLOSE_BRACKET CLOSE_PAREN										# ExprSlices
	| 'Expr_Field' OPEN_PAREN expr COMMA SSYMBOL CLOSE_PAREN			# ExprField
	| 'Expr_Array' OPEN_PAREN expr (COMMA expr)* CLOSE_PAREN			# ExprArray
	| 'Expr_LitInt' OPEN_PAREN SQUOTE (DEC | BINARY) SQUOTE CLOSE_PAREN	# ExprLitInt
	| 'Expr_LitHex' OPEN_PAREN SQUOTE HEXDIGIT+ SQUOTE CLOSE_PAREN		# ExprLitHex
	| 'Expr_LitBits' OPEN_PAREN SQUOTE BINARY SQUOTE CLOSE_PAREN		# ExprLitBits
	| 'Expr_LitMask' OPEN_PAREN SQUOTE BINARY SQUOTE CLOSE_PAREN		# ExprLitMask
	| 'Expr_LitString' OPEN_PAREN SQUOTE SSYMBOL SQUOTE CLOSE_PAREN		# ExprLitString;

targs: expr;

slice_expr: 'Slice_LoWd' OPEN_PAREN expr COMMA expr CLOSE_PAREN;

uuid: (VAR | SSYMBOL) EQUALS EQUALS;
BINARY: BIN+;
BIN: [0-1];
DEC: DIGIT+;
SSYMBOL: (ALPHA | '_' | '.')+;
VAR: BASE64DIGIT+;
ALPHA: [A-Za-z];
DIGIT: [0-9];
HEXDIGIT: [0-9a-fA-F];
BASE64DIGIT: [0-9a-zA-Z+/];
METHOD: (BASE64DIGIT | '_' | '.')+;

// Delimiters
OPEN_PAREN: '(';
CLOSE_PAREN: ')';
COMMA: ',';
OPEN_BRACKET: '[';
CLOSE_BRACKET: ']';
OPEN_CURLY: '{';
CLOSE_CURLY: '}';
SQUOTE: '\'';
QUOTE: '"';
EQUALS: '=';
COLON: ':';
SCOLON: ';';

// // quoteStrings ESCAPE : '\\' ( QUOTE | '\\' | 'n' | '.'); STRING : ( ESCAPE | ~('"' | '\\' |
// '\n' | '\r') )+ ;

// Ignored
NEWLINE: ('\r\n' | '\n') -> skip;
WHITESPACE: ' '+ -> skip;
COMMENT: '//' ~[\r\n]* -> skip;