package ir

/**
 * Welcome to the [[ir.parsing]] package. This package concerns parsing
 * the serialised Basil IR format (specifically, that produced by
 * [[translating.PrettyPrinter]]). Primarily, this contains visitors which
 * traverse a parse tree produced by the BNFC Java backend.
 *
 * # Example
 *
 * This is an example of the IR format which is recognised by this parser.
 *
 * Note that there can be a fair amount of variation in content of the IR.
 * For instance, this example has the "parameter form" transformation applied,
 * so procedures have a list of in- and out- parameters. Without this transformation,
 * the `proc` header will have empty parameter lists `() -> ()` and there will
 * be global variable declarations for registers.
 *
 * ```
 * memory shared $mem : (bv64 -> bv8);
 *
 * prog entry @main_1860;
 *
 * proc @main_1860
 *   (R0_in:bv64, R1_in:bv64, R29_in:bv64, R30_in:bv64, R31_in:bv64, R8_in:bv64, _PC_in:bv64)
 *     -> (R0_out:bv64, R29_out:bv64, R30_out:bv64, R8_out:bv64, _PC_out:bv64)
 *   { .name = "main"; .address = 0x744 }
 *   require eq(_PC_in:bv64, 0x744:bv64);
 *   ensures eq(_PC_out:bv64, R30_in:bv64);
 * [
 *   block %main_entry {.address = 0x744; .originalLabel = "rboiQVDCQyC6FKdIfNmCPw=="} [
 *     assert eq(_PC_in:bv64, 0x744:bv64) { .label = "pc-tracking"; .comment = "pc-tracking";  };
 *     store le $mem bvadd(R31_in:bv64, 0xfffffffffffffff0:bv64) R29_in:bv64 64;
 *     store le $mem bvadd(R31_in:bv64, 0xfffffffffffffff8:bv64) R30_in:bv64 64;
 *     store le $mem bvadd(R31_in:bv64, 0xffffffffffffffec:bv64) 0x0:bv32 32;
 *     store le $mem bvadd(R31_in:bv64, 0xffffffffffffffe8:bv64) extract(32, 0, R0_in:bv64) 32;
 *     var load15_1: bv32 := load le $mem bvadd(R31_in:bv64, 0xffffffffffffffe8:bv64) 32;
 *     store le $mem bvadd(R31_in:bv64, 0xffffffffffffffdc:bv64) load15_1:bv32 32;
 *     goto(%phi_1, %phi_2);
 *   ];
 * ]
 * ```
 *
 * # Overview
 *
 * The parsing process is a **two-stage** parser:
 *
 * - The first stage is a lightweight "early" stage to identify global declarations
 *   (most importantly, procedure signatures and their in/out parameters).
 *
 * - The second stage is the main stage. Given the declarations from the first stage,
 *   this produces a Basil IR DSL program ([[ir.dsl]]), ready to be resolved into
 *   a complete Basil IR program.
 *
 * See [[ir.parsing.ParseBasilIL]] for various entry points which can load Basil IL
 * from various data sources.
 *
 * ## Step-by-step
 *
 * 1. A file or string is read into an [[java.io.FileReader]].
 * 2. This is lexed by [[basil_ir.Yylex]].
 * 3. The tokens are parsed by [[basil_ir.parser]].
 *    BNFC sets up the parser to produce an AST, so we obtain a [[basil_ir.Absyn.Module]] from the parser.
 * 4. Global declarations are obtained by passing the program AST to [[ir.parsing.BasilEarlyBNFCVisitor]].
 *    This results in a [[ir.parsing.Declarations]].
 * 5. The Basil IR program is obtained by passing the program AST,
 *    along with the global declarations, to [[ir.parsing.BasilMainBNFCVisitor]].
 *    The main visitor dispatches to [[ir.parsing.BlockBNFCVisitor]], [[ir.parsing.ExprBNFCVisitor]],
 *    and others as needed.
 *    The visitor returns an unresolved Basil DSL structure, [[ir.dsl.EventuallyProgram]].
 * 6. In [[ir.parsing.ParseBasilIL.makeBasilIRContext]], the DSL structure is resolved into a
 *    real Basil IR [[ir.Program]] and combined with the [[ir.parsing.Declarations]]
 *    to produce a [[util.IRContext]].
 *
 */
package object parsing {}
