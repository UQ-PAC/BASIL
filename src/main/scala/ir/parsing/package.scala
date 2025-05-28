package ir

/**
 * Welcome to the [[ir.parsing]] package. This package concerns parsing
 * the serialised Basil IR format (specifically, that produced by
 * [[translating.PrettyPrinter]]). Primarily, this contains visitors which
 * traverse a parse tree produced by the BNFC Java-Antlr backend.
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
 * memory mem : map bv8[bv64];
 *
 * let entry_procedure = main_1924;
 *
 * proc get_two_1876(R0_in:bv64, R10_in:bv64, R11_in:bv64, R12_in:bv64, R13_in:bv64, R14_in:bv64, R15_in:bv64, R16_in:bv64, R17_in:bv64, R18_in:bv64, R1_in:bv64, R29_in:bv64, R2_in:bv64, R30_in:bv64, R31_in:bv64, R3_in:bv64, R4_in:bv64, R5_in:bv64, R6_in:bv64, R7_in:bv64, R8_in:bv64, R9_in:bv64) -> (R0_out:bv64, R1_out:bv64, R31_out:bv64)
 * {
 *   name = "get_two";
 *   address = 0x754;
 *   entry_block = "lget_two";
 *   blocks = [
 *     block lget_two {address = 0x754} [
 *       var R0_1: bv64 := R0_in:bv64;
 *       var R10_1: bv64 := R10_in:bv64;
 *       var R11_1: bv64 := R11_in:bv64;
 *       goto(lget_two_phi_get_two_1876_basil_return)
 *     ];
 *     block lget_two_phi_get_two_1876_basil_return [
 *       var R1_4: bv64 := R1_3:bv64;
 *       var R0_6: bv64 := R0_5:bv64;
 *       var R31_4: bv64 := R31_3:bv64;
 *       goto(get_two_1876_basil_return)
 *     ];
 *     block get_two_1876_basil_return [
 *       return (R0_6:bv64, R1_4:bv64, R31_4:bv64)
 *     ]
 *   ];
 * };
 * ```
 *
 * # Overview
 *
 * The parsing process is a **two-stage** parser:
 *
 * - The first stage is a lightweight "early" stage to identify global declarations
 * (most importantly, procedure signatures and their in/out parameters).
 *
 * - The second stage is the main stage. Given the declarations from the first stage,
 *   this produces a Basil IR DSL program ([[ir.dsl]]), ready to be resolved into
 *   a complete Basil IR program.
 *
 * ## Step-by-step
 *
 * 1. A file or string is read into an [[org.antlr.v4.runtime.CharStream]].
 * 2. This is lexed by [[basil_ir.BasilIRLexer]].
 * 3. This is passed to a [[org.antlr.v4.runtime.CommonTokenStream]] and parsed by [[basil_ir.BasilIRParser]].
 *    BNFC sets up the Antlr visitor to generate an AST. From the parser,  we obtain a [[basil_ir.Absyn.Program]].
 * 4. Global declarations are obtained by passing the program AST to [[ir.parsing.EarlyBasilBNFCVisitor]].
 *    This results in a [[ir.parsing.Declarations]].
 * 5. The Basil IR program is obtained by passing the program AST,
 *    along with the global declarations, to [[ir.parsing.MainBasilBNFCVisitor]].
 *    This is returned in the form of an unresolved Basil IR structure.
 *
 */
package object parsing {}
