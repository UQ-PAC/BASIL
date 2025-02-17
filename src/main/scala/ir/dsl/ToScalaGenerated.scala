package ir.dsl

import ir.*

/**
 * The end of this file contains generated code to implement ToScala for the various type
 * hierarchies defined by BASIL.
 */

/**
 * XXX: ATTENTION: auto-generated code!
 * ====================================
 *
 * If you are here to manually fix a compilation error, please make sure that
 * you understand the context and carefully apply a specific fix. Make sure
 * that the code maintains the general contract of the ToScala trait.
 *
 * That is, the returned string must be valid Scala code to construct the given
 * object. The `ensure_constructible` functions are defined to match the produced
 * string. Successful compilation of the ensure_constructible functions ensures
 * that the produced strings are valid Scala code.
 *
 * When making manual changes, be sure to change the string literal and the
 * ensure_constrictible functions in the same way.
 */


/**
 * Running the auto-generator
 * --------------------------
 *
 * For large changes, it may be more convenient to re-run the auto-generator
 * instead of manually changing the code.
 *
 * 1. Look below the scissors line (containing "- >8 -") to find the "command:" line.
 *    Take note of which JSON files are mentioned.
 * 2. For each JSON file, find its corresponding Scala file (e.g. statements.json comes
 *    from Statement.scala).
 * 3. Go to https://astexplorer.net/#/gist/eb0f2062180067b412017010df04eace/latest and
 *    make sure that the scalameta setting is set to "Scala 3".
 * 4. Paste the contents of the Scala file into the left side.
 * 5. Copy the JSON from the right-hand side into a new JSON file.
 * 6. Repeat this for the rest of the required JSON files.
 *
 * 7. Run the Python command listed below the scissors line.
 * 8. Make sure that the code compiles and passes tests:
 *
 *     ./mill test.testOnly 'ir.ToScalaTest'
 *
 * 9. Upload the changes.
 *
 */


/**
 * Manually-defined ToScala instances
 * ----------------------------------
 *
 * Externals: DirectCall, IndirectCall, GoTo, Return
 *
 * The "Externals" line above tells the generator the following types as having
 * manually-defined ToScala instances. These types interact with control-flow,
 * and the naive auto-generated code would be large and possibly cyclic.
 */


// NOTE: It is important that these handwritten given instances live in the same
// file as the generated code. These instances must be locatable by summon[],
// otherwise the generated code will self-recurse, leading to non-termination.

given ToScala[Return] = ToScala.Make(_ => "ret")
given ToScala[DirectCall] = ToScala.Make(x => s"directCall(${x.target.procName.toScala})")
given ToScala[IndirectCall] = ToScala.Make(x => s"indirectCall(${x.target.toScala})")
given ToScala[GoTo] = ToScala.Make(x => s"goto(${x.targets.map(x => x.label.toScala).mkString(", ")})")


/**
 * Automatically-derived instances
 * -------------------------------
 *
 * For some types (namely the sealed traits, enums, and case classes), the ToScala
 * instances can be automatically derived. See ToScalaDeriving.
 *
 */

given ToScala[Expr] = ToScala.derived
given ToScala[UnOp] = ToScala.derived
given ToScala[BinOp] = ToScala.derived
given ToScala[Endian] = ToScala.derived
given ToScala[Global] = ToScala.derived
given ToScala[IRType] = ToScala.derived


// NOTE: Unfortunately, for the Command trait, this is not possible because the classes are not case classes.

// given ToScala[Command] = ToScala.deriveWithExclusions[Command, Return | DirectCall | IndirectCall | GoTo] {
//   case x: Return => "ret"
//   case x: DirectCall => s"directCall(${x.target.procName.toScala})"
//   case x: IndirectCall => s"indirectCall(${x.target.toScala})"
//   case x: GoTo => s"goto(${x.targets.map(x => x.label.toScala).mkString(", ")})"
// }


// WARNING: Everything below the next line will be overwritten by the generator!
// ------------------------ >8 ------------------------

// format: off

// command:
// scripts/make_repr_functions.py src/main/scala/ir/dsl/ToScalaGenerated.scala ./statements.json

// generated from ./statements.json
given ToScala[Command] with
  extension (x: Command) def toScala: String = x match {
    case x: Statement => x match {
      case x: Assign => x match {
        case x: SingleAssign => x match {
          case x: LocalAssign => {
            def ensure_constructible(): LocalAssign = LocalAssign(x.lhs, x.rhs, x.label)
            s"LocalAssign(${x.lhs.toScala}, ${x.rhs.toScala}, ${x.label.toScala})"
          }
          case x: MemoryLoad => {
            def ensure_constructible(): MemoryLoad = MemoryLoad(x.lhs, x.mem, x.index, x.endian, x.size, x.label)
            s"MemoryLoad(${x.lhs.toScala}, ${x.mem.toScala}, ${x.index.toScala}, ${x.endian.toScala}, ${x.size.toScala}, ${x.label.toScala})"
          }
        }
        case x: DirectCall => {
          def ensure_constructible(): DirectCall = DirectCall(x.target, x.label, x.outParams, x.actualParams)
          if (Thread.interrupted()) { Thread.currentThread().interrupt(); "<interrupted>" } else summon[ToScala[DirectCall]].toScala(x)
        }
      }
      case x: MemoryStore => {
        def ensure_constructible(): MemoryStore = MemoryStore(x.mem, x.index, x.value, x.endian, x.size, x.label)
        s"MemoryStore(${x.mem.toScala}, ${x.index.toScala}, ${x.value.toScala}, ${x.endian.toScala}, ${x.size.toScala}, ${x.label.toScala})"
      }
      case x: NOP => {
        def ensure_constructible(): NOP = NOP(x.label)
        s"NOP(${x.label.toScala})"
      }
      case x: Assert => {
        def ensure_constructible(): Assert = Assert(x.body, x.comment, x.label)
        s"Assert(${x.body.toScala}, ${x.comment.toScala}, ${x.label.toScala})"
      }
      case x: Assume => {
        def ensure_constructible(): Assume = Assume(x.body, x.comment, x.label, x.checkSecurity)
        s"Assume(${x.body.toScala}, ${x.comment.toScala}, ${x.label.toScala}, ${x.checkSecurity.toScala})"
      }
      case x: Call => x match {
        case x: DirectCall => {
          def ensure_constructible(): DirectCall = DirectCall(x.target, x.label, x.outParams, x.actualParams)
          if (Thread.interrupted()) { Thread.currentThread().interrupt(); "<interrupted>" } else summon[ToScala[DirectCall]].toScala(x)
        }
        case x: IndirectCall => {
          def ensure_constructible(): IndirectCall = IndirectCall(x.target, x.label)
          if (Thread.interrupted()) { Thread.currentThread().interrupt(); "<interrupted>" } else summon[ToScala[IndirectCall]].toScala(x)
        }
      }
    }
    case x: Jump => x match {
      case x: Unreachable => {
        def ensure_constructible(): Unreachable = Unreachable(x.label)
        s"Unreachable(${x.label.toScala})"
      }
      case x: Return => {
        def ensure_constructible(): Return = Return(x.label, x.outParams)
        if (Thread.interrupted()) { Thread.currentThread().interrupt(); "<interrupted>" } else summon[ToScala[Return]].toScala(x)
      }
      case x: GoTo => {
        // unable to validate constructor with private field:
        // def ensure_constructible(): GoTo = GoTo(x._targets, x.label)
        if (Thread.interrupted()) { Thread.currentThread().interrupt(); "<interrupted>" } else summon[ToScala[GoTo]].toScala(x)
      }
    }
  }

// end generated from ./statements.json

// format: on
