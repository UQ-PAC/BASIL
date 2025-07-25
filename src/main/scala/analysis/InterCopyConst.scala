package analysis

import analysis.solvers.ForwardIDESolver
import ir.*

trait CopyConstAnalysisFunctions(parameterForm: Boolean) extends ForwardIDEAnalysis[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice] {

  val valuelattice: ConstantPropagationLattice = ConstantPropagationLattice()
  val edgelattice: EdgeFunctionLattice[FlatElement[BitVecLiteral], ConstantPropagationLattice] = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]] = {


    if !parameterForm then Map(d->IdEdge())
    else
      d match {
        case Left(a) => // already existing variables

          call.actualParams.toList.foldLeft(Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]]()) {
           case (m, (inVar, expression)) => expression match
             case LocalVar(_, _, _) | Register(_, _) if expression == a => m ++ Map(Left(inVar) -> IdEdge(), d -> IdEdge())
             case LocalVar(_, _, _) | Register(_, _) if expression != a && inVar != a => m ++ Map(d -> IdEdge())
             case _ => m ++ Map()

          }
        case Right(a) =>
          val lambdaToLambda: Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]] = Map(d -> IdEdge())
            call.actualParams.toList.foldLeft(Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]]()) {
            case (m, (inVar, expression)) => expression match {
              case LocalVar(_,_,_) | Register(_,_) => m ++ lambdaToLambda //not add anything else from lambda
              case BitVecLiteral(value, size) => m ++ lambdaToLambda ++ Map(Left(inVar)->ConstEdge(valuelattice.bv(BitVecLiteral(value, size)))) //assign val to in param
              case _ => m ++ lambdaToLambda ++ Map(Left(inVar) -> ConstEdge(valuelattice.top))

            }

          }
      }




  }

  def edgesExitToAfterCall(exit: Return, aftercall: Command)(d: DL): Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]] = {


    if !parameterForm then Map(d->IdEdge())
    else
      val call: DirectCall = aftercall match {
        case aftercall: Statement => aftercall.parent.statements.getPrev(aftercall).asInstanceOf[DirectCall]
        case _: Jump => aftercall.parent.statements.last.asInstanceOf[DirectCall]
      }

      d match {
        case Left(a) =>
          exit.outParams.toList.foldLeft(Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]]()) {
            case (m, (retVar, expression)) => expression match
              case LocalVar(_, _, _) | Register(_, _) if expression == a => m ++ Map(Left(call.outParams(retVar)) -> IdEdge())
              case _ => m ++ Map() //ignore other kind of expr, including local vars / in params of the procedure

          }
        case Right(a) =>
          val lambdaToLambda: Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]] = Map(d -> IdEdge())

          exit.outParams.toList.foldLeft(Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]]()) {
            case (m, (retVar, expression)) => expression match {
              case LocalVar(_, _, _) | Register(_, _) => m ++ lambdaToLambda //not add anything else from lambda
              case BitVecLiteral(value, size) => m ++ lambdaToLambda ++ Map(Left(call.outParams(retVar)) -> ConstEdge(valuelattice.bv(BitVecLiteral(value, size))))
              case _ => m ++ lambdaToLambda ++ Map(Left(call.outParams(retVar)) -> ConstEdge(valuelattice.top))


            }

          }


      }
  }

  def edgesCallToAfterCall(call: DirectCall, aftercall: Command)(d: DL): Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]] = {

    if !parameterForm then Map()
    else
      d match {
        case Left(v) if call.outParams.exists(_._1 == v) || call.outParams.exists(_._2 == v)  || call.actualParams.exists(_._2 == v) => Map()
        case _ => Map(d->IdEdge()) // unused locals in function ignore proc call

      }
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]] = {


    n match {
      case LocalAssign(variable, expression, _) =>
        d match {
          case Right(_) =>
            val lambdaToLambda : Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]] = Map(d -> IdEdge())
            expression match {
              case LocalVar(_,_,_) | Register(_,_) => lambdaToLambda //not add anything else from lambda
              case BitVecLiteral(value, size) => lambdaToLambda ++ Map(Left(variable)->ConstEdge(valuelattice.bv(BitVecLiteral(value, size))))
              case BinaryExpr(op, arg1, arg2) => (arg1,  arg2) match {
                case (BitVecLiteral(value1,size1), BitVecLiteral(value2, size2)) => op match {
                  //started to add evaluations of simple expressions before assignment e.g. x = 1+2 will be evaluated as x->3 instead of x->Top
                    case BVADD => lambdaToLambda ++ Map(Left(variable)->ConstEdge(valuelattice.bvadd(valuelattice.bv(BitVecLiteral(value1, size1)), valuelattice.bv(BitVecLiteral(value2, size2)))))
                    case BVSUB => lambdaToLambda ++ Map(Left(variable)->ConstEdge(valuelattice.bvsub(valuelattice.bv(BitVecLiteral(value1, size1)), valuelattice.bv(BitVecLiteral(value2, size2)))))
                    case BVMUL => lambdaToLambda ++ Map(Left(variable)->ConstEdge(valuelattice.bvmul(valuelattice.bv(BitVecLiteral(value1, size1)), valuelattice.bv(BitVecLiteral(value2, size2)))))
                    case _ => lambdaToLambda ++ Map(Left(variable) -> ConstEdge(valuelattice.top))

                  }
                  case _ => lambdaToLambda ++ Map(Left(variable) -> ConstEdge(valuelattice.top))
                }
              case _ => lambdaToLambda ++ Map(Left(variable) -> ConstEdge(valuelattice.top))
              }


          case Left(a) =>
            expression match {
              case LocalVar(_, _, _) | Register(_,_) if expression == a => Map(Left(variable)->IdEdge(), d ->IdEdge())
              case _ => Map() //ignore other kind of expr

            }

          }

      case MemoryLoad(variable, _, _, _, _, _)=>
        d match {
          case Left(_) => Map(d -> IdEdge())
          case Right(_) => Map(Left(variable) -> ConstEdge(valuelattice.top), d -> IdEdge())
        }

      case exit: Return =>
        // needed to map abstract states of formal in and out parameters and whether they constant in all calls

        Map(d->IdEdge())
        d match {
          case Left(a) =>
            exit.outParams.toList.foldLeft(Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]]()) {
              case (m, (retVar, expression)) => expression match
                case LocalVar(_, _, _) | Register(_, _) if expression == a => m ++ Map(Left(retVar) -> IdEdge())
                case _ => m ++ Map()

            }
          case Right(a) =>
            val lambdaToLambda: Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]] = Map(d -> IdEdge())

            exit.outParams.toList.foldLeft(Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]]()) {
              case (m, (retVar, expression)) => expression match {
                case LocalVar(_, _, _) | Register(_, _) => m ++ lambdaToLambda //not add anything else from lambda
                case BitVecLiteral(value, size) => m ++ lambdaToLambda ++ Map(Left(retVar) -> ConstEdge(valuelattice.bv(BitVecLiteral(value, size))))
                case _ => m ++ lambdaToLambda ++ Map(Left(retVar) -> ConstEdge(valuelattice.top))

              }

            }
        }

      case _ => Map(d->IdEdge())
    }
    }
  }


/**
 * Performs copy-constant propagation analysis on a program. Determines the variables with a constant value, thus
 * providing information for relevant transforms to replace function calls and assignments to variables as assignments
 * to constants. Note that only information for copy assignments is determined, to allow for distributivity and use of
 * IDE solver. 'parameterForm' may be unnecessary as this analysis and corresponding transform may only occur after
 * parameter simplifications have been done.
 */
class InterCopyConst(program:Program, parameterForm: Boolean) extends ForwardIDESolver[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice](program), CopyConstAnalysisFunctions(parameterForm)
