package analysis

import analysis.solvers.ForwardIDESolver
import ir.*

trait CopyConstAnalysisFunctions(parameterForm: Boolean) extends ForwardIDEAnalysis[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice] {

  val valuelattice: ConstantPropagationLattice = ConstantPropagationLattice()
  val edgelattice: EdgeFunctionLattice[FlatElement[BitVecLiteral], ConstantPropagationLattice] = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]] = {

  //print("actual params: " + call.actualParams)


    // below only for param form no?, otherwise just give everything
    if !parameterForm then Map(d->IdEdge())
    else
      d match {
        case Left(a) =>
          call.actualParams.toList.foldLeft(Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]]()) {
           case (m, (inVar, expression)) => expression match
             case LocalVar(_, _, _) | Register(_, _) if expression == a => m ++ Map(Left(inVar) -> IdEdge(), d -> IdEdge()) // idk if this actually checks properly
             case LocalVar(_, _, _) | Register(_, _) if expression != a && inVar != a => m ++ Map(d -> IdEdge())
             case _ => m ++ Map()

          }
        case Right(a) =>
          val lambdaToLambda: Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]] = Map(d -> IdEdge())
          call.actualParams.toList.foldLeft(Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]]()) {
            case (m, (inVar, expression)) => expression match {
              case LocalVar(_,_,_) | Register(_,_) => m ++ lambdaToLambda //not add anything else from lambda
              case BitVecLiteral(value, size) => m ++ lambdaToLambda ++ Map(Left(inVar)->ConstEdge(valuelattice.bv(BitVecLiteral(value, size))))
              case _ => m ++ lambdaToLambda ++ Map(Left(inVar) -> ConstEdge(valuelattice.top))
              // direct call?

            }

          }
      }




  }

  def edgesExitToAfterCall(exit: Return, aftercall: Command)(d: DL): Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]] = {

    //print(d)

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
              //case LocalVar(_, _, _) | Register(_, _) if expression != a => m ++ Map(d -> IdEdge()) // lol need to fix up here in params shld always just be Map() so add case
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
    else  // unused locals in function and lambda need identity
      d match {
        case Left(v) if (call.outParams.exists(_._2 == v)  || call.actualParams.exists(_._2 == v)) => Map()
        case _ => Map(d->IdEdge())

      }
      // currently every global going into each procedure regardless if going to be modified or not - way to check??
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]] = {


    n match {
      case LocalAssign(variable, expression, _) =>
        // shld make this function icl --> figure out if can just put all under one
        d match {
          case Right(_) =>
            val lambdaToLambda : Map[DL, EdgeFunction[FlatElement[BitVecLiteral]]] = Map(d -> IdEdge())
            expression match {
              case LocalVar(_,_,_) | Register(_,_) => lambdaToLambda //not add anything else from lambda
              case BitVecLiteral(value, size) => lambdaToLambda ++ Map(Left(variable)->ConstEdge(valuelattice.bv(BitVecLiteral(value, size))))
              case _ => lambdaToLambda ++ Map(Left(variable) -> ConstEdge(valuelattice.top))
            }

          case Left(a) =>
            expression match {
              case LocalVar(_, _, _) | Register(_,_) if expression == a => Map(Left(variable)->IdEdge(), d ->IdEdge()) // idk if this actually checks properly
              //case LocalVar(_, _, _) | Register(_,_) if expression != a => Map(d->IdEdge())
              case BitVecLiteral(_, _) => Map() //remove old value
              case _ => Map(d->IdEdge())
              //case _ => Map() //ignore other kind of expr, should this be top or nothing <-- or d is identity, var is top??

            }

          }
      case MemoryLoad(variable, _, _, _, _, _)=> //might have ti fix this up
        d match {
          case Left(_) => Map(d -> IdEdge())
          case Right(_) => Map(Left(variable) -> ConstEdge(valuelattice.top), d -> IdEdge())
        }

      case _ => Map(d->IdEdge())
    }
  }
}

class InterCopyConst(program:Program, parameterForm: Boolean) extends ForwardIDESolver[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice](program), CopyConstAnalysisFunctions(parameterForm)
