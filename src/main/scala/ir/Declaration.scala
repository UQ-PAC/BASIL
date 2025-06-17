package ir
import boogie.*


sealed trait Decl {
  def toBoogie: BDeclaration
}

case class AxiomDecl(e: Expr) extends Decl {
  def toBoogie = BAxiom(e.toBoogie, List())
}

case class FunctionDecl(
  name: String,
  params: List[LocalVar],
  returnType: IRType,
  definition: Option[Expr],
  attribs: List[(String, Option[String])] = List(),
  inlineDef: Boolean = false
) extends Decl {
  def toBoogie =

    val bparams = params.map(p => BParam(p.name, p.getType.toBoogie))

    val body = definition.map(_.toBoogie)

    BFunction(name, bparams, BParam(returnType.toBoogie), body, attribs.map((n, v) => BAttribute(n, v)))
  def makeCall(actualParams: List[Expr] = List()) = {
    require(params.map(_.getType) == actualParams.map(_.getType))
    if (!inlineDef || definition.isEmpty) then {
      UninterpretedFunction(name, actualParams, returnType, false)
    } else {
      val l = LambdaExpr(params, definition.get)
      ir.eval.evalLambdaApply(l, UninterpretedFunction(name, params, returnType, false))
    }
  }
}

