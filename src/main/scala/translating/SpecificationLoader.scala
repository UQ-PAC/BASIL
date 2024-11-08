package translating

import Parsers.SpecificationsParser._
import boogie._
import specification._
import ir._

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

// take symbol table entries as input
// parse all global defs and check sizes
// use these new nameToGlobals going forward when parsing the rest of the spec

case class SpecificationLoader(symbols: Set[SpecGlobal], program: Program) {
  private val idToSymbol = symbols.map(g => (g.name, g)).toMap

  def visitSpecification(ctx: SpecificationContext): Specification = {
    val globals = Option(ctx.globals) match {
      case Some(_) => visitGlobals(ctx.globals)
      case None    => Set()
    }
    val nameToGlobals = globals.map(g => (g.name, g)).toMap

    val lPreds = Option(ctx.lPreds) match {
      case Some(_) => visitLPreds(ctx.lPreds, nameToGlobals)
      case None    => Map()
    }

    val relies = Option(ctx.relies) match {
      case Some(_) => visitRelies(ctx.relies, nameToGlobals)
      case None    => List()
    }

    val guarantees = Option(ctx.guarantees) match {
      case Some(_) => visitGuarantees(ctx.guarantees, nameToGlobals)
      case None    => List()
    }

    val subroutines = ctx.subroutine.asScala.map(s => visitSubroutine(s, nameToGlobals)).toList

    val directFunctions = Option(ctx.directFunctions) match {
      case Some(_) => visitDirectFunctions(ctx.directFunctions)
      case None    => Set()
    }
    Specification(Set(), globals, lPreds, relies, guarantees, subroutines, directFunctions)
  }

  def visitDirectFunctions(ctx: DirectFunctionsContext): Set[FunctionOp] = {
    ctx.directFunction.asScala.map(d => visitDirectFunction(d)).toSet
  }

  def visitDirectFunction(ctx: DirectFunctionContext): FunctionOp = ctx match {
    case m: MemoryLoadContext =>
      val suffix = m.getText.stripPrefix("memory_load")
      val size = suffix.stripSuffix("_le").stripSuffix("_be")
      val endian = suffix.stripPrefix(size)
      MemoryLoadOp(64, 8, parseEndian(endian), Integer.parseInt(size))
    case m: MemoryStoreContext =>
      val suffix = m.getText.stripPrefix("memory_store")
      val size = suffix.stripSuffix("_le").stripSuffix("_be")
      val endian = suffix.stripPrefix(size)
      MemoryStoreOp(64, 8, parseEndian(endian), Integer.parseInt(size))
    case g: GammaLoadContext =>
      val sizeText = g.getText.stripPrefix("gamma_load")
      val size = Integer.parseInt(sizeText)
      GammaLoadOp(64, size, size / 8)
    case g: GammaStoreContext =>
      val sizeText = g.getText.stripPrefix("gamma_store")
      val size = Integer.parseInt(sizeText)
      GammaStoreOp(64, size, size / 8)
    case z: ZeroExtendContext =>
      val suffix = z.getText.stripPrefix("zero_extend")
      val sizes = suffix.split("_")
      val extension = Integer.parseInt(sizes(0))
      val bodySize = Integer.parseInt(sizes(1))
      BVFunctionOp(s"zero_extend${extension}_$bodySize", s"zero_extend $extension", List(BParam(BitVecBType(bodySize))), BParam(BitVecBType(bodySize + extension)))
    case s: SignExtendContext =>
      val suffix = s.getText.stripPrefix("sign_extend")
      val sizes = suffix.split("_")
      val extension = Integer.parseInt(sizes(0))
      val bodySize = Integer.parseInt(sizes(1))
      BVFunctionOp(s"sign_extend${extension}_$bodySize", s"sign_extend $extension", List(BParam(BitVecBType(bodySize))), BParam(BitVecBType(bodySize + extension)))
    case b: BvOpContext =>
      val body = b.getText.stripPrefix("bv")
      val sizeText = body.replaceAll("\\D+","")
      val size = Integer.parseInt(sizeText)
      val op = body.stripSuffix(sizeText)
      val outType = op match {
        case "and" | "or" | "add" | "mul" | "udiv" | "urem" | "shl" | "lshr" | "nand" | "nor" | "xor" | "xnor" | "sub" |
            "srem" | "sdiv" | "smod" | "ashr" =>
          BitVecBType(size)
        case "comp" =>
          BitVecBType(1)
        case "ult" | "ule" | "ugt" | "uge" | "slt" | "sle" | "sgt" | "sge" =>
          BoolBType
        case _ =>
          throw new Exception("parsing error")
      }
      BVFunctionOp(s"bv$op$size", s"bv$op", List(BParam(BitVecBType(size)), BParam(BitVecBType(size))), BParam(outType))
  }

  def parseEndian(endian: String): Endian = endian match {
    case "_le" => Endian.LittleEndian
    case "_be" => Endian.BigEndian
  }

  def visitGlobals(ctx: GlobalsContext): Set[SpecGlobal] = {
    ctx.globalDef.asScala.map(g => visitGlobalDef(g)).toSet
  }

  def visitGlobalDef(ctx: GlobalDefContext): SpecGlobal = {
    val id = ctx.id.getText
    val symbol = idToSymbol.get(id) match {
      case Some(g: SpecGlobal) => g
      case None                => throw new Exception(s"unresolvable reference to '$id' in specification")
    }
    val size = ctx.typeName match {
      case b: BvTypeContext    => Integer.parseInt(b.BVSIZE.getText.stripPrefix("bv"))
      case _: LongTypeContext  => 64
      case _: ShortTypeContext => 16
      case _: IntTypeContext   => 32
      case _: CharTypeContext  => 8
    }
    val arraySize = Option(ctx.arraySize) match {
      case Some(a: ArraySizeContext) => Some(Integer.parseInt(a.size.getText))
      case None                      => None
    }
    val bits = size * arraySize.getOrElse(1)
    if (bits != symbol.size) {
      throw new Exception(s"variable $id in specification does not match symbol table")
    }
    SpecGlobal(id, size, arraySize, symbol.address)
  }

  def visitLPred(ctx: LPredContext, nameToGlobals: Map[String, SpecGlobal]): (SpecGlobal, BExpr) = {
    (nameToGlobals(ctx.id.getText), visitExpr(ctx.expr, nameToGlobals))
  }

  def visitLPreds(ctx: LPredsContext, nameToGlobals: Map[String, SpecGlobal]): Map[SpecGlobal, BExpr] = {
    ctx.lPred.asScala.map(l => visitLPred(l, nameToGlobals)).toMap
  }

  def visitRelies(ctx: ReliesContext, nameToGlobals: Map[String, SpecGlobal]): List[BExpr] = {
    ctx.expr.asScala.map(e => visitExpr(e, nameToGlobals)).toList
  }

  def visitGuarantees(ctx: GuaranteesContext, nameToGlobals: Map[String, SpecGlobal]): List[BExpr] = {
    ctx.expr.asScala.map(e => visitExpr(e, nameToGlobals)).toList
  }

  def visitExpr(
      ctx: ExprContext,
      nameToGlobals: Map[String, SpecGlobal],
      params: Map[String, Parameter] = Map()
  ): BExpr = {
    val exprs = ctx.impliesExpr.asScala.map(e => visitImpliesExpr(e, nameToGlobals, params))
    if (exprs.size > 1) {
      exprs.tail.foldLeft(exprs.head)((opExpr: BExpr, next: BExpr) => BinaryBExpr(BoolEQUIV, opExpr, next))
    } else {
      exprs.head
    }
  }

  def visitImpliesExpr(
      ctx: ImpliesExprContext,
      nameToGlobals: Map[String, SpecGlobal],
      params: Map[String, Parameter] = Map()
  ): BExpr = Option(ctx.arg2) match {
    case Some(_) =>
      BinaryBExpr(
        BoolIMPLIES,
        visitLogicalExpr(ctx.arg1, nameToGlobals, params),
        visitImpliesExpr(ctx.arg2, nameToGlobals, params)
      )
    case None => visitLogicalExpr(ctx.arg1, nameToGlobals, params)
  }

  def visitLogicalExpr(
      ctx: LogicalExprContext,
      nameToGlobals: Map[String, SpecGlobal],
      params: Map[String, Parameter] = Map()
  ): BExpr = {
    val rels = ctx.relExpr.asScala.map(r => visitRelExpr(r, nameToGlobals, params))
    if (rels.size > 1) {
      val op = if (ctx.AND_OP.size > 0) {
        BoolAND
      } else {
        BoolOR
      }
      rels.tail.foldLeft(rels.head)((opExpr: BExpr, next: BExpr) => BinaryBExpr(op, opExpr, next))
    } else {
      rels.head
    }
  }

  def visitRelExpr(
      ctx: RelExprContext,
      nameToGlobals: Map[String, SpecGlobal],
      params: Map[String, Parameter] = Map()
  ): BExpr = Option(ctx.arg2) match {
    case Some(_) =>
      BinaryBExpr(
        visitRelOp(ctx.op),
        visitTerm(ctx.arg1, nameToGlobals, params),
        visitTerm(ctx.arg2, nameToGlobals, params)
      )
    case None => visitTerm(ctx.arg1, nameToGlobals, params)
  }

  def visitTerm(
      ctx: TermContext,
      nameToGlobals: Map[String, SpecGlobal],
      params: Map[String, Parameter] = Map()
  ): BExpr = Option(ctx.arg2) match {
    case Some(_) =>
      BinaryBExpr(
        visitAddSubOp(ctx.op),
        visitFactor(ctx.arg1, nameToGlobals, params),
        visitFactor(ctx.arg2, nameToGlobals, params)
      )
    case None => visitFactor(ctx.arg1, nameToGlobals, params)
  }

  def visitFactor(
      ctx: FactorContext,
      nameToGlobals: Map[String, SpecGlobal],
      params: Map[String, Parameter] = Map()
  ): BExpr = Option(ctx.arg2) match {
    case Some(_) =>
      BinaryBExpr(
        visitMulDivModOp(ctx.op),
        visitUnaryExpr(ctx.arg1, nameToGlobals, params),
        visitUnaryExpr(ctx.arg2, nameToGlobals, params)
      )
    case None => visitUnaryExpr(ctx.arg1, nameToGlobals, params)
  }

  def visitUnaryExpr(
      ctx: UnaryExprContext,
      nameToGlobals: Map[String, SpecGlobal],
      params: Map[String, Parameter] = Map()
  ): BExpr = ctx match {
    case n: NegExprContext       => UnaryBExpr(BVNEG, visitUnaryExpr(n.unaryExpr, nameToGlobals, params))
    case a: AtomUnaryExprContext => visitAtomExpr(a.atomExpr, nameToGlobals, params)
    case n: NotExprContext       => UnaryBExpr(BoolNOT, visitUnaryExpr(n.unaryExpr, nameToGlobals, params))
  }

  def visitAtomExpr(
      ctx: AtomExprContext,
      nameToGlobals: Map[String, SpecGlobal],
      params: Map[String, Parameter] = Map()
  ): BExpr = ctx match {
    case b: BoolLitExprContext     => visitBoolLit(b.boolLit)
    case i: IdExprContext          => visitId(i.id, nameToGlobals, params)
    case o: OldExprContext         => visitOldExpr(o, nameToGlobals, params)
    case p: ParenExprContext       => visitExpr(p.expr, nameToGlobals, params)
    case i: IfThenElseExprContext  => visitIfThenElseExpr(i, nameToGlobals, params)
    case a: ArrayAccessExprContext => visitArrayAccess(a.arrayAccess, nameToGlobals, params)
    case b: BvExprContext          => visitBv(b.bv)
    case d: DirectExprContext      => visitDirectE(d)
  }

  def visitArrayAccess(
      ctx: ArrayAccessContext,
      nameToGlobals: Map[String, SpecGlobal],
      params: Map[String, Parameter] = Map()
  ): ArrayAccess = {
    val global = visitId(ctx.id, nameToGlobals, params) match {
      case g: SpecGlobal => g
      case _ => throw new Exception("invalid array access '" + ctx.getText + "' to non-global in specification")
    }
    ArrayAccess(global, Integer.parseInt(ctx.nat.getText))
  }

  def visitBoogieTypeName(ctx: BoogieTypeNameContext): BType = {
    ctx match {
      case b: BvBTypeContext => BitVecBType(Integer.parseInt(b.BVSIZE.getText.stripPrefix("bv")))
      case _: IntBTypeContext => IntBType
      case _: BoolBTypeContext => BoolBType
      case m: MapBTypeContext => MapBType(visitBoogieTypeName(m.keyT), visitBoogieTypeName(m.valT))
    }
  }

  def visitDirectE(ctx: DirectExprContext): BDirectExpr = {
    BDirectExpr(
      ctx.literalval.getText.stripPrefix("\"").stripSuffix("\""), visitBoogieTypeName(ctx.btype))
  }

  def visitBv(ctx: BvContext): BitVecBLiteral = {
    BitVecBLiteral(BigInt(ctx.value.getText), Integer.parseInt(ctx.BVSIZE.getText.stripPrefix("bv")))
  }

  def visitOldExpr(
      ctx: OldExprContext,
      nameToGlobals: Map[String, SpecGlobal],
      params: Map[String, Parameter] = Map()
  ): Old = Old(visitExpr(ctx.expr, nameToGlobals, params))

  def visitIfThenElseExpr(
      ctx: IfThenElseExprContext,
      nameToGlobals: Map[String, SpecGlobal],
      params: Map[String, Parameter] = Map()
  ): IfThenElse = {
    IfThenElse(
      visitExpr(ctx.guard, nameToGlobals, params),
      visitExpr(ctx.thenExpr, nameToGlobals, params),
      visitExpr(ctx.elseExpr, nameToGlobals, params)
    )
  }

  def visitNat(ctx: NatContext): IntBLiteral = IntBLiteral(BigInt(ctx.getText))

  def visitBoolLit(ctx: BoolLitContext): BoolBLiteral = ctx.getText match {
    case "true"  => TrueBLiteral
    case "false" => FalseBLiteral
  }

  def visitId(ctx: IdContext, nameToGlobals: Map[String, SpecGlobal], params: Map[String, Parameter] = Map()): BExpr = {
    ctx.getText match {
      case id if id.startsWith("Gamma_R") =>
        BVariable(id, BoolBType, Scope.Global)
      case id if id.startsWith("Gamma_") =>
        val gamma_id = id.stripPrefix("Gamma_")
        params.get(gamma_id) match {
          case Some(p: Parameter) => p.value.toGamma
          case None =>
            nameToGlobals.get(gamma_id) match {
              case Some(g: SpecGlobal) => SpecGamma(g)
              case None                => throw new Exception(s"unresolvable reference to '$id' in specification")
            }
          }
      case id if id.startsWith("R") =>
        BVariable(id, BitVecBType(64), Scope.Global)
      case id =>
        params.get(id) match {
          case Some(p: Parameter) =>
            val registerSize = p.value.size
            val paramSize = p.size
            if (paramSize == registerSize) {
              p.value.toBoogie
            } else if (registerSize > paramSize) {
              BVExtract(registerSize - p.size, 0, p.value.toBoogie)
            } else {
              throw Exception(s"parameter $p doesn't fit in register ${p.value} for ID $id")
            }
          case None =>
            nameToGlobals.get(ctx.getText) match {
              case Some(g: SpecGlobal) => g
              case None                => throw new Exception(s"unresolvable reference to '$id' in specification")
            }
        }
    }
  }

  def visitMulDivModOp(ctx: MulDivModOpContext): BVBinOp = ctx.getText match {
    case "*"   => BVMUL
    case "div" => BVSDIV
    case "mod" => BVSMOD
  }

  def visitAddSubOp(ctx: AddSubOpContext): BVBinOp = ctx.getText match {
    case "+" => BVADD
    case "-" => BVSUB
  }

  // may need to make this more sophisticated and check for bool == bool
  def visitRelOp(ctx: RelOpContext): BVBinOp = ctx.getText match {
    case "==" => BVEQ
    case "!=" => BVNEQ
    case ">"  => BVSGT
    case ">=" => BVSGE
    case "<"  => BVSLT
    case "<=" => BVSLE
  }

  def visitSubroutine(ctx: SubroutineContext, nameToGlobals: Map[String, SpecGlobal]): SubroutineSpec = {
    val name = ctx.id.getText
    val irProc = program.procedures.collectFirst { case p if p.name == name => p }

    val params: Map[String, Parameter] = irProc match {
      case None => Map()
      case Some(p) =>
        p.in.map(p => p.name -> p).toMap ++ p.out.map(p => p.name -> p).toMap
    }

    val requires = ctx.requires.asScala.collect { case r: ParsedRequiresContext =>
      visitExpr(r.expr, nameToGlobals, params)
    }.toList

    val modifies = Option(ctx.modifies) match {
      case Some(_) => visitModifies(ctx.modifies)
      case None => List()
    }

    val ensures = ctx.ensures.asScala.collect {
      case e: ParsedEnsuresContext => visitExpr(e.expr, nameToGlobals, params)
    }.toList

    val requiresDirect = ctx.requires.asScala.collect { case r: DirectRequiresContext =>
      r.QUOTESTRING.getText.stripPrefix("\"").stripSuffix("\"")
    }.toList

    val ensuresDirect = ctx.ensures.asScala.collect { case r: DirectEnsuresContext =>
      r.QUOTESTRING.getText.stripPrefix("\"").stripSuffix("\"")
    }.toList

    val rely = Option(ctx.relies) match {
      case Some(_) => visitRelies(ctx.relies, nameToGlobals)
      case None => List()
    }

    val guarantee = Option(ctx.guarantees) match {
      case Some(_) => visitGuarantees(ctx.guarantees, nameToGlobals)
      case None => List()
    }

    SubroutineSpec(ctx.id.getText, requires, requiresDirect, ensures, ensuresDirect, modifies, rely, guarantee)
  }

  def visitModifies(ctx: ModifiesContext): List[String] = {
    ctx.id.asScala.map(_.getText).toList
  }

}
