package translating
import ir.*
import scala.collection.mutable


case class BST[+T](val v: String) {
  def ++(s: String) = BST(v ++ s)
  override def toString = v
}

class BasilIRPrettyPrinter() extends BasilIR[BST] {
  val blockIndent = "  " 
  val statementIndent = "    " 
  val seenVars = mutable.HashSet[Variable]()

  def apply(x: Block) : String = {
    vblock(x).v
  }
  def apply(x: Procedure) : String = {
    vproc(x).v
  }
  def apply(x: Statement) : String = {
    vstmt(x).v
  }
  def apply(x: Jump) : String = {
    vjump(x).v
  }
  def apply(x: Expr) : String = {
    vexpr(x).v
  }
  def apply(x: Program) : String = {
    vprog(x).v
  }

  def vprog(mainProc: String, procedures: List[BST[Procedure]]) : BST[Program] = {
    BST(s"(main_procedure ${mainProc})\n" + procedures.mkString("\n\n"))
  }

  override def vblock(label: String, statements: List[BST[Statement]], terminator: BST[Jump]): BST[Block] = {
    BST(s"${blockIndent}block ${label} {\n"
    ++ statements.map(statementIndent + _ + ";").mkString("\n") 
    ++ "\n" ++ statementIndent + terminator + ";\n"
    ++ blockIndent + "}")
  }

  override def vproc(p: Procedure) : BST[Procedure] = {
    seenVars.clear()
    super.vproc(p)
  }

  override def vproc(
      name: String,
      inParams: List[BST[Variable]],
      outParams: List[BST[Variable]],
      entryBlock: Option[BST[Block]],
      middleBlocks: List[BST[Block]],
      returnBlock: Option[BST[Block]]
    ): BST[Procedure] = {

    val entry = entryBlock.map(b => {
      b.toString + "\n"
    }).getOrElse("")
    val ret = returnBlock.map(b => {
      "\n" + b.toString
    }).getOrElse("")

    BST(s"proc $name(${inParams.mkString(", ")}) -> (${outParams.mkString(", ")}) {\n$entry${middleBlocks.mkString("\n")}$ret\n}")
  }

  override def vassign(lhs: BST[Variable], rhs: BST[Expr]): BST[Assign] = BST(s"${lhs} := ${rhs}")

  override def vstore(mem: String, index: BST[Expr], value: BST[Expr], endian: Endian, size: Int): BST[MemoryAssign] = {
    val le = if endian == Endian.LittleEndian then "le" else "be"
    BST(s"store_$le(${mem}, ${index}, ${value}, ${size})")
  }

  override def vcall(
      outParams: List[(BST[Variable], BST[Expr])],
      procname: String,
      inparams: List[(BST[Variable], BST[Expr])]
  ): BST[DirectCall] = {
    BST(s"(${outParams.map((l,r) => l).mkString(", ")} := call $procname ${inparams.map((l,r) => r).mkString(", ")})")
  }


  override def vindirect(target: BST[Variable]): BST[IndirectCall] = BST(s"indirect_call(${target})")
  override def vassert(body: BST[Expr]): BST[Assert] = BST(s"assert $body")
  override def vassume(body: BST[Expr]): BST[Assume] = BST(s"assume $body")
  override def vnop(): BST[NOP] = BST("nop")

  override def vgoto(t: List[String]): BST[GoTo] = BST(s"goto(${t.mkString(", ")})")
  override def vunreachable(): BST[Unreachable] = BST("unreachable")
  override def vreturn(outs: List[(BST[Variable], BST[Expr])]) = BST(s"return (${outs.map((l, r) => r).mkString(", ")})")

  def vtype(t: IRType): String = t match {
    case BitVecType(sz) => s"bv$sz"
    case IntType => "nat"
    case BoolType => "bool"
  }

  override def vrvar(e: Variable): BST[Variable] = vlvar(e) 
  override def vlvar(e: Variable): BST[Variable] = {
    if (seenVars.contains(e)) {
      BST(e.name)
    } else {
      seenVars.add(e)
      BST(e.name + s": ${vtype(e.getType)}")
    }
  }

  override def vextract(ed: Int, start: Int, a: BST[Expr]): BST[Expr] = BST(s"${a}[$ed:$start]")
  override def vzeroextend(bits: Int, b: BST[Expr]): BST[Expr]  = BST(s"zero_extend($bits, $b)")
  override def vsignextend(bits: Int, b: BST[Expr]): BST[Expr] = BST(s"sign_extend($bits, $b)")
  override def vrepeat(reps: Int, b: BST[Expr]) = BST(s"repeat($reps, $b)")
  override def vbinary_expr(e: BinOp, l: BST[Expr], r: BST[Expr]): BST[Expr] = {
    val opn = e.getClass.getSimpleName.toLowerCase.stripSuffix("$")
    BST(s"$opn($l $r)")
  }
  override def vunary_expr(e: UnOp, arg: BST[Expr]): BST[Expr] = {
    val opn = e.getClass.getSimpleName.toLowerCase.stripSuffix("$")
    BST(s"$opn($arg)")
  }


  override def vboollit(b: Boolean) = BST(b.toString)
  override def vintlit(i: BigInt) = BST("%x".format(i))
  override def vbvlit(i: BitVecLiteral) = BST("%x".format(i.value) + s"bv${i.size}")
  override def vuninterp_function(name: String, args: Seq[BST[Expr]]): BST[Expr] = BST(s"$name(${args.mkString(", ")})")

  override def vload(arg: MemoryLoad): BST[Expr] = {
    val le = if Endian == Endian.LittleEndian then "le" else "be"
    BST(s"load_${le}(${arg.mem.name}, ${vexpr(arg.index)}, ${arg.size})")
  }


}


