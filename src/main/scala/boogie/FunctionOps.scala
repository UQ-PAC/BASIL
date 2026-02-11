package boogie
import ir.*
import util.BoogieMemoryAccessMode

val externAttr = BAttribute("extern")
val inlineAttr = BAttribute("inline")

type BasilIRFunctionOp = BoolToBV1Op | BVFunctionOp | MemoryLoadOp | MemoryStoreOp | ByteExtract | InBounds | BUninterpreted

def genFunctionOpDefinition(
  f: BasilIRFunctionOp,
  memory: BoogieMemoryAccessMode = BoogieMemoryAccessMode.SuccessiveStoreSelect
): BFunction = {
  f match {
    case b: BoolToBV1Op =>
      val invar = BParam("arg", BoolBType)
      val outvar = BParam(BitVecBType(1))
      val body = IfThenElse(invar, BitVecBLiteral(1, 1), BitVecBLiteral(0, 1))
      BFunction(b.fnName, List(invar), outvar, Some(body), List(externAttr, b.attribute))
    case b: BVFunctionOp => BFunction(b.name, b.in, b.out, None, List(externAttr, b.attribute))
    case m: MemoryLoadOp => genLoadFunction(m)
    case m: MemoryStoreOp =>
      memory match
        case BoogieMemoryAccessMode.SuccessiveStoreSelect => genStoreFunction(m)
        case BoogieMemoryAccessMode.LambdaStoreSelect => genStoreLambdaFunction(m)
    case b: ByteExtract =>
      val valueVar = BParam("value", BitVecBType(b.valueSize))
      val offsetVar = BParam("offset", BitVecBType(b.offsetSize))
      val in = List(valueVar, offsetVar)
      val out = BParam(BitVecBType(8))
      val shift = BinaryBExpr(BVMUL, offsetVar, BitVecBLiteral(8, b.offsetSize))
      val eshift = if (b.valueSize < b.offsetSize) {
        BVExtract(b.valueSize, 0, shift)
      } else if (b.valueSize == b.offsetSize) {
        shift
      } else {
        BVZeroExtend(b.valueSize - b.offsetSize, shift)
      }
      val body = BVExtract(8, 0, BinaryBExpr(BVLSHR, valueVar, eshift))
      BFunction(b.fnName, in, out, Some(body), List(inlineAttr))
    case b: InBounds => genInBoundsFunction(b)
    case u: BUninterpreted =>
      BFunction(u.name, u.in.map(BParam(_)), BParam(u.out), None, List(externAttr))
  }
}

def genLoadFunction(m: MemoryLoadOp) = {
  val memVar = BMapVar("memory", MapBType(BitVecBType(m.addressSize), BitVecBType(m.valueSize)), Scope.Parameter)
  val indexVar = BParam("index", BitVecBType(m.addressSize))
  val in = List(memVar, indexVar)
  val out = BParam(BitVecBType(m.bits))
  val accesses: Seq[MapAccess] = for (i <- 0 until m.accesses) yield {
    if (i == 0) {
      MapAccess(memVar, indexVar)
    } else {
      MapAccess(memVar, BinaryBExpr(BVADD, indexVar, BitVecBLiteral(i, m.addressSize)))
    }
  }
  val accessesEndian = m.endian match {
    case Endian.BigEndian => accesses.reverse
    case Endian.LittleEndian => accesses
  }

  val body: BExpr = accessesEndian.toList match {
    case h :: Nil => h
    case h :: tail =>
      tail.foldLeft(h) { (concat: BExpr, next: MapAccess) =>
        BinaryBExpr(BVCONCAT, next, concat)
      }
    case Nil => throw Exception(s"Zero byte access: $m")
  }

  BFunction(m.fnName, in, out, Some(body), List(externAttr))
}

def genStoreLambdaFunction(m: MemoryStoreOp) = {
  val memType = MapBType(BitVecBType(m.addressSize), BitVecBType(m.valueSize))
  val memVar = BMapVar("memory", memType, Scope.Parameter)
  val indexVar = BParam("index", BitVecBType(m.addressSize))
  val valueVar = BParam("value", BitVecBType(m.bits))
  val in = List(memVar, indexVar, valueVar)
  val out = BParam(memType)
  val body =
    if (m.accesses == 1) {
      MapUpdate(memVar, indexVar, valueVar)
    } else {
      val i = BVariable("i", BitVecBType(m.addressSize), Scope.Local)
      Lambda(
        List(i),
        IfThenElse(
          BInBounds(indexVar, BitVecBLiteral(m.accesses, m.addressSize), m.endian, i),
          BByteExtract(valueVar, BinaryBExpr(BVSUB, i, indexVar)),
          MapAccess(memVar, i)
        )
      )
    }

  BFunction(m.fnName, in, out, Some(body), List(externAttr))
}

def genStoreFunction(m: MemoryStoreOp) = {
  val memType = MapBType(BitVecBType(m.addressSize), BitVecBType(m.valueSize))
  val memVar = BMapVar("memory", memType, Scope.Parameter)
  val indexVar = BParam("index", BitVecBType(m.addressSize))
  val valueVar = BParam("value", BitVecBType(m.bits))
  val in = List(memVar, indexVar, valueVar)
  val out = BParam(memType)
  val indices: Seq[BExpr] = for (i <- 0 until m.accesses) yield {
    if (i == 0) {
      indexVar
    } else {
      BinaryBExpr(BVADD, indexVar, BitVecBLiteral(i, m.addressSize))
    }
  }
  val values: Seq[BExpr] = for (i <- 0 until m.accesses) yield {
    BVExtract((i + 1) * m.valueSize, i * m.valueSize, valueVar)
  }
  val valuesEndian = m.endian match {
    case Endian.BigEndian => values.reverse
    case Endian.LittleEndian => values
  }
  val indiceValues = for (i <- 0 until m.accesses) yield {
    (indices(i), valuesEndian(i))
  }

  val body = indiceValues.tail.foldLeft(MapUpdate(memVar, indices.head, valuesEndian.head)) {
    (update: MapUpdate, next: (BExpr, BExpr)) => MapUpdate(update, next(0), next(1))
  }

  BFunction(m.fnName, in, out, Some(body), List(externAttr))
}

def genInBoundsFunction(b: InBounds) = {
  val baseVar = BParam("base", BitVecBType(b.bits))
  val lenVar = BParam("len", BitVecBType(b.bits))
  val iVar = BParam("i", BitVecBType(b.bits))
  val in = List(baseVar, lenVar, iVar)
  val out = BParam(BoolBType)
  val begin = b.endian match {
    case Endian.LittleEndian => baseVar
    case Endian.BigEndian => BinaryBExpr(BVSUB, baseVar, lenVar)
  }
  val end = b.endian match {
    case Endian.LittleEndian => BinaryBExpr(BVADD, baseVar, lenVar)
    case Endian.BigEndian => baseVar
  }

  val above = BinaryBExpr(BVULE, begin, iVar)
  val below = BinaryBExpr(BVULT, iVar, end)
  val wrap = BinaryBExpr(BVULE, baseVar, end)
  val body = IfThenElse(wrap, BinaryBExpr(BoolAND, above, below), BinaryBExpr(BoolOR, above, below))
  BFunction(b.fnName, in, out, Some(body), List(inlineAttr))

}
