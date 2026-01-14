package boogie

import boogie.*

// case class BMemEncoding() extends BDeclaration {
//   override def toString: String = {
//     BTypeDecl(CustomBType("magictype")).toString()
//   }
// }

object MemEncoding {
  def requires: Map[String, BExpr] = Map(
    "malloc" -> mallocRequires,
    "free" -> freeRequires
  )

  def ensures: Map[String, BExpr] = Map(
    "malloc" -> mallocEnsures,
    "free" -> freeEnsures
  )

  val mallocRequires: List[BExpr] = List(
    
  )

  val mallocEnsures: List[BExpr] = List(
    
  )

  val freeRequires: List[BExpr] = List(
    
  )

  val freeEnsures: List[BExpr] = List(
    
  )
}
