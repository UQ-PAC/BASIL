package analysis

import scala.collection.mutable

object BiMap {
  private[BiMap] trait MethodDistinctor
  implicit object MethodDistinctor extends MethodDistinctor
}

class BiMap[X, Y] {
  val forwardMap: mutable.Map[X, Y] = mutable.Map[X, Y]()
  val backwardMap: mutable.Map[Y, X] = mutable.Map[Y, X]()
  val domain = forwardMap.keys
  val codomain = backwardMap.keys

  def addOne(elem: (X, Y)): Unit = {
    forwardMap.addOne(elem)
    backwardMap.addOne((elem._2, elem._1))
  }

  def apply(x: X): Y = forwardMap(x)
  def apply(y: Y)(implicit d: BiMap.MethodDistinctor): X = backwardMap(y)
  
  override def toString: String = forwardMap.toString

}
