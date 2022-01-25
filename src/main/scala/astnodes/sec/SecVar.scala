package astnodes.sec
import astnodes.exp
import astnodes.pred.Pred

/**
 * if gamma = true then this is a security level variable
 * else it is a security level literal
 */
case class SecVar (name: String, gamma: Boolean = false) extends Sec {
  override def toString: String = s"${if (gamma) "Gamma" else "s"}_$name"
  override def vars = List()
}
