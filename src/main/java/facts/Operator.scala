package facts
import scala.language.implicitConversions

object Operator extends Enumeration {
  type Operator = Value
  val Addition: Operator = Value("+")
  val Subtraction: Operator = Value("-")
  val Multiplication: Operator = Value("*")
  val Division: Operator = Value("/")
  val BitFlip: Operator = Value("~")
}
