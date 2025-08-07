package ir.parsing

type HasParsePosition = {
  val line_num: Int
  val col_num: Int
  val offset: Int
}

final case class ParseException(
  private val message: String,
  val token: HasParsePosition,
  private val cause: Throwable = null
) extends Exception(message, cause) {

  import scala.reflect.Selectable.reflectiveSelectable

  override def toString =
    val line = token.line_num
    val col = token.col_num
    val name = token.getClass.getName
    val excname = this.getClass.getName

    val location = (line, col) match {
      case (-1, -1) => "<unknown>"
      case _ => s"line $line, col $col"
    }

    s"$excname: $message (while parsing $name at $location)"
}
