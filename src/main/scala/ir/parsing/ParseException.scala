package ir.parsing

type HasParsePosition = {
  val line_num: Int
  val col_num: Int
  val offset: Int
}

final case class ParseException(private val message: String, val token: HasParsePosition) extends Exception(message) {
  import scala.reflect.Selectable.reflectiveSelectable
  override def toString = s"ParseException: $message (at line ${token.line_num}, col ${token.col_num})"
}
