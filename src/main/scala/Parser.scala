package indentp

sealed abstract class Token(val text: String)
case class NumToken(override val text: String) extends Token(text)
case class NameToken(override val text: String) extends Token(text)
case object LeftParenToken extends Token("(")
case object RightParenToken extends Token(")")
case object CommaToken extends Token(",")
case object ColonToken extends Token(":")
case object AssignOpToken extends Token("=")
case object EqualOpToken extends Token("==")
case object AddOpToken extends Token("+")
case object MulOpToken extends Token("*")
case object NegOpToken extends Token("-")
case object NotOpToken extends Token("!")
case object IfToken extends Token("if")
case object WhileToken extends Token("while")
case object NewlineToken extends Token("NEWLINE")
case object IndentToken extends Token("INDENT")
case object UnindentToken extends Token("UNINDENT")

class Parser {
  def parse(source: String): Either[String, Ast] = {
    return Left("not implemented")
  }
}
