package indentp

import scala.util.matching.Regex

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
case object DedentToken extends Token("DEDENT")
case object EndToken extends Token("EOF")

class Parser {
  def parse(source: String): Either[String, Ast] = {
    tokenize(source)
    Left("not implemented")
  }

  def tokenize(source: String): Either[String, List[Token]] = {
    // Ignore empty lines
    val emptyPattern = "\\s*$".r
    val lines = source.lines.filter(emptyPattern.findPrefixOf(_) == None).toList
    val indentStack = List(0)
    tokenizeLines(lines, indentStack, List())
  }

  def tokenizeLines(lines: List[String], indentStack: List[Int], buf: List[Token]): Either[String, List[Token]] = {
    lines match {
      case Nil => {
        Right((EndToken::List.fill(indentStack.size - 1)(DEDENTToken) ++ buf).reverse)
      }
      case line::rest => {
        tokenizeLine(line, indentStack, buf)
        Left("not implemented")
      }
    }
  }

  def tokenizeLine(line: String, indentStack: List[Int], buf: List[Token]): Either[String, (List[Int], List[Token])] = {
    val pattern = "^(\\s*)(\\S.*)$".r
    line match {
      case pattern(indent, body) => {
        tokenizeIndent(indent.size, indentStack, buf) match {
          case Right((indentStack, buf)) => {
            tokenizeLineBody(body, indentStack, buf) match {
              case Right((indentStack, buf)) => Right((indentStack, buf))
              case e => e
            }
          }
          case e => e
        }
      }
    }
  }

  def tokenizeIndent(indentSize: Int, indentStack: List[Int], buf: List[Token], topLevel: Boolean = true): Either[String, (List[Int], List[Token])] = {
    indentStack match {
      case lastIndent::restStack => {
        indentSize match {
          case n if n > lastIndent => {
            if (topLevel) Right(n::indentStack, IndentToken::buf) else Left("inconsistent dedent")
          }
          case n if n == lastIndent => Right(indentStack, buf)
          case _ => {
            tokenizeIndent(indentSize, restStack, DedentToken::buf, false)
          }
        }
      }
      case _ => {
        Left("BUG: indentStack is empty")
      }
    }
  }

  def tokenizeLineBody(body: String, indentStack: List[Int], buf: List[Token]): Either[String, (List[Int], List[Token])] = {
    Left("not implemented")
  }
}
