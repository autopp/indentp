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

class Parser {
  type MayError[A] = Either[String, A]

  def parse(source: String): MayError[Ast] = {
    new Lexer().tokenize(source) match {
      case Right(tokens) => parseProgram(tokens, Nil)
      case Left(msg) => Left(msg)
    }
  }

  def parseProgram(tokens: List[Token], buf: List[Stmt]): MayError[Program] = {
    tokens match {
      case Nil => {
        Right(Program(buf.reverse))
      }
      case _ => {
        parseStmt(tokens) match {
          case Right((stmt, rest)) => parseProgram(rest, stmt::buf)
          case Left(msg) => Left(msg)
        }
      }
    }
  }

  def parseStmt(tokens: List[Token]): MayError[(Stmt, List[Token])] = {
    Left("parseStmt is not implemented")
  }

  class Lexer {
    val rules = List[(Regex, String => Option[Token])](
      ("[ ]".r, (s) => None),
      ("[0-9]+".r, (s) => Some(NumToken(s))),
      ("[a-zA-Z_][a-zA-Z_0-9]*".r, (s) => Some(NameToken(s))),
      ("[(]".r, (s) => Some(LeftParenToken)),
      ("[)]".r, (s) => Some(RightParenToken)),
      (",".r, (s) => Some(CommaToken)),
      (":".r, (s) => Some(ColonToken)),
      ("==".r, (s) => Some(EqualOpToken)),
      ("=".r, (s) => Some(AssignOpToken)),
      ("[+]".r, (s) => Some(AddOpToken)),
      ("[*]".r, (s) => Some(MulOpToken)),
      ("[-]".r, (s) => Some(NegOpToken)),
      ("[!]".r, (s) => Some(NotOpToken)),
      ("if".r, (s) => Some(IfToken)),
      ("while".r, (s) => Some(WhileToken))
    )

    def tokenize(source: String): MayError[List[Token]] = {
      // Ignore empty lines
      val emptyPattern = "\\s*$".r
      val lines = source.lines.filter(emptyPattern.findPrefixOf(_) == None).toList
      val indentStack = List(0)
      tokenizeLines(lines, indentStack, List())
    }

    def tokenizeLines(lines: List[String], indentStack: List[Int], buf: List[Token]): MayError[List[Token]] = {
      lines match {
        case Nil => {
          Right((List.fill(indentStack.size - 1)(DedentToken) ++ buf).reverse)
        }
        case line::rest => {
          tokenizeLine(line, indentStack, buf) match {
            case Right((indentStack, buf)) => tokenizeLines(rest, indentStack, NewlineToken::buf)
            case Left(msg) => Left(msg)
          }
        }
      }
    }

    def tokenizeLine(line: String, indentStack: List[Int], buf: List[Token]): MayError[(List[Int], List[Token])] = {
      val pattern = "^(\\s*)(\\S.*)$".r
      line match {
        case pattern(indent, body) => {
          tokenizeIndent(indent.size, indentStack, buf) match {
            case Right((indentStack, buf)) => {
              tokenizeLineBody(body, buf) match {
                case Right(buf) => Right((indentStack, buf))
                case Left(e) => Left(e)
              }
            }
            case e => e
          }
        }
      }
    }

    def tokenizeIndent(indentSize: Int, indentStack: List[Int], buf: List[Token], topLevel: Boolean = true): MayError[(List[Int], List[Token])] = {
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

    def tokenizeLineBody(body: String, buf: List[Token]): MayError[(List[Token])] = {
      if (body.isEmpty) {
        Right(buf)
      } else {
        applyRule(body, rules, buf) match {
          case Right((rest, buf)) => tokenizeLineBody(rest, buf)
          case Left(msg) => Left(msg)
        }
      }
    }

    def applyRule(body: String, rules: List[(Regex, String => Option[Token])], buf: List[Token]): MayError[(String, List[Token])] = {
      rules match {
        case Nil => Left(s"cannot recognize `${body.head}`")
        case (pattern, converter)::restRules => {
          pattern.findPrefixOf(body) match {
            case None => applyRule(body, restRules, buf)
            case Some(matched) => {
              val restBody = body.drop(matched.length)
              converter(matched) match {
                case None => Right((restBody, buf))
                case Some(token) => Right((restBody, token::buf))
              }
            }
          }
        }
      }
    }
  }
}
