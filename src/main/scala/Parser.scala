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
case object PassToken extends Token("pass")
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
    tokens match {
      case PassToken::rest => {
        rest match {
          case NewlineToken::rest => Right(PassStmt, rest)
          case _ => Left(genError("NEWLINE", rest))
        }
      }
      case _ => {
        parseExpr(tokens) match {
          case Right((expr, rest)) => {
            rest match {
              case NewlineToken::rest => Right(ExprStmt(expr), rest)
              case _ => Left(genError("NEWLINE", rest))
            }
          }
          case Left(msg) => Left(msg)
        }
      }
    }
  }

  def parseExpr(tokens: List[Token]): MayError[(Expr, List[Token])] = {
    type ExprParserFunc = (List[Token]) => MayError[(Expr, List[Token])]
    def genBinOpExprParser(opToken: Token, opStr: String, parseOperand: ExprParserFunc): ExprParserFunc = {
      (tokens: List[Token]) => {
        def parseBinOpExprRest(tokens: List[Token], prev: Expr): MayError[(Expr, List[Token])] = {
          tokens match {
            case opToken::rest => {
              parseOperand(rest) match {
                case Right((expr, rest)) => parseBinOpExprRest(rest, BinOpExpr(opStr, prev, expr))
                case err => err
              }
            }
            case _ => Right((prev, tokens))
          }
        }

        parseOperand(tokens) match {
          case Right((expr, rest)) => parseBinOpExprRest(rest, expr)
          case err => err
        }
      }
    }

    val parseMulExpr = genBinOpExprParser(MulOpToken, "*", parseUnOpExpr)
    val parseAddExpr = genBinOpExprParser(AddOpToken, "+", parseMulExpr)
    val parseLogicalExpr = genBinOpExprParser(EqualOpToken, "+", parseAddExpr)

    tokens match {
      case NameToken(name)::AssignOpToken::rest => {
        parseExpr(rest) match {
          case Right((expr, rest)) => Right((AssignExpr(name, expr), rest))
          case err => err
        }
      }
      case _ => parseLogicalExpr(tokens)
    }
  }

  def parseUnOpExpr(tokens: List[Token]): MayError[(Expr, List[Token])] = {
    val opMap = Map[Token, String](NegOpToken -> "-", NotOpToken -> "!")
    tokens match {
      case token::rest if opMap.contains(token) => {
        parseUnOpExpr(rest) match {
          case Right((expr, rest)) => Right((UnOpExpr(opMap(token), expr), rest))
          case err => err
        }
      }
      case _ => {
        parsePrimaryExpr(tokens)
      }
    }
  }

  def parsePrimaryExpr(tokens: List[Token]): MayError[(Expr, List[Token])] = {
    Left("parsePrimaryExpr is not implemented")
  }

  def genError(expected: String, tokens: List[Token]): String = {
    val actual = tokens match {
      case token::_ => s"`${token.toString}`"
      case Nil => "end of input"
    }

    s"expect ${expected}, but got ${actual}"
  }

  class Lexer {
    val rules = List[(Regex, String => Option[Token])](
      ("[ ]".r, (s) => None),
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
      ("while".r, (s) => Some(WhileToken)),
      ("pass".r, (s) => Some(PassToken)),
      ("[0-9]+".r, (s) => Some(NumToken(s))),
      ("[a-zA-Z_][a-zA-Z_0-9]*".r, (s) => Some(NameToken(s)))
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
