import org.scalatest.FunSpec
import org.scalatest.Matchers._
import indentp._

class ParserTest extends FunSpec {
  describe("Parser") {
    val parser = new Parser
    describe("parse") {
      describe("with `x = 42`") {
        it("returns AssignStmt") {
          pending
          val stmt = ExprStmt(AssignExpr("x", Num(42)))
          parser.parse("x = 42") should equal(Right(Program(List(stmt))))
        }
      }

      describe("with arith expr") {
        it("returns Expr") {
          pending
          val stmt = ExprStmt(BinOpExpr("+", Num(1), BinOpExpr("*", Num(2), UnOpExpr("-", Num(3)))))
          parser.parse("1 + 2 * -3") should equal(Right(Program(List(stmt))))
        }
      }

      describe("with function call expr") {
        it("returns Expr") {
          pending
          val stmt = ExprStmt(CallExpr(CallExpr(CallExpr(Var("f"), List()), List(Var("x"))), List(Var("y"), Var("z"))))
          parser.parse("f()(x)(y, z)") should equal(Right(Program(List(stmt))))
        }
      }

      describe("with logical expr") {
        it("returns Expr") {
          pending
          val stmt = ExprStmt(UnOpExpr("!", BinOpExpr("==", Var("x"), BinOpExpr("+", Var("y"), Num(42)))))
          parser.parse("!(x == y + 42)") should equal(Right(Program(List(stmt))))
        }
      }

      describe("with multiline code `x = 42\\nf()`") {
        it("returns two Expr") {
          pending
          val first = ExprStmt(AssignExpr("x", Num(42)))
          val second = ExprStmt(CallExpr(Var("f"), List()))
          parser.parse("x = 42\\nf()`") should equal(Right(Program(List(first, second))))
        }
      }

      describe("with pass stmt") {
        it("returns PassStmt") {
          pending
          parser.parse("pass") should equal(Right(Program(List(PassStmt))))
        }
      }
    }
  }
}
