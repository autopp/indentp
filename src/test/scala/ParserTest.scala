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
    }
  }
}
