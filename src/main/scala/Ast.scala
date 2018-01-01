package indentp

sealed abstract class Ast

/*
  Program: (Stmt)*
 */
case class Program(stmts: List[Stmt])

/*
  Stmt: ExprStmt
      | IfStmt
      | WhileStmt
      | PassStmt
 */
abstract class Stmt extends Ast

/*
  ExprStmt: Expr
 */
case class ExprStmt(body: Expr) extends Stmt

/*
  IfStmt: `if` `(` Expr `)` (Stmt)+
 */
case class IfStmt(cond: Expr, stmts: List[Stmt]) extends Stmt

/*
  WhileStmt: `while` `(` Expr `)` (Stmt)+
 */
case class WhileStmt(cond: Expr, stmts: List[Stmt]) extends Stmt

/*
  PassStmt: `pass`
 */
case object PassStmt extends Stmt

/*
  Expr: AssignExpr
 */
abstract class Expr extends Ast

/*
  AssignExpr: Name `=` AssignExpr
            | LogicalExpr
 */
case class AssignExpr(name: String, expr: Expr) extends Expr

/*
  LogicalExpr: LogicalExpr `==` AddExpr
             | AddExpr

  AddExpr: AddExpr `+` MulExpr
         | MulExpr

  MulExpr: MulExpr `*` PrimaryExpr
         | PrimaryExpr
 */
case class class BinOpExpr(op: String, left: Expr, right: Expr) extends Expr
