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

abstract class Expr extends Ast
