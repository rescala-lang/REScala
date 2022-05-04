package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.annotation.targetName

case class CPlusExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "+"
}

/*object + {
  def unapply(expr: CPlusExpr): Option[(CExpr, CExpr)] = Some((expr.lhs, expr.rhs))
}*/
