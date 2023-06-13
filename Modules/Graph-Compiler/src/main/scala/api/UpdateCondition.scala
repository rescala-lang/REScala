package api

import clangast.expr.binaryop.{CAndExpr, COrExpr}
import clangast.expr.{CExpr, CParenExpr, CTrueLiteral}

case class UpdateCondition(normalized: Set[Set[CExpr]]) {
  def and(cond: UpdateCondition): UpdateCondition =
    UpdateCondition(
      normalized.foldLeft(Set[Set[CExpr]]()) { (acc, ands) =>
        acc union cond.normalized.map { _ union ands }
      }
    )

  def or(cond: UpdateCondition): UpdateCondition =
    UpdateCondition(normalized union cond.normalized)

  def add(v: CExpr): UpdateCondition = UpdateCondition(normalized.map(_ + v))

  def compile: CExpr =
    normalized.toList.map(compileAnd) match {
      case Nil                       => CTrueLiteral
      case List(CParenExpr(subExpr)) => subExpr
      case andList                   => andList.reduceLeft(COrExpr.apply)
    }

  private def compileAnd(s: Set[CExpr]): CExpr = {
    s.toList match {
      case List(cond) => cond
      case l          => CParenExpr(l.tail.foldLeft(l.head)(CAndExpr.apply))
    }
  }
}

object UpdateCondition {
  val empty: UpdateCondition = UpdateCondition(Set.empty[Set[CExpr]])

  def apply(v: CExpr): UpdateCondition = UpdateCondition(Set(Set(v)))
}
