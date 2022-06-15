package api

import clangast.decl.CValueDecl
import clangast.expr.binaryop.{CAndExpr, COrExpr}
import clangast.expr.{CDeclRefExpr, CExpr, CParenExpr, CTrueLiteral}

case class UpdateCondition(normalized: Set[Set[CValueDecl]]) {
  def and(cond: UpdateCondition): UpdateCondition =
    UpdateCondition(
      normalized.foldLeft(Set[Set[CValueDecl]]()) { (acc, ands) =>
        acc union cond.normalized.map { _ union ands }
      }
    )

  def or(cond: UpdateCondition): UpdateCondition =
    UpdateCondition(normalized union cond.normalized)

  def add(v: CValueDecl): UpdateCondition = UpdateCondition(normalized.map(_ + v))

  def compile: CExpr =
    if (normalized.isEmpty) CTrueLiteral
    else {
      val andList = normalized.toList.map(compileAnd)

      andList.tail.foldLeft(andList.head)(COrExpr.apply)
    }

  private def compileAnd(s: Set[CValueDecl]): CExpr = {
    val l = s.toList.map(_.ref)

    CParenExpr(l.tail.foldLeft(l.head)(CAndExpr.apply))
  }
}

object UpdateCondition {
  val empty: UpdateCondition = UpdateCondition(Set.empty[Set[CValueDecl]])
  
  def apply(v: CValueDecl): UpdateCondition = UpdateCondition(Set(Set(v)))
}
