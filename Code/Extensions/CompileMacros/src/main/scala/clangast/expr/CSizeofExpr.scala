package clangast.expr

import clangast.traversal.CASTMapper
import clangast.types.CType

import scala.quoted.{Expr, Quotes}

case class CSizeofExpr(arg: Either[CType, CExpr]) extends CExpr {
  override def textgen: String = arg match
    case Left(t)     => s"sizeof(${t.textgen})"
    case Right(expr) => s"sizeof(${expr.textgen})"

  override def toExpr(using Quotes): Expr[CSizeofExpr] = {
    val argExpr = arg match {
      case Left(tpe) =>
        val tpeExpr = tpe.toExpr
        '{ Left($tpeExpr) }
      case Right(expr) =>
        val exprExpr = expr.toExpr
        '{ Right($exprExpr) }
    }

    '{ CSizeofExpr($argExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CExpr =
    CSizeofExpr(arg match {
      case Left(tpe)   => Left(mapper.mapCType(tpe))
      case Right(expr) => Right(mapper.mapCExpr(expr))
    })
}
