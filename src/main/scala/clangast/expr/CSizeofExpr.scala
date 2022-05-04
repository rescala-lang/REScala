package clangast.expr

import clangast.types.CType

case class CSizeofExpr(arg: Either[CType, CExpr]) extends CExpr {
  override def textgen: String = arg match
    case Left(t) => s"sizeof(${t.textgen})"
    case Right(expr) => s"sizeof(${expr.textgen})"
}
