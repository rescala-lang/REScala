package clangast.decl
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CTranslationUnitDecl(children: List[CDecl], includes: List[CInclude]) extends CDecl with CDeclContext {
  override def decls: List[CDecl] = children

  override def textgen: String =
    includes.map(_.textgen).mkString("\n") + "\n\n" + children.map(_.textgen).mkString("\n\n")

  override def toExpr(using Quotes): Expr[CTranslationUnitDecl] = {
    val childrenExpr = Expr.ofList(children.map(_.toExpr))
    val includesExpr = Expr.ofList(includes.map(_.toExpr))

    '{ CTranslationUnitDecl($childrenExpr, $includesExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CTranslationUnitDecl =
    CTranslationUnitDecl(children.map(mapper.mapCDecl), includes)
}
