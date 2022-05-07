package clangast.decl
import scala.quoted.{Expr, Quotes}

case class CTranslationUnitDecl(children: List[CDecl]) extends CDecl with CDeclContext {
  override def decls: List[CDecl] = children

  override def textgen: String = children.map(_.textgen).mkString("\n\n")

  override def toExpr(using Quotes): Expr[CTranslationUnitDecl] = {
    val childrenExpr = Expr.ofList(children.map(_.toExpr))

    '{ CTranslationUnitDecl($childrenExpr) }
  }
}
