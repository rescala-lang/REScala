package clangast.decl

import clangast.toExpr
import clangast.traversal.CASTMapper

import scala.quoted.*

case class CTranslationUnitDecl(includes: List[CInclude], children: List[CDecl], ifndef: Option[String] = None)
    extends CDecl with CDeclContext {
  override def decls: List[CDecl] = children

  override def textgen: String = {
    val includesStr = if includes.isEmpty then "" else includes.map(_.textgen).mkString("\n") + "\n\n"
    val childrenStr = children.map(_.textgen).mkString("\n\n")

    ifndef match {
      case None    => includesStr + childrenStr
      case Some(s) => includesStr + s"#ifndef $s\n#define $s\n\n" + childrenStr + "\n\n#endif"
    }
  }

  override def toExpr(using Quotes): Expr[CTranslationUnitDecl] = {
    val includesExpr = Expr.ofList(includes.map(_.toExpr))
    val childrenExpr = Expr.ofList(children.map(_.toExpr))
    val ifndefExpr   = ifndef.map(Expr.apply).toExpr

    '{ CTranslationUnitDecl($includesExpr, $childrenExpr, $ifndefExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CTranslationUnitDecl =
    CTranslationUnitDecl(includes, children.map(mapper.mapCDecl), ifndef)
}
