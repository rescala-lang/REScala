package clangast.decl
import scala.quoted.{Expr, Quotes}

trait CNamedDecl extends CDecl {
  val name: String

  override def toExpr(using Quotes): Expr[CNamedDecl]
}
