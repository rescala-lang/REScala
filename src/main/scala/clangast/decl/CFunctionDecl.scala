package clangast.decl

import clangast.toExpr
import clangast.stmt.{CCompoundStmt, CDeclStmt, CStmt}
import clangast.types.{CFunctionType, CQualType}
import clangast.given

import scala.quoted.{Expr, Quotes}

case class CFunctionDecl(name: String, parameters: List[CParmVarDecl], returnType: CQualType, body: Option[CCompoundStmt] = None) extends CValueDecl with CDeclContext {
  override def getType: CQualType = CFunctionType(parameters.map(_.declaredType), returnType)

  override def decls: List[CDecl] = body match {
    case None => List()
    case Some(stmt) => stmt.body.collect {
      case CDeclStmt(decl) => decl
    }
  }

  override def textgen: String =
    val decl = s"${returnType.textgen} $name(${parameters.map(_.textgen).mkString(", ")})"

    body match {
      case None => decl + ";"
      case Some(stmt) => decl + " " + stmt.textgen
    }

  override def toExpr(using Quotes): Expr[CFunctionDecl] = {
    val nameExpr = Expr(name)
    val parametersExpr = Expr.ofList(parameters.map(_.toExpr))
    val returnTypeExpr = returnType.toExpr
    val bodyExpr = body.map(_.toExpr).toExpr

    '{ CFunctionDecl($nameExpr, $parametersExpr, $returnTypeExpr, $bodyExpr) }
  }
}
