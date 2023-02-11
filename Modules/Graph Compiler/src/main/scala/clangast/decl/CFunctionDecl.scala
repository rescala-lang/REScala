package clangast.decl

import clangast.expr.{CExpr, CStmtExpr}
import clangast.toExpr
import clangast.stmt.{CCompoundStmt, CDeclStmt, CReturnStmt, CStmt}
import clangast.types.{CFunctionType, CQualType, CVoidType}
import clangast.given
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CFunctionDecl(
    name: String,
    parameters: List[CParmVarDecl],
    returnType: CQualType,
    body: Option[CCompoundStmt] = None,
    variadic: Boolean = false
) extends CValueDecl with CDeclContext {
  override def getType: CQualType = CFunctionType(parameters.map(_.declaredType), returnType)

  override def decls: List[CDecl] = body match {
    case None => List()
    case Some(stmt) => stmt.body.collect {
        case CDeclStmt(decl) => decl
      }
  }

  override def textgen: String =
    val params = parameters.map(_.textgen).mkString(", ") + (if variadic then ", ..." else "")

    val decl = s"${returnType.textgen} $name($params)"

    body match {
      case None       => decl + ";"
      case Some(stmt) => decl + " " + stmt.textgen
    }

  override def toExpr(using Quotes): Expr[CFunctionDecl] = {
    val nameExpr       = Expr(name)
    val parametersExpr = Expr.ofList(parameters.map(_.toExpr))
    val variadicExpr   = Expr(variadic)
    val returnTypeExpr = returnType.toExpr
    val bodyExpr       = body.map(_.toExpr).toExpr

    '{ CFunctionDecl($nameExpr, $parametersExpr, $returnTypeExpr, $bodyExpr, $variadicExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CFunctionDecl =
    CFunctionDecl(
      name,
      parameters.map(mapper.mapCParmVarDecl),
      mapper.mapCQualType(returnType),
      body.map(mapper.mapCCompoundStmt),
      variadic
    )

  override def declOnly: CFunctionDecl = this.copy(body = None)

  def inlineCall(args: List[CExpr]): CExpr = {
    def argDecls: List[CStmt] = (args zip parameters).map {
      case (a, p) => CVarDecl(p.name, p.declaredType, Some(a))
    }

    val mapper = new CASTMapper {
      override protected val mapCStmtHook: PartialFunction[CStmt, CStmt] = {
        case CReturnStmt(Some(retVal)) => retVal
      }
    }

    val inlinedBody: CStmt =
      if this.returnType.unqualType == CVoidType then mapper.mapCCompoundStmt(body.get)
      else CStmtExpr(mapper.mapCCompoundStmt(body.get))

    CStmtExpr(CCompoundStmt(argDecls :+ inlinedBody))
  }
}
