package clangast.decl
import clangast.stmt.{CDeclStmt, CStmt}
import clangast.types.{CFunctionType, CQualType}
import clangast.given

case class CFunctionDecl(name: String, parameters: List[CParmVarDecl], returnType: CQualType, body: Option[List[CStmt]] = None) extends CValueDecl with CDeclContext {
  override def getType: CQualType = CFunctionType(parameters.map(_.declaredType), returnType)

  override def decls: List[CDecl] = body match {
    case None => List()
    case Some(stmts) => stmts.collect {
      case CDeclStmt(decl) => decl
    }
  }

  override def textgen: String =
    val decl = s"${returnType.textgen} $name(${parameters.map(_.textgen).mkString(", ")})"

    body match {
      case None =>
        decl + ";"
      case Some(stmts) =>
        s"""
           |$decl {
           |${stmts.map(_.textgen).mkString("\n").indent(2).stripTrailing()}
           |}
        """.strip().stripMargin
    }
}
