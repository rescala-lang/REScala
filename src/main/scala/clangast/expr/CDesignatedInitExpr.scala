package clangast.expr

case class CDesignatedInitExpr(inits: List[(String, CExpr)]) extends CExpr {
  override def textgen: String =
    "{" +
      inits.map { case (fieldName, expr) => s".$fieldName = ${expr.textgen}" }.mkString(", ") +
      "}"
}
