package clangast.expr

case class CCallExpr(callee: CExpr, args: List[CExpr]) extends CExpr {
  override def textgen: String = s"$callee(${args.map(_.textgen).mkString(", ")})"
}
