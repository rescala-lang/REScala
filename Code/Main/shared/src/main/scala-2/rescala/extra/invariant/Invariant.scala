package rescala.extra.invariant

import scala.annotation.nowarn
import scala.reflect.macros.blackbox

object Invariant {
  def apply[T](inv: T => Boolean): Invariant[T] = macro InvariantInterface.createInvariantImpl[T]
}

class InvariantInterface(val c: blackbox.Context) {
  @nowarn("msg=is never used")
  def createInvariantImpl[T: c.WeakTypeTag](inv: c.Expr[T => Boolean]): c.Expr[Invariant[T]] = {
    import c.universe._

    val invariantRep     = showCode(inv.tree)
    val invariantRepTree = Literal(Constant(invariantRep))
    val invarientRepExpr = c.Expr[String](invariantRepTree)
    reify(new Invariant[T](invarientRepExpr.splice, inv.splice))
  }
}

class Invariant[T](val description: String, val inv: T => Boolean) {
  def validate(value: T): Boolean = inv(value)
}
