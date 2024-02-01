package loci
package transmitter

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object DummyImplicit {
  class Resolvable private[DummyImplicit]

  object Resolvable {
    implicit def dummy: Resolvable = new Resolvable
    implicit def noDummy: Resolvable = macro NoDummyImplicit.skip
  }

  class Unresolvable private[DummyImplicit]

  object Unresolvable {
    implicit def noDummy: Unresolvable = macro NoDummyImplicit.skip
  }
}

object NoDummyImplicit {
  def skip(c: whitebox.Context): c.Tree =
    c.abort(c.enclosingPosition, "`noDummy` must not be called")
}
