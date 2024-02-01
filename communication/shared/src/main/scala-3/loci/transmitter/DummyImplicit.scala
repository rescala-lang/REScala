package loci
package transmitter

import scala.quoted.*

object DummyImplicit:
  sealed trait Resolvable

  object Resolvable:
    object instance extends Resolvable
    transparent inline given dummy: Resolvable = instance
    transparent inline given noDummy: Resolvable = ${ NoDummyImplicit.skip }

  sealed trait Unresolvable

  object Unresolvable:
    transparent inline given noDummy: Unresolvable = ${ NoDummyImplicit.skip }

object NoDummyImplicit:
  def skip(using Quotes) =
    quotes.reflect.report.errorAndAbort("`noDummy` must not be called")
