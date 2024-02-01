package loci
package transmitter

import scala.annotation.unchecked.uncheckedVariance
import scala.quoted.*

sealed trait IsInferred[T]

sealed trait IsInferredDefault:
  transparent inline given [T]: IsInferred[T] =
    IsInferred.implementation: IsInferred.Impl[T]

object IsInferred extends IsInferredDefault:
  type NotInferred

  transparent inline given IsInferred[NotInferred] =
    IsInferred.implementation: IsInferred.Impl[NotInferred]

  sealed trait Impl[-T] extends IsInferred[T @uncheckedVariance]

  object implementation extends Impl[scala.Any]

  inline def apply[T: Type](using quotes: Quotes): Boolean =
    apply(quotes.reflect.TypeRepr.of[T])

  inline def apply(using quotes: Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    !(quotes.reflect.TypeRepr.of[NotInferred] =:= tpe)
