package lofi_acl.ardt.datatypes

import rdts.base.LocalUid
import rdts.dotted.Dotted
import rdts.time.{Dot, Dots}

import scala.language.implicitConversions

opaque type MultiValueRegister[V] = Dotted[Map[Dot, V]]

object MultiValueRegister:
  extension [V](reg: MultiValueRegister[V])
    def read: Set[V] = {
      reg.data.values.toSet
    }

  private[datatypes] given regToCausal[V]: Conversion[MultiValueRegister[V], Dotted[Map[Dot, V]]] = identity
  private[datatypes] given causalToReg[V]: Conversion[Dotted[Map[Dot, V]], MultiValueRegister[V]] = identity

  object mutators:
    def write[V](register: MultiValueRegister[V], value: V, replicaId: LocalUid): MultiValueRegister[V] =
      val dot = register.context.nextDot(replicaId.uid)
      Dotted(
        Map(dot -> value),
        Dots.from(register.data.keySet + dot)
      )

    def clear[V](register: MultiValueRegister[V]): MultiValueRegister[V] =
      Dotted(
        Map.empty,
        Dots.from(register.data.keySet)
      )
