package lofi_acl.ardt.datatypes

import lofi_acl.ardt.base.Causal
import lofi_acl.ardt.causality.DotStore
import lofi_acl.ardt.causality.DotStore.DotFun
import lofi_acl.ardt.base.Causal
import rdts.syntax.LocalUid
import rdts.time.Dots

import scala.language.implicitConversions

opaque type MultiValueRegister[V] = Causal[DotFun[V]]

object MultiValueRegister:
  extension [V](reg: MultiValueRegister[V])
    def read: Set[V] = {
      reg.dotStore.values.toSet
    }

  private[datatypes] given regToCausal[V]: Conversion[MultiValueRegister[V], Causal[DotFun[V]]] = identity
  private[datatypes] given causalToReg[V]: Conversion[Causal[DotFun[V]], MultiValueRegister[V]] = identity

  object mutators:
    def write[V](register: MultiValueRegister[V], value: V, replicaId: LocalUid): MultiValueRegister[V] =
      val dot = register.causalContext.nextDot(replicaId.uid)
      Causal(
        Map(dot -> value),
        Dots.from(register.dotStore.keySet + dot)
      )

    def clear[V](register: MultiValueRegister[V]): MultiValueRegister[V] =
      Causal(
        DotStore[DotFun[V]].empty,
        Dots.from(register.dotStore.keySet)
      )
