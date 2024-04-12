package com.github.ckuessner.ardt.datatypes

import com.github.ckuessner.ardt.base.{Causal, Lattice}
import com.github.ckuessner.ardt.causality.DotStore
import com.github.ckuessner.ardt.causality.DotStore.DotFun
import com.github.ckuessner.ardt.causality.impl.ArrayCausalContext

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
    def write[V](register: MultiValueRegister[V], value: V, replicaId: String): MultiValueRegister[V] =
      val dot = register.causalContext.clockOf(replicaId).advance(replicaId)
      Causal(
        Map(dot -> value),
        ArrayCausalContext.fromSet(register.dotStore.keySet).add(dot.replicaId, dot.time)
      )

    def clear[V](register: MultiValueRegister[V]): MultiValueRegister[V] =
      Causal(
        DotStore[DotFun[V]].bottom,
        ArrayCausalContext.fromSet(register.dotStore.keySet)
      )
