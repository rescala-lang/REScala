package kofre.decompose.interfaces

import kofre.base.DecomposeLattice
import kofre.causality.CausalContext
import kofre.decompose.*
import kofre.syntax.OpsSyntaxHelper
import kofre.causality.Dot
import kofre.decompose.interfaces.LWWRegisterInterface.LWWRegister
import kofre.contextual.WithContext

/** An MVRegister (Multi-Value Register) is a Delta CRDT modeling a register.
  *
  * In the absence of concurrent writes, the MVRegister is either empty or holds one value.
  * When multiple values are written concurrently, reading the MVRegister returns a set holding all these values.
  */
object MVRegisterInterface {
  def empty[A]: MVRegister[A] = Map.empty

  type MVRegister[A] = Map[Dot, A]

  implicit class MVRegisterSyntax[C, A](container: C) extends OpsSyntaxHelper[C, MVRegister[A]](container) {

    def read(using QueryP): Set[A] = current.values.toSet

    def write(v: A)(using CausalMutationP, IdentifierP): C = {
      val nextDot = context.nextDot(replicaID)

      WithContext(
        Map(nextDot -> v),
        CausalContext.fromSet(current.keySet + nextDot)
      ).mutator
    }

    def clear()(using CausalMutationP, DecomposeLattice[MVRegister[A]]): C =
      WithContext(
        DecomposeLattice[MVRegister[A]].empty,
        CausalContext.fromSet(current.keySet)
      ).mutator
  }
}
