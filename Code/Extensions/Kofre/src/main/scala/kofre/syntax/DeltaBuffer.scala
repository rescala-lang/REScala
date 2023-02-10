package kofre.syntax

import kofre.base.{Bottom, Lattice}
import kofre.dotted.{Dotted, DottedLattice}
import kofre.time.Dots
import kofre.base.Id

/** ReactiveCRDTs are Delta CRDTs that store applied deltas in their deltaBuffer attribute. Middleware should regularly
  * take these deltas and ship them to other replicas, using applyDelta to apply them on the remote state. After deltas
  * have been read and propagated by the middleware, it should call resetDeltaBuffer to empty the deltaBuffer.
  */
case class DeltaBuffer[State](
    state: State,
    deltaBuffer: List[State] = Nil
) {
  def applyDelta(delta: State)(using Lattice[State]): DeltaBuffer[State] =
    DeltaBuffer(state merge delta, delta :: deltaBuffer)
  def clearDeltas() = DeltaBuffer(state)
}

object DeltaBuffer {

  given dottedPermissions[L: DottedLattice]: PermCausalMutate[DeltaBuffer[Dotted[L]], L] = new {
    override def query(c: DeltaBuffer[Dotted[L]]): L = c.state.store
    override def mutateContext(
        container: DeltaBuffer[Dotted[L]],
        withContext: Dotted[L]
    ): DeltaBuffer[Dotted[L]] = container.applyDelta(withContext)

    override def context(c: DeltaBuffer[Dotted[L]]): Dots = c.state.context
  }

  given plainPermissions[L: Lattice]: PermMutate[DeltaBuffer[L], L] = new {
    override def mutate(c: DeltaBuffer[L], delta: L): DeltaBuffer[L] = c.applyDelta(delta)
    override def query(c: DeltaBuffer[L]): L                         = c.state
  }
}
