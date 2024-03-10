package rdts.syntax

import rdts.base.Lattice
import rdts.dotted.{Dotted}
import rdts.time.Dots

/** ReactiveCRDTs are Delta CRDTs that store applied deltas in their deltaBuffer attribute. Middleware should regularly
  * take these deltas and ship them to other replicas, using applyDelta to apply them on the remote state. After deltas
  * have been read and propagated by the middleware, it should call resetDeltaBuffer to empty the deltaBuffer.
  */
case class DeltaBuffer[State](
    state: State,
    deltaBuffer: List[State] = Nil
) {
  def applyDelta(delta: State)(using Lattice[State]): DeltaBuffer[State] =
    val merged = state merge delta
    DeltaBuffer(merged, if merged == state then deltaBuffer else delta :: deltaBuffer)
  def clearDeltas() = DeltaBuffer(state)

  def mutable: DeltaBufferContainer[State] = new DeltaBufferContainer(this)

  def transform(f: State => State)(using Lattice[State]) = applyDelta(f(state))
}

object DeltaBuffer {

  given dottedPermissions[L](using Lattice[Dotted[L]]): PermCausalMutate[DeltaBuffer[Dotted[L]], L] = new {
    override def query(c: DeltaBuffer[Dotted[L]]): L = c.state.data
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

  given plainNestedPermissions[L: Lattice]: PermMutate[DeltaBuffer[Dotted[L]], L] = new {
    override def mutate(c: DeltaBuffer[Dotted[L]], delta: L): DeltaBuffer[Dotted[L]] =
      given Lattice[Dotted[L]] = Lattice.derived
      c.applyDelta(Dotted(delta))

    override def query(c: DeltaBuffer[Dotted[L]]): L = c.state.data
  }
}

class DeltaBufferContainer[State](var result: DeltaBuffer[State]) {
  def applyDelta(delta: State)(using Lattice[State]): Unit =
    result = result.applyDelta(delta)
}

object DeltaBufferContainer extends DeltaBufferContainer.LowPrio {

  given dottedPermissions[L](using
      pcm: PermCausalMutate[DeltaBuffer[Dotted[L]], L]
  ): PermCausalMutate[DeltaBufferContainer[Dotted[L]], L] = new {
    override def query(c: DeltaBufferContainer[Dotted[L]]): L = c.result.state.data
    override def mutateContext(
        container: DeltaBufferContainer[Dotted[L]],
        withContext: Dotted[L]
    ): DeltaBufferContainer[Dotted[L]] =
      container.result = pcm.mutateContext(container.result, withContext)
      container

    override def context(c: DeltaBufferContainer[Dotted[L]]): Dots = c.result.state.context
  }

  given plainPermissions[L](using pm: PermMutate[DeltaBuffer[L], L]): PermMutate[DeltaBufferContainer[L], L] = new {
    override def mutate(c: DeltaBufferContainer[L], delta: L): DeltaBufferContainer[L] =
      c.result = pm.mutate(c.result, delta)
      c
    override def query(c: DeltaBufferContainer[L]): L = c.result.state
  }

  trait LowPrio {
    given nestedPlainPermissions[L](using
        pm: PermMutate[DeltaBuffer[Dotted[L]], L]
    ): PermMutate[DeltaBufferContainer[Dotted[L]], L] = new {
      override def mutate(c: DeltaBufferContainer[Dotted[L]], delta: L): DeltaBufferContainer[Dotted[L]] =
        c.result = pm.mutate(c.result, delta)
        c

      override def query(c: DeltaBufferContainer[Dotted[L]]): L = c.result.state.data
    }
  }
}
