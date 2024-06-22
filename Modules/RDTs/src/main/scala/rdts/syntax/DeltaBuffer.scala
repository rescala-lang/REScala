package rdts.syntax

import rdts.base.Lattice
import rdts.dotted.Dotted
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
  def clearDeltas(): DeltaBuffer[State] = DeltaBuffer(state)

  def mutable: DeltaBufferContainer[State] = new DeltaBufferContainer(this)

  def transform(f: State => State)(using Lattice[State]): DeltaBuffer[State] = applyDelta(f(state))

}

object DeltaBuffer {

  extension [A](curr: DeltaBuffer[Dotted[A]])(using Lattice[Dotted[A]]) {
    inline def mod(f: Dots ?=> A => Dotted[A]): DeltaBuffer[Dotted[A]] = {
      curr.applyDelta(curr.state.mod(f(_)))
    }
    inline def modn(f: A => A): DeltaBuffer[Dotted[A]] = {
      curr.applyDelta(Dotted(f(curr.state.data)))
    }
  }

  extension [A](curr: DeltaBuffer[Dotted[A]]) {
    def data: A = curr.state.data
  }

  extension [A](curr: DeltaBuffer[A])(using Lattice[A]) {

    inline def modp(f: A => A): DeltaBuffer[A] = {
      curr.applyDelta(f(curr.state))
    }
  }

}

class DeltaBufferContainer[State](var result: DeltaBuffer[State]) {
  def applyDelta(delta: State)(using Lattice[State]): Unit =
    result = result.applyDelta(delta)
}

object DeltaBufferContainer {

  extension [A](curr: DeltaBufferContainer[Dotted[A]])(using Lattice[Dotted[A]]) {
    inline def mod(f: Dots ?=> A => Dotted[A]): Unit = {
      curr.applyDelta(curr.result.state.mod(f(_)))
    }
    inline def modn(f: A => A): DeltaBufferContainer[Dotted[A]] = {
      curr.applyDelta(Dotted(f(curr.result.state.data)))
      curr
    }
  }

  extension [A](curr: DeltaBufferContainer[A])(using Lattice[A]) {

    inline def modp(f: A => A): DeltaBufferContainer[A] = {
      curr.applyDelta(f(curr.result.state))
      curr
    }
  }

  extension [A](curr: DeltaBufferContainer[Dotted[A]]) {
    def data: A = curr.result.state.data
  }


}
