package rdts.syntax

import rdts.base.Lattice
import rdts.dotted.Dotted
import rdts.time.Dots

/** ReactiveCRDTs are Delta CRDTs that store applied deltas in their deltaBuffer attribute. Middleware should regularly
  * take these deltas and ship them to other replicas, using applyDelta to apply them on the remote state. After deltas
  * have been read and propagated by the middleware, it should call resetDeltaBuffer to empty the deltaBuffer.
  */
case class DeltaBuffer[A](
    state: A,
    deltaBuffer: List[A] = Nil
) {
  def applyDelta(delta: A)(using Lattice[A]): DeltaBuffer[A] =
    val merged = state merge delta
    DeltaBuffer(merged, if merged == state then deltaBuffer else delta :: deltaBuffer)
  def clearDeltas(): DeltaBuffer[A] = DeltaBuffer(state)

  def mutable: DeltaBufferContainer[A] = new DeltaBufferContainer(this)

  def transform(f: A => A)(using Lattice[A]): DeltaBuffer[A] = applyDelta(f(state))

  inline def mod(f: A => A)(using Lattice[A]): DeltaBuffer[A] = {
    applyDelta(f(state))
  }

}

object DeltaBuffer {

  extension [A](curr: DeltaBuffer[Dotted[A]])(using Lattice[Dotted[A]]) {
    inline def modd(f: A => Dots ?=> Dotted[A]): DeltaBuffer[Dotted[A]] = {
      curr.applyDelta(curr.state.mod(f(_)))
    }
  }

  extension [A](curr: DeltaBuffer[Dotted[A]]) {
    def data: A = curr.state.data
  }
}

class DeltaBufferContainer[A](var result: DeltaBuffer[A]) {
  def applyDelta(delta: A)(using Lattice[A]): Unit =
    result = result.applyDelta(delta)

  def mod(f: A => A)(using Lattice[A]): DeltaBufferContainer[A] = {
    applyDelta(f(result.state))
    this
  }
}

object DeltaBufferContainer {

  extension [A](curr: DeltaBufferContainer[Dotted[A]])(using Lattice[Dotted[A]]) {
    inline def modd(f: A => Dots ?=> Dotted[A]): Unit = {
      curr.applyDelta(curr.result.state.mod(f(_)))
    }
  }

  extension [A](curr: DeltaBufferContainer[Dotted[A]]) {
    def data: A = curr.result.state.data
  }

}
