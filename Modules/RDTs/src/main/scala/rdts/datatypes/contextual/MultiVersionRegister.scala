package rdts.datatypes.contextual

import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.dotted.{Dotted, HasDots}
import rdts.time.{Dot, Dots}

/** An MultiVersionRegister (Multi-Value Register) is a Delta CRDT modeling a register.
  *
  * In the absence of concurrent writes, the MultiVersionRegister is either empty or holds one value.
  * When multiple values are written concurrently, reading the MultiVersionRegister returns a set holding all these values.
  */
case class MultiVersionRegister[A](repr: Map[Dot, A]) {

  type Delta = Dotted[MultiVersionRegister[A]]

  def read: Set[A] = repr.values.toSet

  def write(using LocalUid)(v: A)(using context: Dots): Delta = {
    val nextDot = context.nextDot(LocalUid.replicaId)

    Dotted(
      MultiVersionRegister(Map(nextDot -> v)),
      Dots.from(repr.keySet).add(nextDot)
    )
  }

  def clear(): Delta =
    Dotted(
      MultiVersionRegister.empty,
      Dots.from(repr.keySet)
    )
}

object MultiVersionRegister {
  def empty[A]: MultiVersionRegister[A] = MultiVersionRegister(Map.empty)

  given bottomInstance[A]: Bottom[MultiVersionRegister[A]] = Bottom.derived

  given dottedLattice[A]: Lattice[MultiVersionRegister[A]] =
    given Lattice[A] = Lattice.assertEquals
    Lattice.derived

  given hasDot[A]: HasDots[MultiVersionRegister[A]] = HasDots.derived

}
