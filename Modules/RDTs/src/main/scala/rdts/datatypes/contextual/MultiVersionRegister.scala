package rdts.datatypes.contextual

import rdts.base.{Bottom, Lattice}
import rdts.dotted.{Dotted, HasDots}
import rdts.syntax.{LocalReplicaId, OpsSyntaxHelper}
import rdts.time.{Dot, Dots}

/** An MultiVersionRegister (Multi-Value Register) is a Delta CRDT modeling a register.
  *
  * In the absence of concurrent writes, the MultiVersionRegister is either empty or holds one value.
  * When multiple values are written concurrently, reading the MultiVersionRegister returns a set holding all these values.
  */
case class MultiVersionRegister[A](repr: Map[Dot, A])

object MultiVersionRegister {
  def empty[A]: MultiVersionRegister[A] = MultiVersionRegister(Map.empty)

  given bottomInstance[A]: Bottom[MultiVersionRegister[A]] = Bottom.derived

  private val _assertEqualsOrdering: Ordering[Any] = (l, r) =>
    if l == r then 0
    else throw IllegalStateException(s"assumed equality does not hold for »$l« and »$r« ")
  // we could replace this by casting …
  def assertEqualsOrdering[A]: Ordering[A] = _assertEqualsOrdering.on(identity)
  def assertEqualsLattice[A]: Lattice[A]   = Lattice.fromOrdering(using assertEqualsOrdering)

  given dottedLattice[A]: Lattice[MultiVersionRegister[A]] =
    given Lattice[A] = Lattice.fromOrdering(using assertEqualsOrdering)
    Lattice.derived

  given hasDot[A]: HasDots[MultiVersionRegister[A]] = HasDots.derived

  extension [C, A](container: C)
    def multiVersionRegister: syntax[C, A] = syntax(container)

  implicit class syntax[C, A](container: C) extends OpsSyntaxHelper[C, MultiVersionRegister[A]](container) {

    def read(using IsQuery): Set[A] = current.repr.values.toSet

    def write(using LocalReplicaId)(v: A): CausalMutator = {
      val nextDot = context.nextDot(replicaId)

      Dotted(
        MultiVersionRegister(Map(nextDot -> v)),
        Dots.from(current.repr.keySet).add(nextDot)
      ).mutator
    }

    def clear(using IsCausalMutator)(): C =
      Dotted(
        MultiVersionRegister.empty,
        Dots.from(current.repr.keySet)
      ).mutator
  }
}
