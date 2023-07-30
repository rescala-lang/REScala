package kofre.datatypes.contextual

import kofre.base.{Bottom, Lattice}
import kofre.dotted.{DotFun, Dotted, DottedLattice, HasDots}
import kofre.syntax.{OpsSyntaxHelper, ReplicaId}
import kofre.time.{Dot, Dots}

/** An MultiVersionRegister (Multi-Value Register) is a Delta CRDT modeling a register.
  *
  * In the absence of concurrent writes, the MultiVersionRegister is either empty or holds one value.
  * When multiple values are written concurrently, reading the MultiVersionRegister returns a set holding all these values.
  */
case class MultiVersionRegister[A](repr: DotFun[A])

object MultiVersionRegister {
  def empty[A]: MultiVersionRegister[A] = MultiVersionRegister(DotFun.empty)

  given bottomInstance[A]: Bottom[MultiVersionRegister[A]] = Bottom.derived

  val assertEqualsOrdering: Ordering[Any] = (l, r) =>
    if l == r then 0
    else throw IllegalStateException(s"assumed equality does not hold for »$l« and »$r« ")

  given dottedLattice[A]: DottedLattice[MultiVersionRegister[A]] =
    given Lattice[A] = Lattice.fromOrdering(assertEqualsOrdering.on(identity))
    DottedLattice.derived

  given hasDot[A]: HasDots[MultiVersionRegister[A]] = HasDots.derived

  extension [C, A](container: C)
    def multiVersionRegister: syntax[C, A] = syntax(container)

  implicit class syntax[C, A](container: C) extends OpsSyntaxHelper[C, MultiVersionRegister[A]](container) {

    def read(using PermQuery): Set[A] = current.repr.repr.values.toSet

    def write(using ReplicaId)(v: A): CausalMutate = {
      val nextDot = context.nextDot(replicaId)

      Dotted(
        MultiVersionRegister(DotFun(Map(nextDot -> v))),
        Dots.from(current.repr.repr.keySet).add(nextDot)
      ).mutator
    }

    def clear(using PermCausalMutate)(): C =
      Dotted(
        MultiVersionRegister.empty,
        Dots.from(current.repr.repr.keySet)
      ).mutator
  }
}
