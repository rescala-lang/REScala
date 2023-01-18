package kofre.datatypes

import kofre.base.{Bottom, Lattice}
import kofre.dotted.{DotFun, Dotted, DottedLattice}
import kofre.syntax.OpsSyntaxHelper
import kofre.time.{Dot, Dots}

/** An MultiVersionRegister (Multi-Value Register) is a Delta CRDT modeling a register.
  *
  * In the absence of concurrent writes, the MultiVersionRegister is either empty or holds one value.
  * When multiple values are written concurrently, reading the MultiVersionRegister returns a set holding all these values.
  */

case class MultiVersionRegister[A](repr: DotFun[A])

object MultiVersionRegister {
  def empty[A]: MultiVersionRegister[A] = MultiVersionRegister(DotFun.empty)

  given bottomInstance[A]: Bottom[MultiVersionRegister[A]]                             = Bottom.derived
  given dottedLattice[A: Lattice]: DottedLattice[MultiVersionRegister[A]] = DottedLattice.derived

  extension [C, A](container: C)
    def multiVersionRegister: syntax[C, A] = syntax(container)

  implicit class syntax[C, A](container: C) extends OpsSyntaxHelper[C, MultiVersionRegister[A]](container) {

    def read(using PermQuery): Set[A] = current.repr.values.toSet

    def write(using PermCausalMutate, PermId)(v: A): C = {
      val nextDot = context.nextDot(replicaId)

      Dotted(
        MultiVersionRegister(DotFun(Map(nextDot -> v))),
        Dots.from(current.repr.keySet + nextDot)
      ).mutator
    }

    def clear(using PermCausalMutate)(): C =
      Dotted(
        MultiVersionRegister.empty,
        Dots.from(current.repr.keySet)
      ).mutator
  }
}
