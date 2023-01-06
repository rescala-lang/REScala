package kofre.decompose.interfaces

import kofre.base.{Bottom, DecomposeLattice}
import kofre.time.Dots
import kofre.decompose.*
import kofre.syntax.OpsSyntaxHelper
import kofre.time.Dot
import kofre.dotted.{DotFun, Dotted, DottedDecompose}

/** An MultiVersionRegister (Multi-Value Register) is a Delta CRDT modeling a register.
  *
  * In the absence of concurrent writes, the MultiVersionRegister is either empty or holds one value.
  * When multiple values are written concurrently, reading the MultiVersionRegister returns a set holding all these values.
  */

case class MultiVersionRegister[A](repr: DotFun[A])

object MultiVersionRegister {
  def empty[A]: MultiVersionRegister[A] = MultiVersionRegister(DotFun.empty)

  given bottomInstance[A]: Bottom[MultiVersionRegister[A]] = Bottom.derived
  given dottedDecompose[A: DecomposeLattice]: DottedDecompose[MultiVersionRegister[A]] = DottedDecompose.derived

  implicit class syntax[C, A](container: C) extends OpsSyntaxHelper[C, MultiVersionRegister[A]](container) {

    def read(using QueryP): Set[A] = current.repr.values.toSet

    def write(v: A)(using CausalMutationP, IdentifierP): C = {
      val nextDot = context.nextDot(replicaID)

      Dotted(
        MultiVersionRegister(DotFun(Map(nextDot -> v))),
        Dots.from(current.repr.keySet + nextDot)
      ).mutator
    }

    def clear()(using CausalMutationP): C =
      Dotted(
        MultiVersionRegister.empty,
        Dots.from(current.repr.keySet)
      ).mutator
  }
}
