package kofre.decompose.interfaces

import kofre.base.{Bottom, DecomposeLattice}
import kofre.time.Dots
import kofre.decompose.*
import kofre.syntax.OpsSyntaxHelper
import kofre.time.Dot
import kofre.dotted.{DotFun, Dotted, DottedDecompose}

/** An MVRegister (Multi-Value Register) is a Delta CRDT modeling a register.
  *
  * In the absence of concurrent writes, the MVRegister is either empty or holds one value.
  * When multiple values are written concurrently, reading the MVRegister returns a set holding all these values.
  */

case class MVRegister[A](repr: DotFun[A])

object MVRegister {
  def empty[A]: MVRegister[A] = MVRegister(DotFun.empty)

  given bottomInstance[A]: Bottom[MVRegister[A]] = Bottom.derived
  given dottedDecompose[A: DecomposeLattice]: DottedDecompose[MVRegister[A]] = DottedDecompose.derived

  implicit class syntax[C, A](container: C) extends OpsSyntaxHelper[C, MVRegister[A]](container) {

    def read(using QueryP): Set[A] = current.repr.values.toSet

    def write(v: A)(using CausalMutationP, IdentifierP): C = {
      val nextDot = context.nextDot(replicaID)

      Dotted(
        MVRegister(DotFun(Map(nextDot -> v))),
        Dots.from(current.repr.keySet + nextDot)
      ).mutator
    }

    def clear()(using CausalMutationP): C =
      Dotted(
        MVRegister.empty,
        Dots.from(current.repr.keySet)
      ).mutator
  }
}
