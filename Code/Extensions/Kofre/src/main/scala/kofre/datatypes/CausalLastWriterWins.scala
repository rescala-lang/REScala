package kofre.datatypes

import kofre.base.{Bottom, DecomposeLattice}
import kofre.datatypes.MultiVersionRegister.syntax
import kofre.datatypes.{MultiVersionRegister, TimedVal}
import kofre.dotted.{DotFun, Dotted, DottedDecompose, DottedLattice}
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, PermIdMutate}

/** An LWW (Last Writer Wins) is a Delta CRDT modeling a register.
  *
  * If two concurrent write operations occur, the resulting LWW takes on the value of the write operation with the later timestamp.
  */
case class CausalLastWriterWins[A](repr: MultiVersionRegister[TimedVal[A]])

object CausalLastWriterWins {
  def empty[A]: CausalLastWriterWins[A] = CausalLastWriterWins(MultiVersionRegister.empty)

  given bottomInstance[A]: Bottom[CausalLastWriterWins[A]]           = Bottom.derived
  given dottedDecompose[A]: DottedDecompose[CausalLastWriterWins[A]] = DottedDecompose.derived

  implicit class syntax[C, A](container: C)
      extends OpsSyntaxHelper[C, CausalLastWriterWins[A]](container) {

    def read(using QueryP): Option[A] =
      MultiVersionRegister.syntax(current.repr).read.reduceOption(DecomposeLattice[TimedVal[A]].merge).map(x => x.value)

    def write(using CausalMutationP, IdentifierP)(v: A): C =
      MultiVersionRegister.syntax(current.repr.inherit).write(TimedVal(v, replicaID)).anon.map(
        CausalLastWriterWins.apply
      ).mutator

    def map(using CausalMutationP, IdentifierP)(f: A => A): C =
      read.map(f) match {
        case None    => Dotted(CausalLastWriterWins.empty).mutator
        case Some(v) => write(v)
      }

    def clear(using CausalMutationP)(): C =
      MultiVersionRegister.syntax(current.repr.inheritContext).clear().map(
        CausalLastWriterWins.apply
      ).mutator
  }
}
