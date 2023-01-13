package kofre.datatypes

import kofre.base.{Bottom, DecomposeLattice}
import kofre.datatypes.MultiVersionRegister
import kofre.dotted.{DotFun, Dotted, DottedDecompose, DottedLattice}
import kofre.syntax.{OpsSyntaxHelper, PermIdMutate}
import kofre.datatypes.LastWriterWins.TimedVal

/** An LWW (Last Writer Wins) is a Delta CRDT modeling a register.
  *
  * If two concurrent write operations occur, the resulting LWW takes on the value of the write operation with the later timestamp.
  */
case class CausalLastWriterWins[A](repr: MultiVersionRegister[TimedVal[A]])

object CausalLastWriterWins {
  def empty[A]: CausalLastWriterWins[A] = CausalLastWriterWins(MultiVersionRegister.empty)

  given bottomInstance[A]: Bottom[CausalLastWriterWins[A]]           = Bottom.derived
  given dottedDecompose[A]: DottedDecompose[CausalLastWriterWins[A]] = DottedDecompose.derived

  extension [C, A](container: C)
    def causalLastWriterWins: syntax[C, A] = syntax(container)

  implicit class syntax[C, A](container: C)
      extends OpsSyntaxHelper[C, CausalLastWriterWins[A]](container) {

    def read(using PermQuery): Option[A] =
      current.repr.multiVersionRegister.read.reduceOption(DecomposeLattice[TimedVal[A]].merge).map(x => x.payload)

    def write(using PermCausalMutate, PermId)(v: A): C =
      current.repr.inherit.multiVersionRegister.write(LastWriterWins.now(v, replicaID)).anon.map(
        CausalLastWriterWins.apply
      ).mutator

    def map(using PermCausalMutate, PermId)(f: A => A): C =
      read.map(f) match {
        case None    => Dotted(CausalLastWriterWins.empty).mutator
        case Some(v) => write(v)
      }

    def clear(using PermCausalMutate)(): C =
      current.repr.inheritContext.multiVersionRegister.clear().map(
        CausalLastWriterWins.apply
      ).mutator
  }
}
