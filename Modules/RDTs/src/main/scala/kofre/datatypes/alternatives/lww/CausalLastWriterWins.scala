package kofre.datatypes.alternatives.lww

import kofre.base.{Bottom, Lattice}
import kofre.datatypes.MultiVersionRegister
import kofre.datatypes.alternatives.lww.TimedVal
import kofre.dotted.{DotFun, Dotted, DottedLattice}
import kofre.syntax.{OpsSyntaxHelper, ReplicaId}

/** An LWW (Last Writer Wins) is a Delta CRDT modeling a register.
  *
  * If two concurrent write operations occur, the resulting LWW takes on the value of the write operation with the later timestamp.
  */
case class CausalLastWriterWins[A](repr: MultiVersionRegister[TimedVal[A]])

object CausalLastWriterWins {
  def empty[A]: CausalLastWriterWins[A] = CausalLastWriterWins(MultiVersionRegister.empty)

  given bottomInstance[A]: Bottom[CausalLastWriterWins[A]]       = Bottom.derived
  given dottedLattice[A]: DottedLattice[CausalLastWriterWins[A]] = DottedLattice.derived

  extension [C, A](container: C)
    def causalLastWriterWins: syntax[C, A] = syntax(container)

  implicit class syntax[C, A](container: C)
      extends OpsSyntaxHelper[C, CausalLastWriterWins[A]](container) {

    def read(using PermQuery): Option[A] =
      current.repr.multiVersionRegister.read.reduceOption(Lattice[TimedVal[A]].merge).map(x => x.payload)

    def write(using ReplicaId)(v: A): CausalMutate =
      current.repr.inheritContext.multiVersionRegister.write(TimedVal.now(v, replicaId)).map(
        CausalLastWriterWins.apply
      ).mutator

    def map(using ReplicaId, PermCausalMutate)(f: A => A): C =
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
