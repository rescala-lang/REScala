package kofre.datatypes

import kofre.base.{Bottom, Lattice, Time, Uid}
import kofre.datatypes.MultiVersionRegister
import kofre.dotted.{DotFun, Dotted, DottedLattice}
import kofre.syntax.{OpsSyntaxHelper, ReplicaId}
import math.Ordering.Implicits.infixOrderingOps

/** An LWW (Last Writer Wins) is a Delta CRDT modeling a register.
  *
  * If two concurrent write operations occur, the resulting LWW takes on the value of the write operation with the later timestamp.
  */
case class LastWriterWins[A](repr: MultiVersionRegister[LastWriterWins.WallTimed[A]])

object LastWriterWins {

  case class WallTimed[A](time: Time, payload: A)

  def empty[A]: LastWriterWins[A] = LastWriterWins(MultiVersionRegister.empty)

  given bottomInstance[A]: Bottom[LastWriterWins[A]]       = Bottom.derived
  given dottedLattice[A]: DottedLattice[LastWriterWins[A]] = DottedLattice.derived

  extension [C, A](container: C)
    def causalLastWriterWins: syntax[C, A] = syntax(container)

  implicit class syntax[C, A](container: C)
      extends OpsSyntaxHelper[C, LastWriterWins[A]](container) {

    def read(using PermQuery): Option[A] =
      current.repr.repr.repr.reduceOption{ case (l@(ld, lt), r@(rd, rt)) =>
        java.lang.Long.compareUnsigned(lt.time, rt.time) match
          case -1 => r
          case 1 => l
          case 0 => if ld.replicaId <= rd.replicaId
          then r else l
      }.map(_._2.payload)

    def write(using ReplicaId)(v: A): CausalMutate =

      val mvr = MultiVersionRegister.syntax[Dotted[MultiVersionRegister[WallTimed[A]]], WallTimed[A]](current.repr.inheritContext)

      mvr.write(WallTimed(Time.current(), v)).map(
        LastWriterWins.apply
      ).mutator

    def map(using ReplicaId, PermCausalMutate)(f: A => A): C =
      read.map(f) match {
        case None    => Dotted(LastWriterWins.empty).mutator
        case Some(v) => write(v)
      }

    def clear(using PermCausalMutate)(): C =
      current.repr.inheritContext.multiVersionRegister.clear().map(
        LastWriterWins.apply
      ).mutator
  }
}
