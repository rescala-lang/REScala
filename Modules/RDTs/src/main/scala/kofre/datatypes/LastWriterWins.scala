package kofre.datatypes

import kofre.base.{Bottom, Lattice, Time, Uid}
import kofre.datatypes.MultiVersionRegister
import kofre.dotted.{DotFun, Dotted, DottedLattice}
import kofre.syntax.{OpsSyntaxHelper, ReplicaId}
import kofre.time.{Dot, Dots}

import math.Ordering.Implicits.infixOrderingOps

/** A LastWriterWins (register) is a common fallback for datatypes that don’t have good merge semantics. */
case class LastWriterWins[A](dot: Dot, wallTime: Time, payload: A)

object LastWriterWins {

  def empty[A: Bottom](dot: Dot): LastWriterWins[A] = now(dot, Bottom.empty)

  def fallback[A: Bottom](dot: Dot, v: A): LastWriterWins[A] = LastWriterWins(dot, 0, v)

  def now[A](dot: Dot, v: A): LastWriterWins[A] = LastWriterWins(dot, Time.current(), v)

  given dottedLattice[A]: DottedLattice[LastWriterWins[A]] with {
    override def mergePartial(left: Dotted[LastWriterWins[A]], right: Dotted[LastWriterWins[A]]): LastWriterWins[A] = {
      if left.context.contains(right.store.dot)
      then left.store
      else if right.context.contains(left.store.dot)
      then right.store
      else
        java.lang.Long.compare(left.store.wallTime, right.store.wallTime) match
          case -1 => right.store
          case 1  => left.store
          case 0 =>
            if left.store.dot.replicaId <= right.store.dot.replicaId
            then right.store
            else left.store
    }

  }

  extension [C, A](container: C)
    def causalLastWriterWins: syntax[C, A] = syntax(container)

  implicit class syntax[C, A](container: C)
      extends OpsSyntaxHelper[C, LastWriterWins[A]](container) {

    def read(using PermQuery): A = current.payload

    def write(using ReplicaId)(v: A): CausalMutate =
      val nextDot = context.nextDot(replicaId)
      Dotted(
        LastWriterWins.now(nextDot, v),
        Dots.single(current.dot).add(nextDot)
      ).mutator

    def map[B](using ReplicaId, PermCausalMutate)(using ev: A =:= Option[B])(f: B => B): C =
      read.map(f) match {
        case None => container
        case res  => write(ev.flip(res))
      }
  }
}
