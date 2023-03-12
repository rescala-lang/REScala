package kofre.datatypes

import kofre.base.{Bottom, Lattice, Time, Uid}
import kofre.datatypes.MultiVersionRegister
import kofre.dotted.{DotFun, Dotted, DottedLattice, HasDots}
import kofre.syntax.{OpsSyntaxHelper, ReplicaId}
import kofre.time.{Dot, Dots}

import scala.math.Ordering.Implicits.infixOrderingOps

/** A LastWriterWins (register) is a common fallback for datatypes that don’t have good merge semantics.
  *
  * This implementation uses [[Dot]]s to have “last” coincide with causal order (independent of wall clock time).
  * Concurrent writes are resolved by wallclock time first (favouring replicas with clock drift into the future),
  * and by replica ID, in case both writes happened in the same millisecond.
  */
case class LastWriterWins[+A](dot: Dot, wallTime: Time, payload: A)

object LastWriterWins {

  def empty[A: Bottom](dot: Dot): LastWriterWins[A] = now(dot, Bottom.empty)

  def fallback[A](dot: Dot, v: A): LastWriterWins[A] = LastWriterWins(dot, Long.MinValue, v)

  def now[A](dot: Dot, v: A): LastWriterWins[A] = LastWriterWins(dot, Time.current(), v)

  given HasDots[LastWriterWins[Any]] = a => Dots.single(a.dot)

  given ordering: Ordering[LastWriterWins[Any]] =
    Ordering.by[LastWriterWins[Any], Time](_.wallTime)
      .orElseBy(_.dot.replicaId)
      .orElseBy(_.dot.time)
      .orElse(MultiVersionRegister.assertEqualsOrdering.on(_.payload))

  given dottedLattice[A]: DottedLattice[LastWriterWins[A]] with {
    override def mergePartial(left: Dotted[LastWriterWins[A]], right: Dotted[LastWriterWins[A]]): LastWriterWins[A] =
      if left.context.contains(right.store.dot)
      then left.store
      else if right.context.contains(left.store.dot)
      then right.store
      else if ordering.lteq(left.store, right.store)
      then right.store
      else left.store

    override def filter(value: LastWriterWins[A], dots: Dots): Option[LastWriterWins[A]] = None
  }

  given optionalLwwLattice[A]: DottedLattice[Option[LastWriterWins[A]]] with {
    override def mergePartial(
        left: Dotted[Option[LastWriterWins[A]]],
        right: Dotted[Option[LastWriterWins[A]]]
    ): Option[LastWriterWins[A]] =
      lazy val empty: LastWriterWins[A] = LastWriterWins.fallback[A](Dot(Uid.gen(), 0), null.asInstanceOf)
      val res = dottedLattice.mergePartial(
        left.map(_.getOrElse(empty)),
        right.map(_.getOrElse(empty))
      )
      if res == empty then None else Some(res)

    override def filter(value: Option[LastWriterWins[A]], dots: Dots): Option[Option[LastWriterWins[A]]] =
      value.map { v =>
        DottedLattice.apply.filter(v, dots)
      }

    override def lteq(left: Dotted[Option[LastWriterWins[A]]], right: Dotted[Option[LastWriterWins[A]]]): Boolean =
      (left.context <= right.context) &&
      ((left.store, right.store) match
        case (None, _)          => true
        case (_, None)          => false
        case (Some(l), Some(r)) => left.map(_ => l) <= right.map(_ => r)
      )
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
