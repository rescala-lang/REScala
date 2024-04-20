package rdts.datatypes

import rdts.base.{Bottom, Lattice, Orderings}
import rdts.datatypes.contextual.MultiVersionRegister
import rdts.dotted.HasDots
import rdts.syntax.OpsSyntaxHelper
import rdts.time.CausalTime

import scala.math.Ordering.Implicits.infixOrderingOps

/** A LastWriterWins (register) is a common fallback for datatypes that don’t have good merge semantics.
  *
  * Concurrent writes are resolved by a causality preserving clock based on milliseconds, using a random value as a tie breaker.
  * The random values are non-fair, so a specific replica is more likely to win.
  */
case class LastWriterWins[+A](timestamp: CausalTime, payload: A) {
  def read: A = payload

  def value: A = read

  def write[B](v: B): LastWriterWins[B] =
    LastWriterWins(timestamp.advance, v)

  def map[B](using ev: A <:< Option[B])(f: B => B): LastWriterWins[Option[B]] =
    read.map(f) match {
      case None => ev.substituteCo(this)
      case res  => write(res)
    }
}

object LastWriterWins {

  def empty[A: Bottom]: LastWriterWins[A] = LastWriterWins(Bottom[CausalTime].empty, Bottom[A].empty)

  def fallback[A](v: A): LastWriterWins[A] =
    LastWriterWins(rdts.time.CausalTime(Long.MinValue, 0, CausalTime.countedTime()), v)

  def now[A](v: A): LastWriterWins[A] = LastWriterWins(CausalTime.now(), v)

  given hasDots[A]: HasDots[LastWriterWins[A]] = HasDots.noDots

  given lattice[A]: Lattice[LastWriterWins[A]] =
    given Ordering[A] = MultiVersionRegister.assertEqualsOrdering
    Lattice.fromOrdering(using Orderings.lexicographic)

  inline def generalizedLattice[A]: Lattice[LastWriterWins[A]] = scala.compiletime.summonFrom {
    case conflictCase: Lattice[A] => GenericLastWriterWinsLattice(conflictCase)
    case _                        => GenericLastWriterWinsLattice(MultiVersionRegister.assertEqualsLattice)
  }

  class GenericLastWriterWinsLattice[A](conflict: Lattice[A]) extends Lattice[LastWriterWins[A]] {
    override def lteq(left: LastWriterWins[A], right: LastWriterWins[A]): Boolean = left.timestamp <= right.timestamp

    override def decompose(state: LastWriterWins[A]): Iterable[LastWriterWins[A]] = List(state)

    override def merge(left: LastWriterWins[A], right: LastWriterWins[A]): LastWriterWins[A] =
      CausalTime.ordering.compare(left.timestamp, right.timestamp) match
        case 0 =>
          val newPayload = conflict.merge(left.payload, right.payload)
          LastWriterWins(left.timestamp, newPayload)
        case x if x < 0 => right
        case x if x > 0 => left
  }

  given bottom[A: Bottom]: Bottom[LastWriterWins[A]] with
    override def empty: LastWriterWins[A] = LastWriterWins.this.empty

}
