package kofre.datatypes

import kofre.base.{Bottom, Lattice, Time}
import kofre.datatypes.LastWriterWins.CausalTime
import kofre.datatypes.alternatives.lww.{ILastWriterWins}
import kofre.syntax.OpsSyntaxHelper

import scala.math.Ordering.Implicits.infixOrderingOps

/** A LastWriterWins (register) is a common fallback for datatypes that don’t have good merge semantics.
  *
  * Concurrent writes are resolved by wallclock time first (favouring replicas with clock drift into the future),
  * and by replica ID, in case both writes happened in the same millisecond.
  */
case class LastWriterWins[+A](timestamp: CausalTime, payload: A) extends ILastWriterWins[CausalTime, A]

object LastWriterWins {

  case class CausalTime(time: Time, causal: Long, random: Long):
    def inc: CausalTime = CausalTime(time, causal + 1, System.nanoTime())

  object CausalTime:
    given ordering: Ordering[CausalTime] =
      Ordering.by[CausalTime, Long](_.time)
        .orElseBy(_.causal)
        .orElseBy(_.random)
    def now() = CausalTime(Time.current(), 0, System.nanoTime())

  def empty[A: Bottom]: LastWriterWins[A] = now(Bottom.empty)

  def fallback[A](v: A): LastWriterWins[A] =
    LastWriterWins(CausalTime(Long.MinValue, 0, System.nanoTime()), v)

  def now[A](v: A): LastWriterWins[A] = LastWriterWins(CausalTime(Time.current(), 0, System.nanoTime()), v)

  given lattice[A]: Lattice[LastWriterWins[A]] = ILastWriterWins.lattice
  given bottom[A: Bottom]: Bottom[LastWriterWins[A]] with
    override def empty: LastWriterWins[A] = LastWriterWins.this.empty

  extension [C, A](container: C)
    def causalLastWriterWins: syntax[C, A] = syntax(container)

  implicit class syntax[C, A](container: C)
      extends OpsSyntaxHelper[C, LastWriterWins[A]](container) {

    def read(using PermQuery): A = current.payload

    def write(v: A): Mutate =
      val now = CausalTime.now()
      if now <= current.timestamp
      then LastWriterWins(current.timestamp.inc, v).mutator
      else LastWriterWins(now, v).mutator

    def map[B](using PermMutate)(using ev: A =:= Option[B])(f: B => B): C =
      read.map(f) match {
        case None => container
        case res  => write(ev.flip(res))
      }
  }
}
