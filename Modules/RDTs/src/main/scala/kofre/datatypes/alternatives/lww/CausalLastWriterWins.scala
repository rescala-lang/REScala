package kofre.datatypes.alternatives.lww

import kofre.base.{Bottom, Lattice, Time}
import kofre.syntax.OpsSyntaxHelper

case class CausalTime(time: Time, causal: Long, random: Long):
  def inc: CausalTime = CausalTime(time, causal + 1, System.nanoTime())
object CausalTime:
  given ordering: Ordering[CausalTime] =
    Ordering.by[CausalTime, Long](_.time)
      .orElseBy(_.causal)
      .orElseBy(_.random)

/** A LastWriterWins (register) is a common fallback for datatypes that don’t have good merge semantics.
  *
  * Concurrent writes are resolved by wallclock time first (favouring replicas with clock drift into the future),
  * and by replica ID, in case both writes happened in the same millisecond.
  */
case class CausalLastWriterWins[+A](timestamp: CausalTime, payload: A) extends ILastWriterWins[CausalTime, A]

object CausalLastWriterWins {

  def empty[A: Bottom]: CausalLastWriterWins[A] = now(Bottom.empty)

  def fallback[A](v: A): CausalLastWriterWins[A] = CausalLastWriterWins(CausalTime(Long.MinValue, 0, System.nanoTime()), v)

  def now[A](v: A): CausalLastWriterWins[A] = CausalLastWriterWins(CausalTime(Time.current(), 0, System.nanoTime()), v)

  given lattice[A]: Lattice[CausalLastWriterWins[A]] = ILastWriterWins.lattice
  given bottom[A: Bottom]: Bottom[CausalLastWriterWins[A]] with
    override def empty: CausalLastWriterWins[A] = CausalLastWriterWins.this.empty


  extension [C, A](container: C)
    def causalLastWriterWins: syntax[C, A] = syntax(container)

  implicit class syntax[C, A](container: C)
      extends OpsSyntaxHelper[C, CausalLastWriterWins[A]](container) {

    def read(using PermQuery): A = current.payload

    def write(v: A): Mutate =
      val now = Time.current()
      if now <= current.timestamp.time
      then CausalLastWriterWins(current.timestamp.inc, v).mutator
      else CausalLastWriterWins(CausalTime(now, 0, System.nanoTime()), v).mutator

    def map[B](using PermMutate)(using ev: A =:= Option[B])(f: B => B): C =
      read.map(f) match {
        case None => container
        case res  => write(ev.flip(res))
      }
  }
}
