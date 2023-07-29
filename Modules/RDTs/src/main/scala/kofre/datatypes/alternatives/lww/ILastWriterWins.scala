package kofre.datatypes.alternatives.lww

import kofre.base.Lattice

import scala.math.Ordering.Implicits.infixOrderingOps

trait ILastWriterWins[Time, +Value] {
  def timestamp: Time
  def payload: Value
}

object ILastWriterWins {

  given lattice[Time: Ordering, A, LWW <: ILastWriterWins[Time, A]]: Lattice[LWW] with {
    override def lteq(left: LWW, right: LWW): Boolean = left.timestamp <= right.timestamp

    override def decompose(state: LWW): Iterable[LWW] = List(state)

    override def merge(left: LWW, right: LWW): LWW =
      Ordering[Time].compare(left.timestamp, right.timestamp) match
        case 0 =>
          assert(left.payload == right.payload, s"LWW same timestamp, different value: »$left«, »$right«")
          left
        case x if x < 0 => right
        case x if x > 0 => left
  }
}
