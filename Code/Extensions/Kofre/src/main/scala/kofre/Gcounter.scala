package kofre

import scala.collection.immutable.HashMap

type ReplicaID = String


given maplattice[K, V: Lattice]: Lattice[Map[K, V]] with
  def merge(left: Map[K, V], right: Map[K, V]): Map[K, V] =
    left.to(HashMap).merged(right.to(HashMap)) {
      case ((id, v1), (_, v2)) => (id, (v1 merge v2))
    }

given Lattice[Int] with
  def merge(left: Int, right: Int): Int = left max right


opaque type Version = Map[ReplicaID, Int]

given Lattice[Version] = maplattice

object Version:
  def zero: Version = HashMap.empty

extension (c: Version)
  def value: Int = c.values.sum
  def inc(id: ReplicaID): Version = HashMap(id -> (c.getOrElse(id, 0) + 1))
  def <=(o: Version) = c.forall((k, v) => v <= o.getOrElse(k, 0))
  def <(o: Version) = c <= o && c.exists((k, v) => v < o.getOrElse(k, 0))


class CounterClass(replicaID: ReplicaID):
  private var current = Version.zero

  def inc(): Unit = current = current merge current.inc(replicaID)
  def value: Int = current.value


given[A: Lattice, B: Lattice]: Lattice[Tuple2[A, B]] with
  def merge(left: (A, B), right: (A, B)): (A, B) =
    (left._1 merge right._1, left._2 merge right._2)

type PosNegCounter = (Version, Version)

extension (c: PosNegCounter)
  def value: Int = c._1.value - c._2.value


@main
def test() =
  summon[Lattice[PosNegCounter]]
  val c = CounterClass("a")
  c.inc()
  c.inc()
  println(c.value)
