package kofre.primitives

import kofre.{IdUtil, Lattice}

import scala.collection.immutable.HashMap

opaque type Version = Map[IdUtil.Id, Int]
object Version {

  given lattice: Lattice[Version] = Lattice.mapLattice(_ max _)

  def zero: Version = HashMap.empty

  def fromMap(m: Map[IdUtil.Id, Int]): Version = m

  extension (c: Version)
    def value: Int                  = c.values.sum
    def inc(id: IdUtil.Id): Version = HashMap(id -> (c.getOrElse(id, 0) + 1))
    def <=(o: Version)              = c.forall((k, v) => v <= o.getOrElse(k, 0))
    def <(o: Version)               = c <= o && c.exists((k, v) => v < o.getOrElse(k, 0))
}
