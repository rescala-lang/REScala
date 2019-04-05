package rescala.deltacrdts.dotstores

import rescala.lattices.IdUtil.Id

case class Dot(replicaId: Id, counter: Int)

trait DotStore[A] {
  def add(a: A, d: Dot): A

  def contains(a: A, d: Dot): Boolean

  def dots(a: A): Set[Dot]

  def merge(left: A, right: A): A
}

object DotStore {
  // syntax
  def merge[A](left: A, right: A)(implicit s: DotStore[A]): A =
    s.merge(left, right)

  implicit class DotStoreOps[A](caller: A) {
    def add(d: Dot)(implicit dotStore: DotStore[A]): A = {
      dotStore.add(caller, d)
    }

    def contains(d: Dot)(implicit dotStore: DotStore[A]): Boolean = {
      dotStore.contains(caller, d)
    }

    def dots(implicit dotStore: DotStore[A]): Set[Dot] = {
      dotStore.dots(caller)
    }

    def merge(right: A)(implicit dotStore: DotStore[A]): A = {
      dotStore.merge(caller, right)
    }
  }

  // instances
  implicit def DotSetInstance: DotStore[Set[Dot]] = new DotStore[Set[Dot]] {
    override def dots(a: Set[Dot]): Set[Dot] = a

    override def contains(a: Set[Dot], d: Dot): Boolean = a.contains(d)

    override def add(a: Set[Dot], d: Dot): Set[Dot] = a + d

    override def merge(left: Set[Dot], right: Set[Dot]): Set[Dot] = left.union(right)
  }

  implicit def DotMapInstance[A](implicit dotStore: DotStore[A]): DotStore[Map[Id, A]] = new DotStore[Map[Id, A]] {
    override def add(a: Map[Id, A], d: Dot): Map[Id, A] = a.mapValues(_.add(d))

    override def contains(a: Map[Id, A], d: Dot): Boolean = a.dots.contains(d)

    override def dots(a: Map[Id, A]): Set[Dot] = a.values.flatMap(_.dots).toSet

    override def merge(left: Map[Id, A], right: Map[Id, A]): Map[Id, A] = ???
  }
}
