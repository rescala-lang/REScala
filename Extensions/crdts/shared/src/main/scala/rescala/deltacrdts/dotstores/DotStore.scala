package rescala.deltacrdts.dotstores

import rescala.lattices.IdUtil.Id

case class Dot(replicaId: Id, counter: Int)

trait DotStore[A] {
  def add(a: A, d: Dot): A

  def dots(a: A): Set[Dot]

  //def merge(left: A, right: A): A // TODO: actually merge should not be part of the DotStore but of the delta CRDT
}

object DotStore {
  implicit class DotStoreOps[A](caller: A) {
    def add(d: Dot)(implicit dotStore: DotStore[A]): A = {
      dotStore.add(caller, d)
    }

    def contains(d: Dot)(implicit dotStore: DotStore[A]): Boolean = {
      dotStore.dots(caller).contains(d)
    }

    def dots(implicit dotStore: DotStore[A]): Set[Dot] = {
      dotStore.dots(caller)
    }
  }

  // instances
  implicit def DotSetInstance: DotStore[Set[Dot]] = new DotStore[Set[Dot]] {
    override def add(a: Set[Dot], d: Dot): Set[Dot] = a + d

    override def dots(a: Set[Dot]): Set[Dot] = a
  }

  implicit def DotMapInstance[A](implicit dotStore: DotStore[A]): DotStore[Map[Id, A]] = new DotStore[Map[Id, A]] {
    override def add(a: Map[Id, A], d: Dot): Map[Id, A] = a.mapValues(_.add(d))

    override def dots(a: Map[Id, A]): Set[Dot] = a.values.flatMap(_.dots).toSet
  }
}
