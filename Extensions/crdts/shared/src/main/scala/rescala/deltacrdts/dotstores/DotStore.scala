package rescala.deltacrdts.dotstores

import rescala.lattices.IdUtil.Id

case class Dot(replicaId: Id, counter: Int)

trait DotStore[A] {
  def add(a: A, d: Dot): A

  def dots(a: A): Set[Dot]

  def compress(a: A): A

  def empty: A

  /**
    * Merges two dotstores with respect to their causal contexts. The new element contains all the dots that are either
    * contained in both dotstores or contained in one of the dotstores but not in the causal context (history) of the
    * other one.
    *
    * @return
    */
  def merge(left: A, leftContext: Set[Dot], right: A, rightContext: Set[Dot]): (A, Set[Dot])
}

object DotStore {
  def next[A](id: Id, c: A)(implicit dotStore: DotStore[A]): Dot = {
    val dotsWithId = c.dots.filter(_.replicaId == id)
    val maxCount = if (dotsWithId.isEmpty) 0 else dotsWithId.map(_.counter).max
    Dot(id, maxCount + 1)
  }

  def merge[A](m1: A, c1: Set[Dot], m2: A, c2: Set[Dot])(implicit dotStore: DotStore[A]): (A, Set[Dot]) = {
    dotStore.merge(m1, c1, m2, c2)
  }

  def empty[A](implicit dotStore: DotStore[A]): A = dotStore.empty

  implicit class DotStoreOps[A](caller: A) {
    /**
      * adds a dot to the dotstore and compresses it afterwards
      */
    def add(d: Dot)(implicit dotStore: DotStore[A]): A = {
      dotStore.compress(dotStore.add(caller, d)) // TODO: split add and addAndCompress methods to allow adding without compression
    }

    def addMultiple(c: Iterable[Dot])(implicit dotStore: DotStore[A]): A = {
      dotStore.compress(c.foldLeft(caller: A) { (akk: A, dot: Dot) => dotStore.add(akk, dot)})
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

    /**
      * Only keeps the highest element of each dot subsequence in the set.
      */
    override def compress(a: Set[Dot]): Set[Dot] = a.filter(d => !a.contains(Dot(d.replicaId, d.counter + 1)))

    override def empty: Set[Dot] = Set[Dot]()

    override def merge(left: Set[Dot], leftContext: Set[Dot], right: Set[Dot], rightContext: Set[Dot]): (Set[Dot], Set[Dot]) = {
      val common = left intersect right
      val newElements = (left -- rightContext) union (right -- leftContext)
      (common union newElements, leftContext union rightContext)
    }
  }

  implicit def DotMapInstance[A](implicit dotStore: DotStore[A]): DotStore[Map[Id, A]] = new DotStore[Map[Id, A]] {
    override def add(a: Map[Id, A], d: Dot): Map[Id, A] = a.mapValues(_.add(d))

    override def dots(a: Map[Id, A]): Set[Dot] = a.values.flatMap(_.dots).toSet

    override def compress(a: Map[Id, A]): Map[Id, A] = a.mapValues(dotStore.compress)

    override def empty: Map[Id, A] = Map[Id, A]()

    override def merge(left: Map[Id, A], leftContext: Set[Dot], right: Map[Id, A], rightContext: Set[Dot]): (Map[Id, A], Set[Dot]) = {
      val newStore: Map[Id, A] = (left.keySet union right.keySet).map(id => {
        // those are needed to allow merging even if the element is only in one of the DotMaps
        val leftn: Map[Id, A] = left.withDefaultValue(DotStore.empty[A])
        val rightn: Map[Id, A] = right.withDefaultValue(DotStore.empty[A])

        // merge the dotstores for each id
        val (value, _) = DotStore.merge(leftn(id), leftContext, rightn(id), rightContext)
        (id, value)
      }).filter { case (_, value) => value != DotStore.empty[A] } // filter out empty elements
        .toMap // return a new map
      val newContext = leftContext union rightContext // simply take the union of both contexts
      (newStore, newContext)
    }
  }
}
