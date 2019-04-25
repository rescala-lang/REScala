package rescala.deltacrdts.dotstores

import rescala.lattices.IdUtil.Id

case class Dot(replicaId: Id, counter: Int)
case class Causal[A](store: A, context: Set[Dot])

trait DotStore[A] {
  type Store = A
  def add(a: Store, d: Dot): Store

  def dots(a: Store): Set[Dot]

  def compress(a: Store): Store

  def empty: Store

  /**
    * Merges two dotstores with respect to their causal contexts. The new element contains all the dots that are either
    * contained in both dotstores or contained in one of the dotstores but not in the causal context (history) of the
    * other one.
    *
    * @return
    */
  def merge(left: Causal[A], right: Causal[A]): Causal[A]
}

object DotStore {
  def next[A: DotStore](id: Id, c: A): Dot = {
    val dotsWithId = c.dots.filter(_.replicaId == id)
    val maxCount = if (dotsWithId.isEmpty) 0 else dotsWithId.map(_.counter).max
    Dot(id, maxCount + 1)
  }

  def merge[A: DotStore](left: Causal[A], right: Causal[A]): Causal[A] = {
    DotStore[A].merge(left, right)
  }

  def apply[A](implicit dotStore: DotStore[A]): dotStore.type = dotStore

  def empty[A: DotStore]: A = DotStore[A].empty

  implicit class DotStoreOps[A](val caller: A) extends AnyVal {
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
    override def add(a: Store, d: Dot): Store = a + d

    override def dots(a: Store): Store = a

    /**
      * Only keeps the highest element of each dot subsequence in the set.
      */
    //TODO: how do we know where subsequences started?
    override def compress(a: Store): Store = a.filter(d => !a.contains(Dot(d.replicaId, d.counter + 1)))

    override def empty: Store = Set.empty

    override def merge(left: Causal[Store], right: Causal[Store]): Causal[Store] = {
      val common = left.store intersect right.store
      val newElements = (left.store diff  right.context) union (right.store diff left.context)
      Causal(common union newElements, left.context union right.context)
    }
  }

  implicit def DotMapInstance[A](implicit dotStore: DotStore[A]): DotStore[Map[Id, A]] = new DotStore[Map[Id, A]] {
    override def add(a: Store, d: Dot): Store = a.mapValues(_.add(d))

    override def dots(a: Store): Set[Dot] = a.values.flatMap(_.dots).toSet

    override def compress(a: Store): Store = a.mapValues(dotStore.compress)

    override def empty: Store = Map.empty

    override def merge(left: Causal[Store], right: Causal[Store]): Causal[Store] = {
      val newStore: Store = (left.store.keySet union right.store.keySet).map(id => {
        // those are needed to allow merging even if the element is only in one of the DotMaps
        val leftn: Store = left.store.withDefaultValue(DotStore.empty[A])
        val rightn: Store = right.store.withDefaultValue(DotStore.empty[A])

        // merge the dotstores for each id
        val value = DotStore[A].merge(Causal(leftn(id), left.context), Causal(rightn(id), right.context))
        (id, value.store)
      }).filter { case (_, value) => value != DotStore.empty[A] } // filter out empty elements
        .toMap // return a new map
      val newContext = left.context union right.context // simply take the union of both contexts
      Causal(newStore, newContext)
    }
  }
}
