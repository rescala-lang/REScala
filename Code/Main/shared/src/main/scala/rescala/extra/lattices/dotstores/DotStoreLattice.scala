package rescala.extra.lattices.dotstores

import rescala.extra.lattices.IdUtil.Id
import rescala.extra.lattices.Lattice

case class Dot(replicaId: Id, counter: Int)
case class Causal[A](store: A, context: Set[Dot])

case class CausalO[A](store: A, context: Context)

case class Context(internal: Map[Id, IntTree.Range]) {
  def add(replicaId: Id, time: Int): Context = Context(internal.updated(
    replicaId,
    IntTree.insert(internal.getOrElse(replicaId, IntTree.empty), time)
    ))
  def dots(): Set[Dot] = internal.iterator.flatMap { case (id, range) =>
    IntTree.toSeq(range).map(Dot(id, _))
  }.toSet
  def nextTime(replicaId: Id): Int = {
    val range = internal.getOrElse(replicaId, IntTree.empty)
    IntTree.nextValue(range, 0)
  }
  def diff(extern: Context): Context = Context {
    internal.map{ case (id, range) =>
      val filtered = extern.internal.get(id).map{ erange =>
        val keep = IntTree.toSeq(range).filterNot(IntTree.contains(erange, _))
        IntTree.fromIterator(keep.iterator)
      }
        id -> filtered.getOrElse(range)
    }
  }
  def intersect(other: Context): Context = Context {
    internal.iterator.filter{case (id, _ ) => other.internal.contains(id) }.map{ case (id, range) =>
      val otherRange = other.internal(id)
      val res = IntTree.fromIterator(IntTree.toSeq(range).iterator.filter(IntTree.contains(otherRange, _)))
      id -> res
    }.toMap
  }
}

object Context {
  def single(replicaId: Id, time: Int): Context = Context(Map((replicaId, IntTree.insert(IntTree.empty, time))))
  val empty: Context = Context(Map.empty)

  implicit val contextLattice: Lattice[Context] = new Lattice[Context] {
    override def merge(left: Context, right: Context): Context = {
      Context(Lattice.merge(left.internal, right.internal))
    }
  }
}

object IntTree {

  case class Range(from: Int, until: Int, less: Range, more: Range)

  implicit val rangeLattice: Lattice[Range] = new Lattice[Range] {
    override def merge(left: Range, right: Range): Range = IntTree.merge(left, right)
  }

  def fromIterator(iterable: Iterator[Int]): Range = iterable.foldLeft(empty)(IntTree.insert)

  def show(tree: Range): String = {
    if (tree == null) "[]"
    else s"{L${show(tree.less)} I[${tree.from}, ${tree.until-1}] R${show(tree.more)}}"
  }

  val empty: Range = null

  def insert(tree: Range, value: Int): Range = insert(tree, value, value + 1)

  def ranges(tree: Range): List[Range] = {
    if (tree == null) Nil
    else (ranges(tree.less) :+ tree) ++ ranges(tree.more)
  }

  def toSeq(tree: Range): List[Int] = {
    if (tree == null) Nil
    else toSeq(tree.less) ++ (tree.from until tree.until) ++ toSeq(tree.more)
  }

  def merge(left: Range, right: Range): Range = ranges(right).foldLeft(left){ (ir, r) => insert(ir, r.from, r.until)}

  private def overlap(start: Int, middle: Int, end: Int): Boolean = start <= middle && middle <= end

  def nextValue(tree: Range, default: Int): Int = {
    val (_, maxr) = max(tree)
    if (maxr == null) default
    else maxr.until
  }

  def max(tree: Range): (Range, Range) = {
    import tree._
    if (tree == null) (tree, tree)
    else if (more != null) {
      val (t, m) = max(more)
      (copy(more = t), m)
    }
    else (less, tree)
  }

  def min(tree: Range): (Range, Range) = {
    import tree._
    if (tree == null) (tree, tree)
    else if (less != null) {
      val (t, m) = min(less)
      (copy(less = t), m)
    }
    else (more, tree)
  }


  @scala.annotation.tailrec
  private def flatten(tree: Range): Range = {
    import tree._
    if (tree == null) tree
    else {
      val (upd, lesser) = max(less)
      if (lesser != null && from <= lesser.until) flatten(Range(math.min(from, lesser.from), math.max(until, lesser.until), upd, more))
      else {
        val (upd, morere) = min(more)
        if (morere != null && morere.from <= until) flatten(Range(math.min(from, morere.from), math.max(until, morere.until), less, upd))
        else tree
      }
    }
  }

  def insert(tree: Range, iFrom: Int, iUntil: Int): Range = {
    import tree._
    if (tree == null) Range(iFrom, iUntil, null, null)
    else if (overlap(from, iFrom, until)) flatten(Range(from, math.max(iUntil, until), less, more))
    else if (overlap(from, iUntil, until)) flatten(Range(math.min(from, iFrom), until, less, more))
    else if (iUntil < from) Range(from, until, insert(less, iFrom, iUntil), more)
    else if (until < iFrom) Range(from, until, less, insert(more, iFrom, iUntil))
    else throw new IllegalStateException(s"do not understand how ($iFrom, $iUntil) relates to ($from, $until)")
  }

  @scala.annotation.tailrec
  def contains(tree: Range, search: Int): Boolean = {
    if (tree == null) false
    else if (search < tree.from) contains(tree.less, search)
    else if (tree.until <= search) contains(tree.more, search)
    else true
  }
}




/** Dot stores provide a generic way to merge datastructures,
  * implemented on top of one of the provided dot stores.
  * */
trait DotStoreLattice[Store] {
  def add(a: Store, d: Dot): Store

  def dots(a: Store): Set[Dot]

  def compress(a: Store): Store

  def empty: Store

  /** The new element contains all the dots that are either
    * contained in both dotstores or contained in one of the dotstores but not in the causal context (history) of the
    * other one. */
  def merge(left: Causal[Store], right: Causal[Store]): Causal[Store]
}

object DotStoreLattice {
  def next[A: DotStoreLattice](id: Id, c: A): Dot = {
    val dotsWithId = DotStoreLattice[A].dots(c).filter(_.replicaId == id)
    val maxCount = if (dotsWithId.isEmpty) 0 else dotsWithId.map(_.counter).max
    Dot(id, maxCount + 1)
  }

  def merge[A: DotStoreLattice](left: Causal[A], right: Causal[A]): Causal[A] = {
    DotStoreLattice[A].merge(left, right)
  }

  def apply[A](implicit dotStore: DotStoreLattice[A]): dotStore.type = dotStore

  // instances

  implicit def DotSetInstance: DotStoreLattice[Set[Dot]] = new DotStoreLattice[Set[Dot]] {
    type Store = Set[Dot]

    override def add(a: Store, d: Dot): Store = a + d

    override def dots(a: Store): Store = a

    /**
      * Only keeps the highest element of each dot subsequence in the set.
      */
    //TODO: how do we know where subsequences started?
    //TODO: this most likely only works with causal delivery of things, which we may not have
    override def compress(a: Store): Store = a.filter(d => !a.contains(Dot(d.replicaId, d.counter + 1)))

    override def empty: Store = Set.empty

    override def merge(left: Causal[Store], right: Causal[Store]): Causal[Store] = {
      val common = left.store intersect right.store
      val newElements = (left.store diff  right.context) union (right.store diff left.context)
      Causal(common union newElements, left.context union right.context)
    }
  }



  implicit def DotMapInstance[Key, A](implicit dsl: DotStoreLattice[A]): DotStoreLattice[Map[Key, A]] = new DotStoreLattice[Map[Key, A]] {
    type Store = Map[Key, A]

    override def add(a: Store, d: Dot): Store = a.mapValues(v => dsl.add(v, d))

    override def dots(a: Store): Set[Dot] = a.valuesIterator.flatMap(dsl.dots).toSet

    override def compress(a: Store): Store = a.mapValues(dsl.compress)

    override def empty: Store = Map.empty

    override def merge(left: Causal[Store], right: Causal[Store]): Causal[Store] = {

      val empty = DotStoreLattice[A].empty

      // The new store is everything both sides have seen and everything that is new.
      // If something is missing from the store (but in the context) it has been deleted.
      val newStore: Store = (left.store.keySet union right.store.keySet).map{ id =>
        val value = DotStoreLattice[A].merge(Causal(left.store.getOrElse(id, empty), left.context),
                                      Causal(right.store.getOrElse(id, empty), right.context))
        (id, value.store)
      }.filter { _._2 != empty }
        .toMap

      // the merged state has seen everything from both sides
      val newContext = left.context union right.context
      Causal(newStore, newContext)
    }
  }
}
