package kofre.causality

import kofre.IdUtil
import kofre.causality.DotStore.DotSet
import kofre.causality.impl.IntTree
import kofre.causality.{CContext, Dot}



/** CContext is the typeclass trait for causal contexts. Causal contexts are used in causal CRDTs to keep track of all the dots that a
  * replica has witnessed.
  */
trait CContext[A] {
  def contains(cc: A, d: Dot): Boolean

  def fromSet(dots: Set[Dot]): A

  def empty: A

  def one(dot: Dot): A

  def toSet(cc: A): Set[Dot]

  def union(left: A, right: A): A

  def diff(cc: A, other: Iterable[Dot]): A

  def clockOf(cc: A, replicaID: IdUtil.Id): Option[Dot] = max(cc, replicaID)
  def max(cc: A, replicaID: String): Option[Dot]

  def nextDot(cc: A, replicaID: String): Dot = max(cc, replicaID) match {
    case Some(dot) => dot.next
    case None      => Dot(replicaID, 0)
  }

  def convert[B: CContext](cc: A): B = CContext[B].fromSet(toSet(cc))

  def decompose(cc: A, exclude: Dot => Boolean): Iterable[A]

  def forall(cc: A, cond: Dot => Boolean): Boolean
}

object CContext {
  def apply[A](implicit cc: CContext[A]): CContext[A] = cc

  /** SetCContext is a causal context implementation that simply stores all dots in a set. For most applications you should
    * use DietMapCContext instead, as it uses compression to efficiently store large continuous ranges of dots.
    */
  val SetCContext: CContext[Set[Dot]] = new CContext[Set[Dot]] {
    override def contains(cc: Set[Dot], d: Dot): Boolean = cc.contains(d)

    override def fromSet(dots: Set[Dot]): Set[Dot] = dots

    override def empty: Set[Dot] = Set.empty[Dot]

    override def one(dot: Dot): Set[Dot] = Set(dot)

    override def toSet(cc: Set[Dot]): Set[Dot] = cc

    override def union(left: Set[Dot], right: Set[Dot]): Set[Dot] = left union right

    override def diff(cc: Set[Dot], other: Iterable[Dot]): Set[Dot] = cc -- other

    override def max(cc: Set[Dot], replicaID: String): Option[Dot] =
      cc.filter(_.replicaID == replicaID).maxByOption(_.counter)

    override def decompose(cc: Set[Dot], exclude: Dot => Boolean): Iterable[Set[Dot]] =
      cc.foldLeft(List.empty[Set[Dot]]) {
        case (l, dot) =>
          if (exclude(dot)) l
          else Set(dot) :: l
      }

    override def forall(cc: Set[Dot], cond: Dot => Boolean): Boolean = cc.forall(cond)
  }


  /** DietMapCContext is a causal context implementation that uses the cats-collections Diet (Discrete Interval Encoding Tree)
    * data structure to efficiently store large continuous ranges of dots.
    */
  implicit val intTreeCC: CContext[CausalContext] = new CContext[CausalContext] {
    override def contains(cc: CausalContext, d: Dot): Boolean = d match {
      case Dot(replicaID, counter) =>
        cc.internal.get(replicaID).exists(IntTree.contains(_, counter))
    }
    override def fromSet(dots: Set[Dot]): CausalContext = CausalContext(dots.groupBy(_.replicaId).map {
      (key, times) =>
        key -> IntTree.fromIterator(times.iterator.map(_.counter))
    })
    override def empty: CausalContext         = CausalContext.empty
    override def one(dot: Dot): CausalContext = CausalContext.empty.add(dot.replicaId, dot.counter)
    override def toSet(cc: CausalContext): Set[Dot] =
      cc.internal.flatMap((key, tree) => IntTree.iterator(tree).map(time => Dot(key, time))).toSet
    override def union(left: CausalContext, right: CausalContext): CausalContext =
      CausalContext.contextLattice.merge(left, right)
    override def diff(cc: CausalContext, other: Iterable[Dot]): CausalContext = cc.diff(fromSet(other.toSet))
    override def max(cc: CausalContext, replicaID: String): Option[Dot] =
      cc.internal.get(replicaID).map(tree => Dot(replicaID, IntTree.nextValue(tree, Long.MinValue) - 1))
        .filterNot(_.counter == Long.MinValue)
    override def decompose(cc: CausalContext, exclude: Dot => Boolean): Iterable[CausalContext] =
      cc.internal.flatMap { (id, tree) =>
        IntTree.iterator(tree).map(time => one(Dot(id, time)))
      }
    override def forall(cc: CausalContext, cond: Dot => Boolean): Boolean = cc.internal.forall { (id, tree) =>
      IntTree.iterator(tree).forall(time => cond(Dot(id, time)))
    }
  }

}
