package kofre.causality

import kofre.IdUtil
import kofre.dotbased.DotStore.DotSet
import kofre.causality.impl.IntTree
import kofre.causality.{CContext, Dot}

/** CContext is the typeclass trait for causal contexts. Causal contexts are used in causal CRDTs to keep track of all the dots that a
  * replica has witnessed.
  * TODO: modeling this as a typeclass is somewhat overengineered, but changing the API currently would be too much effort
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

  /** DietMapCContext is a causal context implementation that uses the cats-collections Diet (Discrete Interval Encoding Tree)
    * data structure to efficiently store large continuous ranges of dots.
    */
  implicit val intTreeCC: CContext[CausalContext] = new CContext[CausalContext] {
    override def contains(cc: CausalContext, d: Dot): Boolean                    = cc.contains(d)
    override def fromSet(dots: Set[Dot]): CausalContext                          = CausalContext.fromSet(dots)
    override def empty: CausalContext                                            = CausalContext.empty
    override def one(dot: Dot): CausalContext                                    = CausalContext.one(dot)
    override def toSet(cc: CausalContext): Set[Dot]                              = cc.toSet
    override def union(left: CausalContext, right: CausalContext): CausalContext = left.union(right)
    override def diff(cc: CausalContext, other: Iterable[Dot]): CausalContext    = cc.diff(fromSet(other.toSet))
    override def max(cc: CausalContext, replicaID: String): Option[Dot]          = cc.max(replicaID)
    override def decompose(cc: CausalContext, exclude: Dot => Boolean): Iterable[CausalContext] = cc.decompose(exclude)
    override def forall(cc: CausalContext, cond: Dot => Boolean): Boolean                       = cc.forall(cond)
  }

}
