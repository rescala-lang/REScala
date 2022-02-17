package kofre.decompose

import kofre.causality.Dot

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
  type SetCContext = Set[Dot]
  implicit def SetCContext: CContext[Set[Dot]] = new CContext[Set[Dot]] {
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
}
