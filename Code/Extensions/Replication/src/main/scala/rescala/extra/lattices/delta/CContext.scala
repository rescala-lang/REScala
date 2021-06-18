package rescala.extra.lattices.delta

import cats.collections._

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
          else l :+ Set(dot)
      }

    override def forall(cc: Set[Dot], cond: Dot => Boolean): Boolean = cc.forall(cond)
  }

  /** DietMapCContext is a causal context implementation that uses the cats-collections Diet (Discrete Interval Encoding Tree)
    * data structure to efficiently store large continuous ranges of dots.
    */
  type DietMapCContext = Map[String, Diet[Int]]
  implicit def DietMapCContext: CContext[Map[String, Diet[Int]]] = new CContext[Map[String, Diet[Int]]] {
    override def contains(cc: Map[String, Diet[Int]], d: Dot): Boolean = d match {
      case Dot(replicaID, counter) =>
        cc.get(replicaID).exists(_.contains(counter))
    }

    private def addDot(cc: Map[String, Diet[Int]], dot: Dot): Map[String, Diet[Int]] = dot match {
      case Dot(replicaID, counter) =>
        val oldDiet = cc.getOrElse(replicaID, Diet.empty[Int])
        cc + (replicaID -> (oldDiet + counter))
    }

    override def empty: Map[String, Diet[Int]] = Map.empty[String, Diet[Int]]

    override def one(dot: Dot): Map[String, Diet[Int]] = Map(dot.replicaID -> Diet.one(dot.counter))

    override def fromSet(dots: Set[Dot]): Map[String, Diet[Int]] =
      dots.foldLeft(Map[String, Diet[Int]]())(addDot)

    override def toSet(cc: Map[String, Diet[Int]]): Set[Dot] =
      for (
        replicaID <- cc.keySet;
        counter   <- cc.getOrElse(replicaID, Diet.empty[Int]).toList
      )
        yield Dot(replicaID, counter)

    override def union(left: Map[String, Diet[Int]], right: Map[String, Diet[Int]]): Map[String, Diet[Int]] = {
      right.foldLeft(left) {
        case (m, (k, r)) =>
          m.updatedWith(k) {
            case Some(l) => Some(l ++ r)
            case None    => Some(r)
          }
      }
    }

    override def diff(cc: Map[String, Diet[Int]], other: Iterable[Dot]): Map[String, Diet[Int]] = other.foldLeft(cc) {
      case (d, Dot(id, c)) =>
        d.updatedWith(id) {
          case None       => None
          case Some(diet) => Some(diet.remove(c))
        }
    }

    override def max(cc: Map[String, Diet[Int]], replicaID: String): Option[Dot] =
      cc.getOrElse(replicaID, Diet.empty[Int]).max.map(Dot(replicaID, _))

    override def decompose(cc: Map[String, Diet[Int]], exclude: Dot => Boolean): Iterable[Map[String, Diet[Int]]] =
      cc.foldLeft(List.empty[Map[String, Diet[Int]]]) {
        case (list, (id, diet)) =>
          list ++ diet.foldLeft(List.empty[Map[String, Diet[Int]]]) {
            case (l, c) =>
              if (exclude(Dot(id, c))) l
              else l :+ Map(id -> Diet.one(c))
          }
      }

    override def forall(cc: Map[String, Diet[Int]], cond: Dot => Boolean): Boolean =
      cc.forall {
        case (id, diet) => diet.foldLeft(true) {
            case (b, c) => b && cond(Dot(id, c))
          }
      }
  }
}
