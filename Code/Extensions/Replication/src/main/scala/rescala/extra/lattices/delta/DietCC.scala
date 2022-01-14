package rescala.extra.lattices.delta

import cats.collections._
import kofre.decompose.{CContext, Dot}

object DietCC {

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
          diet.foldLeft(list) {
            case (l, c) =>
              if (exclude(Dot(id, c))) l
              else Map(id -> Diet.one(c)) :: l
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
