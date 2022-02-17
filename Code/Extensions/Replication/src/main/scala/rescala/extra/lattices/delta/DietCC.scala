package rescala.extra.lattices.delta

import cats.collections._
import kofre.decompose.{CContext}
import kofre.causality.Dot

object DietCC {

  /** DietMapCContext is a causal context implementation that uses the cats-collections Diet (Discrete Interval Encoding Tree)
    * data structure to efficiently store large continuous ranges of dots.
    */
  type DietMapCContext = Map[String, Diet[Long]]
  implicit def DietMapCContext: CContext[Map[String, Diet[Long]]] = new CContext[Map[String, Diet[Long]]] {
    override def contains(cc: Map[String, Diet[Long]], d: Dot): Boolean = d match {
      case Dot(replicaID, counter) =>
        cc.get(replicaID).exists(_.contains(counter))
    }

    private def addDot(cc: Map[String, Diet[Long]], dot: Dot): Map[String, Diet[Long]] = dot match {
      case Dot(replicaID, counter) =>
        val oldDiet = cc.getOrElse(replicaID, Diet.empty[Long])
        cc + (replicaID -> (oldDiet + counter))
    }

    override def empty: Map[String, Diet[Long]] = Map.empty[String, Diet[Long]]

    override def one(dot: Dot): Map[String, Diet[Long]] = Map(dot.replicaID -> Diet.one(dot.counter))

    override def fromSet(dots: Set[Dot]): Map[String, Diet[Long]] =
      dots.foldLeft(Map[String, Diet[Long]]())(addDot)

    override def toSet(cc: Map[String, Diet[Long]]): Set[Dot] =
      for (
        replicaID <- cc.keySet;
        counter   <- cc.getOrElse(replicaID, Diet.empty[Long]).toList
      )
        yield Dot(replicaID, counter)

    override def union(left: Map[String, Diet[Long]], right: Map[String, Diet[Long]]): Map[String, Diet[Long]] = {
      right.foldLeft(left) {
        case (m, (k, r)) =>
          m.updatedWith(k) {
            case Some(l) => Some(l ++ r)
            case None    => Some(r)
          }
      }
    }

    override def diff(cc: Map[String, Diet[Long]], other: Iterable[Dot]): Map[String, Diet[Long]] = other.foldLeft(cc) {
      case (d, Dot(id, c)) =>
        d.updatedWith(id) {
          case None       => None
          case Some(diet) => Some(diet.remove(c))
        }
    }

    override def max(cc: Map[String, Diet[Long]], replicaID: String): Option[Dot] =
      cc.getOrElse(replicaID, Diet.empty[Long]).max.map(Dot(replicaID, _))

    override def decompose(cc: Map[String, Diet[Long]], exclude: Dot => Boolean): Iterable[Map[String, Diet[Long]]] =
      cc.foldLeft(List.empty[Map[String, Diet[Long]]]) {
        case (list, (id, diet)) =>
          diet.foldLeft(list) {
            case (l, c) =>
              if (exclude(Dot(id, c))) l
              else Map(id -> Diet.one(c)) :: l
          }
      }

    override def forall(cc: Map[String, Diet[Long]], cond: Dot => Boolean): Boolean =
      cc.forall {
        case (id, diet) => diet.foldLeft(true) {
            case (b, c) => b && cond(Dot(id, c))
          }
      }
  }
}
