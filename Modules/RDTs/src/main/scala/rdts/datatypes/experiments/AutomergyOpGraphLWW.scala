package rdts.datatypes.experiments

import rdts.base.{Bottom, Lattice}
import rdts.time.CausalTime

/* Experimental implementation of an LWW register using a graph of operations, inspired by a paper from Leo Stewen and Martin Kleppmann from the Technical University of Munich submitted to the PLF workshop.
 * This is presumably a simplified variant of how automerge works (full automerge has more operations to build a graph instead of just a LWW register).  */
object AutomergyOpGraphLWW {

  type Id = CausalTime

  enum Op[+T]:
    case set(value: T)
    case del
    case undo(anchor: Id)

  case class Entry[T](op: Op[T], predecessors: Set[Id])

  case class OpGraph[T](elements: Map[Id, Entry[T]]) {
    lazy val predecessors: Set[Id] = elements.values.flatMap(_.predecessors).toSet
    lazy val heads: Map[Id, Entry[T]] =
      elements.filter((k, _) => !predecessors.contains(k))
    lazy val latest: Option[Id] = elements.keysIterator.reduceOption(Lattice.merge[CausalTime])

    def values: List[T] =
      def getTerminals(cur: Map[Id, Entry[T]]): List[T] =
        cur.toList.sortBy(_._1)(using CausalTime.ordering.reverse).map(_._2.op).flatMap:
          case Op.set(v) => List(v)
          case Op.del    => Nil
          case Op.undo(anchor) => elements.get(anchor).toList.flatMap: pred =>
              getTerminals(elements.filter((k, _) => pred.predecessors.contains(k)))

      getTerminals(heads)

    private def applyOp(op: Op[T]) =
      OpGraph(
        Map(
          latest.fold(CausalTime.now())(_.advance)
          -> Entry(op, heads.map(_._1).toSet)
        )
      )

    def set(value: T) = applyOp(Op.set(value))

    def del() = applyOp(Op.del)

    def undo(anchor: Id) = applyOp(Op.undo(anchor))
  }

  object OpGraph {
    given lattice[T]: Lattice[OpGraph[T]] =
      given Lattice[Entry[T]] = Lattice.assertEquals
      Lattice.derived
    given bottom[T]: Bottom[OpGraph[T]] = Bottom.derived
  }
}
