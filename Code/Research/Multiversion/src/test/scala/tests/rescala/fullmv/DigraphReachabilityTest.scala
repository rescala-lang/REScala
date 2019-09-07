package tests.rescala.fullmv

import org.scalatest.FunSuite
import rescala.fullmv.sgt.reachability.DigraphNodeWithReachability

class DigraphReachabilityTest extends FunSuite {
  case class Disagreement[T](from: T, to: T, closure: Boolean, addEdgeSearchPath: Boolean)
  def getDisagreementsBetweenDigraphReachabilityVsTransitiveClosure[T](edges: Map[T, Set[T]]): Traversable[Disagreement[T]] = {
    val trees: Map[T, DigraphNodeWithReachability] = edges.keySet.map(e => (e, new DigraphNodeWithReachability)).toMap
    for((from, tos) <- edges; to <- tos) trees(from).addSuccessor(trees(to))

    var transitiveClosure = edges
    for(via <- edges.keySet) {
      for (from <- edges.keySet if transitiveClosure(from)(via); to <- transitiveClosure(via)){
        transitiveClosure += from -> (transitiveClosure(from) + to)
      }
      transitiveClosure += via -> (transitiveClosure(via) + via)
    }
    for((from, tos) <- edges; to <- edges.keySet if transitiveClosure(from)(to) != trees(from).isReachable(trees(to)))
      yield Disagreement(from, to, transitiveClosure(from)(to), trees(from).isReachable(trees(to)))
  }

  test("Digraph Reachability is correct for paper graph") {
    // G.F. Italiano 1986. "Amortized Efficiency Of A Path Retrieval Data Structure"
    val testEdges = Map[Int, Set[Int]](
      1 -> Set(2, 4, 5, 7),
      2 -> Set(7, 8, 9, 12),
      3 -> Set(9),
      4 -> Set(3, 7),
      5 -> Set(4, 6),
      6 -> Set(3, 10),
      7 -> Set(9),
      8 -> Set(1, 5, 6, 11),
      9 -> Set(10),
      10 -> Set(),
      11 -> Set(),
      12 -> Set(11)
    )
    assert(getDisagreementsBetweenDigraphReachabilityVsTransitiveClosure(testEdges).isEmpty)
  }
}
