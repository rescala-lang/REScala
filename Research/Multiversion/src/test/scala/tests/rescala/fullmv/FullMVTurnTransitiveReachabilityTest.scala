package tests.rescala.fullmv

import org.scalatest.FunSuite
import rescala.fullmv.{FullMVTurn, TurnPhase}

class FullMVTurnTransitiveReachabilityTest extends FunSuite {
  case class Disagreement[T](from: T, to: T, closure: Boolean, addEdgeSearchPath: Boolean)
  def getDisagreementsBetweenDigraphReachabilityVsTransitiveClosure[T](edges: Map[T, Set[T]]): Traversable[Disagreement[T]] = {
    val trees: Map[T, FullMVTurn] = edges.keySet.map(e => (e, new FullMVTurn(null))).toMap

    // put all transactions under a common locked lock, so that all locking assertions hold
    trees.values.foreach(_.awaitAndSwitchPhase(TurnPhase.Executing))
    trees.values.map(_.lock).reduce{ (lockA, lockB) =>
      val resA = lockA.tryLock()
      assert(resA.success)
      val resB = lockB.trySubsume(resA)
      assert(resB.isEmpty)
      resA.newParent.unlock()
      resA.newParent
    }
    assert(trees.head._2.lock.tryLock().success)

    for((from, tos) <- edges; to <- tos) {
      val fromTree = trees(from)
      val toTree = trees(to)
      if(!fromTree.isTransitivePredecessor(toTree))
        fromTree.addPredecessor(trees(to))
    }

    var transitiveClosure = edges
    for(via <- edges.keySet) {
      for (from <- edges.keySet if transitiveClosure(from)(via); to <- transitiveClosure(via)){
        transitiveClosure += from -> (transitiveClosure(from) + to)
      }
      transitiveClosure += via -> (transitiveClosure(via) + via)
    }
    for((from, tos) <- edges; to <- edges.keySet if transitiveClosure(from)(to) != trees(from).isTransitivePredecessor(trees(to)))
      yield Disagreement(from, to, transitiveClosure(from)(to), trees(from).isTransitivePredecessor(trees(to)))
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
