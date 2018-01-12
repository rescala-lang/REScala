package tests.rescala.fullmv

import org.scalatest.FunSuite
import rescala.fullmv.{FullMVTurnImpl, SerializationGraphTracking, TurnPhase}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import rescala.fullmv.FullMVEngine.default._
import rescala.fullmv.sgt.synchronization.Successful

class FullMVTurnTransitiveReachabilityTest extends FunSuite {
  case class Disagreement[T](from: T, to: T, closure: Boolean, addEdgeSearchPath: Boolean)
  def getDisagreementsBetweenDigraphReachabilityVsTransitiveClosure[T](edges: Map[T, Set[T]]): Traversable[Disagreement[T]] = {
    val trees: Map[T, FullMVTurnImpl] = edges.keySet.map(e => (e, newTurn())).toMap

    // put all transactions under a common locked lock, so that all locking assertions hold
    trees.values.foreach(_.awaitAndSwitchPhase(TurnPhase.Executing))
    trees.values.reduce{ (tA, tB) =>
      val resA = Await.result(tA.lock(), Duration.Zero)
      val resB = Await.result(tB.trySubsume(resA), Duration.Zero)
      assert(resB === Successful)
      resA.asyncUnlock()
      tA
    }
    Await.result(trees.head._2.lock(), Duration.Zero)
    SerializationGraphTracking.lock.lock()
    try {
      for ((from, tos) <- edges; to <- tos) {
        val fromTree = trees(from)
        val toTree = trees(to)
        if (!fromTree.isTransitivePredecessor(toTree)) {
          Await.result(fromTree.addPredecessor(toTree.selfNode), Duration.Zero)
        }
      }
    } finally SerializationGraphTracking.lock.unlock()

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
