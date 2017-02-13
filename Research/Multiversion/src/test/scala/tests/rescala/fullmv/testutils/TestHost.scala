package tests.rescala.fullmv.testutils

import rescala.fullmv.api._

object TestHost extends Host {
  override object sgt extends SerializationGraphTracking {
    var predecessors = Map[Transaction, Set[Transaction]]().withDefaultValue(Set.empty)
    var successors = Map[Transaction, Set[Transaction]]().withDefaultValue(Set.empty)
    override def getOrder(a: Transaction, b: Transaction): PartialOrderResult = synchronized {
      if(successors(a).contains(b)) {
        FirstFirst
      } else if (predecessors(a).contains(b)) {
        SecondFirst
      } else {
        Unordered
      }
    }
    override def ensureOrder(defender: Transaction, contender: Transaction): OrderResult = synchronized {
      if(successors(defender).contains(contender)) {
        FirstFirst
      } else if (predecessors(defender).contains(contender)) {
        SecondFirst
      } else {
        val newSuccessors = successors(contender) + contender
        val newPredecessors = predecessors(defender) + defender
        for(successor <- newSuccessors) {
          predecessors += successor -> (predecessors(successor) ++ newPredecessors)
        }
        for(predecessor <- newPredecessors) {
          successors += predecessor -> (successors(predecessor) ++ newSuccessors)
        }
        FirstFirst
      }
    }
  }

  override object taskPool extends TaskPool{
    override def addFraming(node: SignalVersionList[_], txn: Transaction): Unit = node.incrementFrame(txn)
    override def addSupersedingFraming(node: SignalVersionList[_], txn: Transaction, superseded: Transaction): Unit = node.incrementSupersedeFrame(txn, superseded)
    override def addNotification(node: SignalVersionList[_], txn: Transaction, changed: Boolean, maybeFollowFrame: Option[Transaction]): Unit = node.notify(txn, changed, maybeFollowFrame)
  }
}
