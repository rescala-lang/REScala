package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.Delta

trait AntiEntropy[A] {

  def addNeighbor(newNeighbor: String): Unit

  def recordChange(delta: Delta[A], state: A): Unit

  def getReceivedDeltas: List[Delta[A]]

  def replicaID: String

}
