package kofre.decompose.replication

import kofre.Defs
import kofre.decompose.Delta

trait AntiEntropy[A] {
  def replicaID: Defs.Id
  def recordChange(delta: Delta[A], state: A): Unit
  def getReceivedDeltas: List[Delta[A]]
}
