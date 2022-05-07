package kofre.decompose.containers

import kofre.base.Defs
import kofre.decompose.Delta

trait AntiEntropy[A] {
  def replicaID: Defs.Id
  def recordChange(delta: Delta[A], state: A): Unit
  def getReceivedDeltas: List[Delta[A]]
}
