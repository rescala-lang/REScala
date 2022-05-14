package kofre.decompose.containers

import kofre.base.Defs
import kofre.time.Dots
import kofre.decompose.Delta
import kofre.syntax.WithNamedContext
import kofre.contextual.WithContext

trait AntiEntropy[A] {
  def replicaID: Defs.Id
  def recordChange(delta: WithNamedContext[A], state: WithContext[A]): Unit
  def getReceivedDeltas: List[WithNamedContext[A]]
  def state: WithContext[A]
}
