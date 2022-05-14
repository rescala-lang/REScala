package kofre.decompose.containers

import kofre.base.Defs
import kofre.time.Dots
import kofre.decompose.Delta
import kofre.syntax.WithNamedContext
import kofre.contextual.Dotted

trait AntiEntropy[A] {
  def replicaID: Defs.Id
  def recordChange(delta: WithNamedContext[A], state: Dotted[A]): Unit
  def getReceivedDeltas: List[WithNamedContext[A]]
  def state: Dotted[A]
}
