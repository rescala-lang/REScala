package kofre.decompose.containers

import kofre.time.Dots
import kofre.dotted.Dotted
import kofre.syntax.DottedName
import kofre.base.Id

trait AntiEntropy[A] {
  def replicaID: String
  def recordChange(delta: DottedName[A], state: Dotted[A]): Unit
  def getReceivedDeltas: List[DottedName[A]]
  def state: Dotted[A]
}
