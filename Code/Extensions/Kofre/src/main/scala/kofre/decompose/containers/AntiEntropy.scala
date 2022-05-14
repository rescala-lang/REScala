package kofre.decompose.containers

import kofre.base.Defs
import kofre.time.Dots
import kofre.decompose.Delta
import kofre.dotted.Dotted
import kofre.syntax.DottedName

trait AntiEntropy[A] {
  def replicaID: Defs.Id
  def recordChange(delta: DottedName[A], state: Dotted[A]): Unit
  def getReceivedDeltas: List[DottedName[A]]
  def state: Dotted[A]
}
