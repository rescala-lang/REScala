package deltaAntiEntropy.tools

import kofre.base.Id
import kofre.dotted.Dotted
import kofre.syntax.DottedName
import kofre.time.Dots

trait IAntiEntropy[A] {
  def replicaID: String
  def recordChange(delta: DottedName[A], state: Dotted[A]): Unit
  def getReceivedDeltas: List[DottedName[A]]
  def state: Dotted[A]
}
