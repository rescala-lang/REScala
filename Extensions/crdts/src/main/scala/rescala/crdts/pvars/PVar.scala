package rescala.crdts.pvars

import rescala._
import rescala.crdts.statecrdts.StateCRDT

case class PVar[A <: StateCRDT](initial: A) extends Publishable[A] {
  override val internalChanges: Evt[A] = Evt[A]
  override val externalChanges: Evt[A] = Evt[A]

  def apply(a: A): Unit = set(a)

  def set(a: A): Unit = internalChanges.fire(a)
}
