package rescala.crdts.pvars

import rescala.crdts.pvars.DistributedSignal.PVarFactory
import rescala.crdts.statecrdts.sets.ORSet
import rescala.default._

case class PSet[A](initial: ORSet[A] = ORSet[A]())
extends DistributedSignal[Set[A], ORSet[A]](initial) {

  def add(a: A): Unit = localDeviceChange.fire(crdtSignal.readValueOnce.add(a))

  def remove(a: A): Unit = localDeviceChange.fire(crdtSignal.readValueOnce.remove(a))

  def contains(a: A): Boolean = crdtSignal.readValueOnce.contains(a)
}

object PSet {
  /**
    * Allows creation of DistributedSets by passing a set of initial values.
    */
  def apply[A](values: Set[A]): PSet[A] = {
    val init: ORSet[A] = ORSet[A](values)
    new PSet[A](init)
  }

  //noinspection ConvertExpressionToSAM
  implicit def PSetFactory[A]: PVarFactory[PSet[A]] =
    new PVarFactory[PSet[A]] {
      override def apply(): PSet[A] = PSet[A]()
    }

}
