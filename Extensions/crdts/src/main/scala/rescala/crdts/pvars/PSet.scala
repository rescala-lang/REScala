package rescala.crdts.pvars

import loci.transmitter.{Serializable, Transmittable}
import rescala._
import rescala.crdts.pvars.Publishable.{PVarFactory, PVarTransmittable}
import rescala.crdts.statecrdts.sets.ORSet

case class PSet[A](initial: ORSet[A] = ORSet(),
                   internalChanges: Evt[ORSet[A]] = Evt[ORSet[A]],
                   externalChanges: Evt[ORSet[A]] = Evt[ORSet[A]]) extends Publishable[Set[A], ORSet[A]] {

  def add(a: A): Unit = internalChanges.fire(crdtSignal.readValueOnce.add(a))

  def remove(a: A): Unit = internalChanges.fire(crdtSignal.readValueOnce.remove(a))

  def contains(a: A): Boolean = crdtSignal.readValueOnce.contains(a)
}

object PSet {
  /**
    * Allows creation of DistributedSets by passing a set of initial values.
    */
  def apply[A](values: Set[A]): PSet[A] = {
    val init: ORSet[A] = ORSet().fromValue(values)
    new PSet[A](init)
  }

  implicit def pSetTransmittable[A, S](implicit transmittable: Transmittable[ORSet[A], S, ORSet[A]],
                                       serializable: Serializable[S], pVarFactory: PVarFactory[PSet[A]]) = {
    implicit object PSetFactory extends PVarFactory[PSet[A]] {
      override def apply(): PSet[A] = PSet[A](Set[A]())
    }
    new PVarTransmittable[S, ORSet[A], PSet[A]]
  }
}
