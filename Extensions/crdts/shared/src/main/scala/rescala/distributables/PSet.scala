package rescala.distributables

import rescala.default._
import rescala.distributables.DistributedSignal.PVarFactory
import rescala.lattices.sets.ORSet

case class PSet[A](initial: ORSet[A] = ORSet[A]())
extends DistributedSignal[Set[A], ORSet[A]](initial, _.value) {

  def add(a: A): Unit = crdtSignal.transform(_.add(a))

  def remove(a: A): Unit = crdtSignal.transform(_.remove(a))

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
