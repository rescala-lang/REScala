package rescala.extra.distributables

import rescala.default._
import rescala.extra.distributables.DistributedSignal.PVarFactory
import rescala.extra.lattices.sets.ORSet

class PSet[A](initial: ORSet[A] = ORSet.empty)
extends DistributedSignal[Set[A], ORSet[A]](initial, _.value) {

  def add(a: A): Unit = crdtSignal.transform(_.add(a))

  def remove(a: A): Unit = crdtSignal.transform(_.remove(a))

  def contains(a: A): Boolean = crdtSignal.readValueOnce.contains(a)
}

object PSet {
  /**
    * Allows creation of DistributedSets by passing a set of initial values.
    */
  def apply[A](values: Set[A]): PSet[A] = new PSet[A](ORSet[A](values))

  //noinspection ConvertExpressionToSAM
  implicit def PSetFactory[A]: PVarFactory[PSet[A]] =
    new PVarFactory[PSet[A]] {
      override def apply(): PSet[A] = PSet[A](Set.empty)
    }

}
