package rescala.distributables

import rescala.default._
import rescala.distributables.DistributedSignal.PVarFactory
import rescala.lattices.sets.GrowOnlySet

case class PGrowOnlySet[A](initial: GrowOnlySet[A] = GrowOnlySet[A]())
extends DistributedSignal[Set[A], GrowOnlySet[A]](initial, _.value) {

  def add(a: A): Unit = crdtSignal.transform(_.add(a))

  def contains(a: A): Boolean = crdtSignal.readValueOnce.contains(a)
}

object PGrowOnlySet {
  /**
    * Allows creation of DistributedSets by passing a set of initial values.
    */
  def apply[A](values: Set[A]): PGrowOnlySet[A] = {
    val init: GrowOnlySet[A] = GrowOnlySet[A](values)
    new PGrowOnlySet[A](init)
  }

  //noinspection ConvertExpressionToSAM
  implicit def PGrowOnlySetFactory[A]: PVarFactory[PGrowOnlySet[A]] =
    new PVarFactory[PGrowOnlySet[A]] {
      override def apply(): PGrowOnlySet[A] = PGrowOnlySet[A]()
    }
}
