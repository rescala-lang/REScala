package rescala.distributables

import rescala.default._
import rescala.distributables.DistributedSignal.PVarFactory

case class PGrowOnlySet[A](initial: Set[A] = Set[A]())
extends DistributedSignal[Set[A], Set[A]](initial, x => x) {

  def add(a: A): Unit = crdtSignal.transform(_ + a)

  def contains(a: A): Boolean = crdtSignal.readValueOnce.contains(a)
}

object PGrowOnlySet {
  /**
    * Allows creation of DistributedSets by passing a set of initial values.
    */
  def apply[A](values: Set[A]): PGrowOnlySet[A] = {
    new PGrowOnlySet[A](values)
  }

  //noinspection ConvertExpressionToSAM
  implicit def PGrowOnlySetFactory[A]: PVarFactory[PGrowOnlySet[A]] =
    new PVarFactory[PGrowOnlySet[A]] {
      override def apply(): PGrowOnlySet[A] = PGrowOnlySet[A](Set())
    }
}
