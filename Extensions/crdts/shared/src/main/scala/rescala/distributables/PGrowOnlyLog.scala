package rescala.distributables

import rescala.default._
import rescala.distributables.DistributedSignal.PVarFactory
import rescala.lattices.sequences.RGOA
import rescala.lattices.sequences.RGOA.RGOA

case class PGrowOnlyLog[A](initial: RGOA[A] = RGOA.empty[A])
  extends DistributedSignal[List[A], RGOA[A]](initial, _.value)(RGOA.lattice[A]) {

  def append(a: A): Unit = {
    crdtSignal.transform(_.append(a))
  }

  // allows the log to log events of type a and append them to the log
  def observe(e: Event[A]): Unit = e.observe(a => append(a))
}

object PGrowOnlyLog {
  /**
    * Allows creation of PVertexLogs by passing a set of initial values.
    */
  def apply[A](values: List[A]): PGrowOnlyLog[A] = new PGrowOnlyLog[A](RGOA(values))

  //noinspection ConvertExpressionToSAM
  implicit def PGrowOnlyLogFactory[A]: PVarFactory[PGrowOnlyLog[A]] =
    new PVarFactory[PGrowOnlyLog[A]] {
      override def apply(): PGrowOnlyLog[A] = PGrowOnlyLog[A]()
    }
}
