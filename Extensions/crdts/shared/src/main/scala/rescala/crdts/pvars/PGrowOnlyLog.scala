package rescala.crdts.pvars

import rescala.crdts.pvars.DistributedSignal.PVarFactory
import rescala.crdts.statecrdts.StateCRDT
import rescala.crdts.statecrdts.sequences.{RGOA, Vertex}
import rescala.default._

case class PGrowOnlyLog[A](initial: RGOA[A] = RGOA[A]())
extends DistributedSignal[List[A], RGOA[A]](initial)(RGOA.RGOAStateCRDTInstance[A]) {

  def append(a: A): Unit = localDeviceChange.fire(crdtSignal.readValueOnce.append(Vertex(a)))

  def contains(a: A): Boolean = crdtSignal.readValueOnce.containsValue(a)

  // allows the log to log events of type a and append them to the log
  def observe(e: Event[A]): Unit = e.observe(a => append(a))
}

object PGrowOnlyLog {
  /**
    * Allows creation of PVertexLogs by passing a set of initial values.
    */
  def apply[A](values: List[A]): PGrowOnlyLog[A] = {
    val init: RGOA[A] = implicitly[StateCRDT[List[A], RGOA[A]]].fromValue(values)
    new PGrowOnlyLog[A](init)
  }

  //noinspection ConvertExpressionToSAM
  implicit def PGrowOnlyLogFactory[A]: PVarFactory[PGrowOnlyLog[A]] =
    new PVarFactory[PGrowOnlyLog[A]] {
      override def apply(): PGrowOnlyLog[A] = PGrowOnlyLog[A]()
    }
}
