package rescala.crdts.pvars

import rescala.Evt
import rescala.crdts.pvars.Publishable.PVarFactory
import rescala.crdts.statecrdts.counters.GCounter

/**
  * DistributedGCounters are increase-only counter variables.
  *
  * @param initial The initial value of this variable.
  */
case class PGrowOnlyCounter(initial: GCounter = GCounter(0),
                            internalChanges: rescala.Evt[GCounter] = Evt[GCounter],
                            externalChanges: rescala.Evt[GCounter] = Evt[GCounter])
  extends Publishable[Int, GCounter] {

  def increase: Int = {
    internalChanges.fire(crdtSignal.readValueOnce.increase)
    value
  }
}

object PGrowOnlyCounter {

  /*
  object PVarTransmittable {
    implicit def rescalaSignalTransmittable[ValueType, CrdtType, S](implicit
                                                                    transmittable: Transmittable[CrdtType, S, CrdtType],
                                                                    serializable: Serializable[S], pVarFactory: PVarFactory[ValueType,CrdtType]) = {
      type From = CrdtType
      type To = CrdtType
      type P = Publishable[ValueType, CrdtType]

      new PushBasedTransmittable[P, From, S, To, P] {


        def send(value: P, remote: RemoteRef, endpoint: Endpoint[From, To]): To = {

          val observer = value.internalChanges.observe(c => endpoint.send(c))

          endpoint.receive notify value.externalChanges.fire

          endpoint.closed notify { _ => observer.remove }

          value.crdtSignal.readValueOnce
        }

        def receive(value: To, remote: RemoteRef, endpoint: Endpoint[From, To]): P = {
          val pvar: P = pVarFactory.create()
          locally(pvar.valueSignal)
          pvar.externalChanges.fire(value)

          println(s"received $value")
          println(s"before: $pvar, ")

          endpoint.receive notify pvar.externalChanges.fire
          val observer = pvar.internalChanges.observe(c => endpoint.send(c))
          endpoint.closed notify { _ => observer.remove }

          //println(s"manual ${implicitly[StateCRDT[ValueType, CrdtType]].merge(pvar.crdtSignal.readValueOnce, value)}")

          println(s"after: $pvar")

          pvar
        }
      }
    }
  }
  */

  /*

  implicit def pGrowOnlyCounterTransmittable[S](implicit
                                                transmittable: Transmittable[GCounter, S, GCounter],
                                                serializable: Serializable[S], pVarFactory: PVarFactory[PGrowOnlyCounter]) = {
    type From = GCounter
    type To = GCounter

    new PushBasedTransmittable[PGrowOnlyCounter, From, S, To, PGrowOnlyCounter] {


      def send(value: PGrowOnlyCounter, remote: RemoteRef, endpoint: Endpoint[From, To]): To = {

        val observer = value.internalChanges.observe(c => endpoint.send(c))

        endpoint.receive notify value.externalChanges.fire

        endpoint.closed notify { _ => observer.remove }

        value.crdtSignal.readValueOnce
      }

      def receive(value: To, remote: RemoteRef, endpoint: Endpoint[From, To]): PGrowOnlyCounter = {
        val counter: PGrowOnlyCounter = pVarFactory.create()
        locally(counter.valueSignal)
        counter.externalChanges fire value

        println(s"received $value")
        println(s"before: $counter, ")

        endpoint.receive notify counter.externalChanges.fire
        val observer = counter.internalChanges.observe(c => endpoint.send(c))
        endpoint.closed notify { _ => observer.remove }

        // println(s"manual ${implicitly[StateCRDT[Int, GCounter]].merge(counter.crdtSignal.readValueOnce, value)}")

        println(s"after: $counter")

        counter
      }
    }

  }

  */


  /**
    * Allows creation of DistributedGCounters by passing a start value.
    */
  def apply(start: Int): PGrowOnlyCounter = {
    val init: GCounter = GCounter(start)
    new PGrowOnlyCounter(init)
  }

  implicit object PGrowOnlyCounterFactory extends PVarFactory[PGrowOnlyCounter] {
    override def apply(): PGrowOnlyCounter = PGrowOnlyCounter()
  }


  /*
  implicit val PGrowOnlyCounterPVar = new PVar[Int,GCounter,PGrowOnlyCounter] {
    val stateCRDT : StateCRDT[Int,GCounter] = implicitly

    override def create(): PGrowOnlyCounter = PGrowOnlyCounter(0)
    override val initial: GCounter = GCounter(0)
    override val internalChanges: rescala.Evt[GCounter] = Evt[GCounter]
    override val externalChanges: rescala.Evt[GCounter] = Evt[GCounter]

  }
  */

}
