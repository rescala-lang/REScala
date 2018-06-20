package rescala.crdts.pvars

import loci.transmitter._
import loci.serializer.upickle._
import rescala._
import rescala.crdts.pvars.Publishable.{PVarFactory, PVarTransmittable}
import rescala.crdts.statecrdts.counters.GCounter
import rescala.crdts.statecrdts.sets.ORSet

case class PSet[A](initial: ORSet[A] = ORSet[A](),
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


  implicit def PSetFactory[A]: PVarFactory[PSet[A]] =
    new PVarFactory[PSet[A]] {
      override def apply(): PSet[A] = PSet[A]()
    }


  implicit def pSetTransmittableManual[A, S](implicit
                                             transmittable: Transmittable[ORSet[A], S, ORSet[A]],
                                             serializable: Serializable[S], pVarFactory: PVarFactory[PSet[A]]): PushBasedTransmittable[PSet[A], ORSet[A], S, ORSet[A], PSet[A]] = {
    type From = ORSet[A]
    type To = ORSet[A]

    new PushBasedTransmittable[PSet[A], From, S, To, PSet[A]] {


      def send(value: PSet[A], remote: RemoteRef, endpoint: Endpoint[From, To]): To = {

        val observer = value.internalChanges.observe(c => endpoint.send(c))

        endpoint.receive notify value.externalChanges.fire

        endpoint.closed notify { _ => observer.remove }

        value.crdtSignal.readValueOnce
      }

      def receive(value: To, remote: RemoteRef, endpoint: Endpoint[From, To]): PSet[A] = {
        val counter: PSet[A] = pVarFactory()
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

  implicit def pSetTransmittable[A, S](implicit transmittable: Transmittable[ORSet[A], S, ORSet[A]],
                                       serializable: Serializable[S], pVarFactory: PVarFactory[PSet[A]]) = {
    new PVarTransmittable[S, ORSet[A], PSet[A]]
  }
}
