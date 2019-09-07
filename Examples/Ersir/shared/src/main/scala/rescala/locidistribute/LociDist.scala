package rescala.locidistribute

import loci.registry.{Binding, Registry}
import loci.serializer.circe._
import loci.transmitter.{AbstractionRef, Channel, MarshallableArgument, RemoteRef}
import rescala.core.{InitialChange, Scheduler, Struct}
import rescala.extra.lattices.Lattice
import rescala.reactives
import rescala.reactives.Observe

import scala.concurrent.Future

object LociDist {

  def distribute[A: Lattice : MarshallableArgument,
  S <: Struct](signal: reactives.Signal[A, S],
               registry: Registry,
               scheduler: Scheduler[S])
  : Unit = {
    val signalName = signal.name.str
    println(s"binding $signalName")
    val signalBinding = Binding[A => Unit](signalName)
    registry.bind(signalBinding) { newValue =>
      scheduler.forceNewTransaction(signal) { admissionTicket =>
        admissionTicket.recordChange(new InitialChange[S] {
          override val source = signal
          override def writeValue(b: source.Value, v: source.Value => Unit): Boolean = {
            val merged = b.map(Lattice[A].merge(_, newValue)).asInstanceOf[source.Value]
            if (merged != b) {
              v(merged)
              true
            }
            else false
          }
        })
      }
      println(s"update for $signalName complete")
    }

    var observers = Map[RemoteRef, Observe[S]]()

    def registerRemote(remoteRef: RemoteRef) = {
      println(s"registering new remote $signalName")
      val remoteUpdate: A => Future[Unit] = registry.lookup(signalBinding, remoteRef)
      observers += (remoteRef -> signal.observe { s =>
        println(s"calling remote observers for $signalName")
        if (remoteRef.connected) try {
          remoteUpdate(s)
        } catch {
          case other: Throwable => // nothin'
        }
      }(scheduler))
    }


    registry.remotes.foreach(registerRemote)
    registry.remoteJoined.notify(registerRemote)
    registry.remoteLeft.notify { remoteRef =>
      observers(remoteRef).remove()(scheduler)
    }
  }

}
