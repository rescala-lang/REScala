package rescala.distributables

import loci.registry.{Binding, Registry}
import loci.transmitter.RemoteRef
import rescala.core.{InitialChange, Scheduler, Struct}
import rescala.lattices.Lattice
import rescala.reactives
import rescala.reactives.Observe

import scala.concurrent.Future

object LociDist {

  def distribute[A: Lattice, S <: Struct : Scheduler]
  (signal: reactives.Signal[A, S],
   registry: Registry)
  (binding: Binding[A => Unit] {type RemoteCall = A => Future[Unit]})
  : Unit = {
    val signalName = signal.name.str
    println(s"binding $signalName")
    registry.bind(binding) { newValue =>
      println(s"received value for $signalName: ${newValue.hashCode()}")
      Scheduler[S].executeTurn(signal) { admissionTicket =>
        admissionTicket.recordChange(new InitialChange[S] {
          override val source = signal
          override def writeValue(b: source.Value, v: source.Value => Unit): Boolean = {
            val merged = b.map(Lattice[A].merge(_, newValue)).asInstanceOf[source.Value]
            println(s"writing ${newValue.hashCode()} onto ${b.hashCode()}, result is ${merged.hashCode()}")
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

    def registerRemote(remoteRef: RemoteRef): Unit = {
      println(s"registering new remote $remoteRef for $signalName")
      val remoteUpdate: A => Future[Unit] = registry.lookup(binding, remoteRef)
      observers += (remoteRef -> signal.observe { s =>
        println(s"calling remote observer on $remoteRef for $signalName")
        if (remoteRef.connected) remoteUpdate(s)
      })
    }


    registry.remoteJoined.notify(registerRemote)
    registry.remotes.foreach(registerRemote)
    registry.remoteLeft.notify { remoteRef =>
      println(s"removing remote $remoteRef")
      observers(remoteRef).remove()
    }
  }

}
