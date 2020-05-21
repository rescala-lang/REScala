package rescala.extra.distributables

import loci.registry.{Binding, Registry}
import loci.transmitter.RemoteRef
import rescala.core.{InitialChange, Scheduler, Struct}
import rescala.extra.lattices.Lattice
import rescala.extra.lattices.sequences.RGA
import rescala.extra.lattices.sequences.RGA.RGA
import rescala.interface.RescalaInterface
import rescala.reactives.{Event, Observe, Signal}

import scala.concurrent.Future

object LociDist {

  def dfold[T, Res, S <: Struct : Scheduler]
  (event: Event[T, S])
  (init: Res)(f: (Res, T) => Res)
  (registry: Registry, binding: Binding[RGA[T] => Unit] {type RemoteCall = RGA[T] => Future[Unit]})
  (implicit api: RescalaInterface[S])
  : Signal[Res, S] = {
    val fold = api.Events.foldOne(event, RGA.empty[T]) { (acc, occ) =>
      println(s"appending $acc $occ")
      acc.append(occ) }

    val res = api.Signals.static(fold){st => st.dependStatic(fold).iterator.foldLeft(init){ (acc, occ) =>
      println(s"folding $acc $occ")
      f(acc, occ)}}
    distribute(fold, registry)(binding)
    res
  }

  def distribute[A: Lattice, S <: Struct : Scheduler]
  (signal: Signal[A, S],
   registry: Registry)
  (binding: Binding[A => Unit] {type RemoteCall = A => Future[Unit]})
  : Unit = {
    val signalName = signal.name.str
    println(s"binding $signalName")
    registry.bind(binding) { newValue =>
      println(s"received value for $signalName: ${newValue.hashCode()}")
      Scheduler[S].forceNewTransaction(signal) { admissionTicket =>
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


    registry.remoteJoined.monitor(registerRemote)
    registry.remotes.foreach(registerRemote)
    registry.remoteLeft.monitor { remoteRef =>
      println(s"removing remote $remoteRef")
      observers(remoteRef).remove()
    }
  }

}
