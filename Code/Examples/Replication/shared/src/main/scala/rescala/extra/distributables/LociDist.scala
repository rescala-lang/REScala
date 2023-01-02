package rescala.extra.distributables

import loci.registry.{Binding, Registry}
import loci.transmitter.RemoteRef
import rescala.default._
import kofre.base.Lattice
import rescala.operator.Pulse

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Future

object LociDist {

  def distribute[A: Lattice](
      signal: Signal[A],
      registry: Registry
  )(binding: Binding[A => Unit, A => Future[Unit]]) =
    distributePerRemote(_ => signal, registry)(binding)

  def distributePerRemote[A: Lattice](
      signalFun: RemoteRef => Signal[A],
      registry: Registry
  )(binding: Binding[A => Unit, A => Future[Unit]]): Unit = {

    println(s"starting new distribution for »${binding.name}«")

    registry.bindSbj(binding)((remoteRef: RemoteRef, newValue: A) => {
      val signal: Signal[A] = signalFun(remoteRef)
      val signalName        = signal.name.str
      // println(s"received value for $signalName: ${newValue.hashCode()}")
      scheduler.forceNewTransaction(signal) { admissionTicket =>
        admissionTicket.recordChange(new InitialChange {
          override val source = signal
          override def writeValue(b: source.Value, v: source.Value => Unit): Boolean = {
            val merged = b.asInstanceOf[Pulse[A]].map(Lattice[A].merge(_, newValue)).asInstanceOf[source.Value]
            given CanEqual[source.Value, source.Value] = CanEqual.canEqualAny
            if (merged != b) {
              v(merged)
              true
            } else false
          }
        })
      }
      println(s"update for $signalName complete")
    })

    val observers = new ConcurrentHashMap[RemoteRef, Disconnectable]()

    def registerRemote(remoteRef: RemoteRef): Unit = {
      val signal: Signal[A] = signalFun(remoteRef)
      // val signalName        = signal.name.str
      // println(s"registering new remote $remoteRef for $signalName")
      val remoteUpdate: A => Future[Unit] = {
        println(s"calling lookup on »${binding.name}«")
        registry.lookup(binding, remoteRef)
      }
      observers.put(
        remoteRef,
        signal.observe { s =>
          // println(s"calling remote observer on $remoteRef for $signalName, remote connection: ${remoteRef.connected}")
          if (remoteRef.connected) {
            remoteUpdate(s)
            ()
          } else Option(observers.get(remoteRef)).foreach(_.disconnect())
        }
      )
      ()
    }

    registry.remotes.foreach(registerRemote)
    registry.remoteJoined.foreach(registerRemote)
    registry.remoteLeft.foreach { remoteRef =>
      // println(s"removing remote $remoteRef")
      Option(observers.get(remoteRef)).foreach(_.disconnect())
    }
    ()
  }

}
