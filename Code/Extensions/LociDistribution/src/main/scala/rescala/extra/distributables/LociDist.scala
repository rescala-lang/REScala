package rescala.extra.distributables

import loci.registry.{Binding, Registry}
import loci.transmitter.RemoteRef
import rescala.core.{Scheduler, Struct}
import rescala.extra.lattices.delta.crdt.CRDTInterface
import rescala.extra.lattices.delta.{Delta, UIJDLattice}
import rescala.operator.{Evt, Observe, Signal}
import scribe.Execution.global

import scala.concurrent.Future
import scala.util.{Failure, Success}

object LociDist {

  def distributeDeltaCRDT[A: UIJDLattice, S <: Struct: Scheduler](
      signal: Signal[CRDTInterface[A], S],
      deltaEvt: Evt[Delta[A], S],
      registry: Registry
  )(binding: Binding[A => Unit] { type RemoteCall = A => Future[Unit] }): Unit = {
    registry.bindPerRemote(binding) { remoteRef => deltaState =>
      deltaEvt.fire(Delta(remoteRef.toString, deltaState))
    }

    var observers = Map[RemoteRef, Observe[S]]()
    var resendBuffer = Map[RemoteRef, A]()

    def registerRemote(remoteRef: RemoteRef): Unit = {
      val remoteUpdate: A => Future[Unit] = registry.lookup(binding, remoteRef)

      // Send full state to initialize remote
      val currentState = signal.readValueOnce
      remoteUpdate(currentState.crdt.state)

      // Whenever the crdt is changed propagate the delta
      // Praktisch wäre etwas wie crdt.observeDelta
      observers += (remoteRef -> signal.observe { s =>
        s.lastDelta foreach { delta =>
          val buffered = resendBuffer.get(remoteRef)

          val combinedState = if (delta.replicaID == remoteRef.toString)
            buffered
          else
            Some(buffered.fold(delta.deltaState) {
              UIJDLattice[A].merge(_, delta.deltaState)
            })

          combinedState.foreach { s =>
            val mergedResendBuffer = resendBuffer.updatedWith(remoteRef) {
              case None => Some(s)
              case Some(prev) => Some(UIJDLattice[A].merge(prev, s))
            }

            if (remoteRef.connected) {
              remoteUpdate(s).onComplete {
                case Success(_) =>
                  println(s"successfully sent delta: $s")
                  resendBuffer = resendBuffer.removed(remoteRef)
                case Failure(_) =>
                  resendBuffer = mergedResendBuffer
              }
            } else {
              resendBuffer = mergedResendBuffer
            }
          }
        }
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
