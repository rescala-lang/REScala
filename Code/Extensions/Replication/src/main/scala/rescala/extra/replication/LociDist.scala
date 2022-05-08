package rescala.extra.replication

import kofre.base.DecomposeLattice
import kofre.contextual.WithContext
import kofre.decompose.containers.DeltaBufferRDT
import kofre.syntax.WithNamedContext
import loci.registry.{Binding, Registry}
import loci.transmitter.RemoteRef
import rescala.interface.RescalaInterface
import scribe.Execution.global

import scala.concurrent.Future
import scala.util.{Failure, Success}

object LociDist extends LociDist[rescala.default.type](rescala.default)

class LociDist[Api <: RescalaInterface](val api: Api) {
  import api._

  def distributeDeltaCRDT[A](
      signal: Signal[DeltaBufferRDT[A]],
      deltaEvt: Evt[WithNamedContext[A]],
      registry: Registry
  )(binding: Binding[WithContext[A] => Unit, WithContext[A] => Future[Unit]])(implicit dcl: DecomposeLattice[WithContext[A]]): Unit = {
    registry.bindSbj(binding) { (remoteRef: RemoteRef, deltaState: WithContext[A]) =>
      deltaEvt.fire(WithNamedContext(remoteRef.toString, deltaState))
    }

    var observers    = Map[RemoteRef, Disconnectable]()
    var resendBuffer = Map[RemoteRef, WithContext[A]]()

    def registerRemote(remoteRef: RemoteRef): Unit = {
      val remoteUpdate: WithContext[A] => Future[Unit] = registry.lookup(binding, remoteRef)

      // Send full state to initialize remote
      val currentState = signal.readValueOnce.state
      if (currentState != dcl.empty) remoteUpdate(currentState)

      // Whenever the crdt is changed propagate the delta
      // Praktisch wäre etwas wie crdt.observeDelta
      val observer = signal.observe { s =>
        val deltaStateList = s.deltaBuffer.collect {
          case WithNamedContext(replicaID, deltaState) if replicaID != remoteRef.toString => deltaState
        } ++ resendBuffer.get(remoteRef).toList

        val combinedState = deltaStateList.reduceOption(DecomposeLattice[WithContext[A]].merge)

        combinedState.foreach { s =>
          val mergedResendBuffer = resendBuffer.updatedWith(remoteRef) {
            case None       => Some(s)
            case Some(prev) => Some(DecomposeLattice[WithContext[A]].merge(prev, s))
          }

          if (remoteRef.connected) {
            remoteUpdate(s).onComplete {
              case Success(_) =>
                resendBuffer = resendBuffer.removed(remoteRef)
              case Failure(_) =>
                resendBuffer = mergedResendBuffer
            }
          } else {
            resendBuffer = mergedResendBuffer
          }
        }
      }
      observers += (remoteRef -> observer)
    }

    registry.remoteJoined.monitor(registerRemote)
    registry.remotes.foreach(registerRemote)
    registry.remoteLeft.monitor { remoteRef =>
      println(s"removing remote $remoteRef")
      observers(remoteRef).disconnect()
    }
    ()
  }
}
