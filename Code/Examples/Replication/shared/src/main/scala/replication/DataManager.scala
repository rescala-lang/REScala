package replication

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import kofre.base.{Bottom, Id, Lattice}
import kofre.dotted.{Dotted, DottedLattice}
import kofre.syntax.{DeltaBuffer, Named, PermCausalMutate, PermId}
import kofre.time.Dots
import loci.registry.{Binding, Registry}
import loci.serializer.jsoniterScala.given
import loci.transmitter.{IdenticallyTransmittable, RemoteRef, Transmittable}
import replication.JsoniterCodecs.given
import rescala.default.{Evt, Event}

import java.util.concurrent.atomic.AtomicReference
import scala.collection.View
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import kofre.base.Lattice.optionLattice

type PushBinding[T] = Binding[T => Unit, T => Future[Unit]]

class DataManager[State: JsonValueCodec: DottedLattice: Bottom](
    replicaId: Id,
    registry: Registry
) {

  type TransferState = Named[Dotted[State]]

  // note that deltas are not guaranteed to be ordered the same in the buffers
  private val lock: AnyRef                      = new {}
  private var localDeltas: List[TransferState]  = Nil
  private var localBuffer: List[TransferState]  = Nil
  private var remoteDeltas: List[TransferState] = Nil

  private var contexts: Map[RemoteRef, Dots] = Map.empty

  private val changeEvt             = Evt[TransferState]()
  val changes: Event[TransferState] = changeEvt
  val mergedState                   = changes.fold(Bottom.empty[Dotted[State]]) { (curr, ts) => curr merge ts.anon }
  val currentContext                = mergedState.map(_.context)

  def applyLocalDelta(dotted: Dotted[State]): Unit = lock.synchronized {
    val named = Named(replicaId, dotted)
    localBuffer = named :: localBuffer
    changeEvt.fire(named)
  }

  class ManagedPermissions extends PermCausalMutate[State, State] with PermId[State] {
    override def replicaId(c: State): Id = DataManager.this.replicaId

    override def query(c: State): State = c

    override def mutateContext(container: State, withContext: Dotted[State]): State =
      applyLocalDelta(withContext)
      container

    override def context(c: State): Dots = currentContext.now
  }

  def transform(fun: ManagedPermissions ?=> State => Unit) = lock.synchronized {
    fun(using ManagedPermissions())(mergedState.now.store)
  }

  def allDeltas: View[Named[Dotted[State]]] = lock.synchronized {
    View(localBuffer, remoteDeltas, localDeltas).flatten
  }

  def requestMissingBinding[PushBinding[Dots]] =
    given IdenticallyTransmittable[Dots] = IdenticallyTransmittable[Dots]()
    Binding[Dots => Unit]("requestMissing")

  val pushStateBinding: PushBinding[TransferState] =
    given JsonValueCodec[TransferState]           = JsonCodecMaker.make
    given IdenticallyTransmittable[TransferState] = IdenticallyTransmittable[TransferState]()
    Binding[TransferState => Unit]("pushState")

  def updateRemoteContext(rr: RemoteRef, dots: Dots) = lock.synchronized {
    contexts = contexts.updatedWith(rr)(curr => curr merge Some(dots))
  }

  registry.bindSbj(pushStateBinding) { (rr: RemoteRef, named: TransferState) =>
    lock.synchronized {
      updateRemoteContext(rr, named.anon.context)
      remoteDeltas = named :: remoteDeltas
    }
    changeEvt.fire(named)
  }

  registry.bindSbj(requestMissingBinding) { (rr: RemoteRef, knows: Dots) =>
    pushDeltas(allDeltas.filterNot(dt => dt.anon.context <= knows), rr)
    updateRemoteContext(rr, currentContext.now merge knows)
  }

  def disseminate() =
    val deltas = lock.synchronized {
      val deltas = localBuffer
      localBuffer = Nil
      deltas
    }
    registry.remotes.foreach { remote =>
      pushDeltas(deltas.view, remote)
    }

  def requestMissingFrom(rr: RemoteRef) =
    val req = registry.lookup(requestMissingBinding, rr)
    req(currentContext.now)

  private def pushDeltas(deltas: View[TransferState], remote: RemoteRef): Unit = {
    val push = registry.lookup(pushStateBinding, remote)
    deltas.map(push).foreach(_.failed.foreach { cause =>
      println(s"sending to $remote failed: ${cause.toString}")
    })
  }
}
