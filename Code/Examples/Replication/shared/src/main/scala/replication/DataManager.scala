package replication

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import kofre.base.{Bottom, Id, Lattice}
import kofre.dotted.{Dotted, DottedLattice, HasDots}
import kofre.syntax.{Named, PermCausalMutate, PermId}
import kofre.time.Dots
import loci.registry.{Binding, Registry}
import loci.serializer.jsoniterScala.given
import loci.transmitter.{IdenticallyTransmittable, RemoteRef, Transmittable}
import replication.JsoniterCodecs.given
import rescala.default.{Event, Evt, Signal, Var}

import scala.collection.mutable
import java.util.concurrent.atomic.AtomicReference
import scala.collection.View
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import kofre.base.Lattice.optionLattice

import java.util.{Objects, Timer}

type PushBinding[T] = Binding[T => Unit, T => Future[Unit]]

class Key[T](name: String)(using lat: DottedLattice[T], hado: HasDots[T])

case class HMap(keys: Map[String, Key[_]], values: Map[String, Any])

class DataManager[State: JsonValueCodec: DottedLattice: Bottom: HasDots](
    val replicaId: Id,
    val registry: Registry
) {

  type TransferState = Named[Dotted[State]]

  val timer = new Timer()

  timer.scheduleAtFixedRate(
    { () =>
      registry.remotes.foreach(requestMissingFrom)
    },
    10000,
    10000
  )

  registry.remoteJoined.foreach(requestMissingFrom)

  // note that deltas are not guaranteed to be ordered the same in the buffers
  private val lock: AnyRef                      = new {}
  private var localDeltas: List[TransferState]  = Nil
  private var localBuffer: List[TransferState]  = Nil
  private var remoteDeltas: List[TransferState] = Nil

  private val contexts: Var[Map[RemoteRef, Dots]]                            = Var(Map.empty)
  private val filteredContexts: mutable.Map[RemoteRef, Signal[Option[Dots]]] = mutable.Map.empty

  def contextOf(rr: RemoteRef) =
    val internal = filteredContexts.getOrElseUpdate(rr, contexts.map(_.get(rr)))
    internal.map {
      case None       => Dots.empty
      case Some(dots) => dots
    }

  private val changeEvt             = Evt[TransferState]()
  val changes: Event[TransferState] = changeEvt
  val mergedState                   = changes.fold(Bottom.empty[Dotted[State]]) { (curr, ts) => curr merge ts.anon }
  val currentContext                = mergedState.map(_.context)

  val encodedStateSize = mergedState.map(s => writeToArray(s).size)

  def applyLocalDelta(dotted: Dotted[State]): Unit = lock.synchronized {
    val named = Named(replicaId, dotted)
    localBuffer = named :: localBuffer
    changeEvt.fire(named)
    disseminateLocalBuffer()
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

  def updateRemoteContext(rr: RemoteRef, dots: Dots) = {
    contexts.transform(_.updatedWith(rr)(curr => curr merge Some(dots)))
  }

  registry.bindSbj(pushStateBinding) { (rr: RemoteRef, named: TransferState) =>
    lock.synchronized {
      updateRemoteContext(rr, named.anon.context)
      remoteDeltas = named :: remoteDeltas
    }
    changeEvt.fire(named)
  }

  registry.bindSbj(requestMissingBinding) { (rr: RemoteRef, knows: Dots) =>
    val contained = HasDots[State].dots(mergedState.now.store)
    val cc        = currentContext.now
    // we always send all the removals in addition to any other deltas
    val removed = cc subtract contained
    pushDeltas(
      allDeltas.filterNot(dt => dt.anon.context <= knows).concat(List(Dotted(Bottom.empty, removed).named(replicaId))),
      rr
    )
    updateRemoteContext(rr, cc merge knows)
  }

  def disseminateLocalBuffer() =
    val deltas = lock.synchronized {
      val deltas = localBuffer
      localBuffer = Nil
      localDeltas = deltas ::: localDeltas
      deltas
    }
    registry.remotes.foreach { remote =>
      val ctx = contexts.now.getOrElse(remote, Dots.empty)
      pushDeltas(deltas.view, remote)
    }

  def disseminateFull() =
    registry.remotes.foreach { remote =>
      pushDeltas(List(Named(replicaId, mergedState.now)).view, remote)
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
