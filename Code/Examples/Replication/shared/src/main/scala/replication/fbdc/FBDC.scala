package replication.fbdc

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import de.rmgk.options.{Argument, Style}
import kofre.base.{Bottom, Id, Lattice}
import kofre.datatypes.alternatives.ObserveRemoveSet
import kofre.datatypes.{AddWinsSet, CausalQueue, ObserveRemoveMap, ReplicatedList}
import kofre.dotted.{Dotted, DottedLattice, HasDots}
import kofre.syntax.{PermCausalMutate, PermId}
import kofre.time.Dots
import loci.communicator.tcp.TCP
import loci.registry.Registry
import replication.DataManager
import replication.JsoniterCodecs.given

import java.nio.file.Path
import java.util.Timer
import scala.annotation.nowarn

enum Req:
  def executor: Id
  case Fortune(executor: Id)
enum Res:
  case Fortune(req: Req.Fortune, result: String)

class FbdcExampleData {
  val replicaId = Id.gen()
  val registry  = new Registry

  given Bottom[State] = Bottom.derived

  val dataManager =
    @nowarn given JsonValueCodec[State] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    new DataManager[State](replicaId, registry)

  case class State(
      requests: CausalQueue[Req],
      responses: CausalQueue[Res],
      providers: ObserveRemoveMap[Id, AddWinsSet[String]]
  ) derives DottedLattice {

    class Forward[T](select: State => T, wrap: T => State)(using pcm: PermCausalMutate[State, State], pi: PermId[State])
        extends PermId[T]
        with PermCausalMutate[T, T] {
      override def replicaId(c: T): Id = pi.replicaId(State.this)
      override def mutateContext(container: T, withContext: Dotted[T]): T =
        pcm.mutateContext(State.this, withContext.map(wrap))
        container
      override def query(c: T): T      = select(pcm.query(State.this))
      override def context(c: T): Dots = pcm.context(State.this)
    }

    type Mod[T] = PermCausalMutate[T, T] ?=> PermId[T] ?=> T => Unit

    def modReq(using pcm: PermCausalMutate[State, State], pi: PermId[State])(fun: Mod[CausalQueue[Req]]) = {
      val x = new Forward(_.requests, State(_, Bottom.empty, Bottom.empty))
      fun(using x)(using x)(requests)
    }

    def modRes(using pcm: PermCausalMutate[State, State], pi: PermId[State])(fun: Mod[CausalQueue[Res]]) = {
      val x = new Forward(_.responses, State(Bottom.empty, _, Bottom.empty))
      fun(using x)(using x)(responses)
    }

    def modParticipants(using
        pcm: PermCausalMutate[State, State],
        pi: PermId[State]
    )(fun: Mod[ObserveRemoveMap[Id, AddWinsSet[String]]]) = {
      val x = new Forward(_.providers, State(Bottom.empty, Bottom.empty, _))
      fun(using x)(using x)(providers)
    }
  }

  val mergedState = dataManager.changes.fold(Bottom.empty[Dotted[State]]) { (curr, ts) => curr merge ts.anon }

  val requests   = mergedState.map(_.store.requests.elements)
  val myRequests = requests.map(_.filter(_.executor == replicaId))
  val responses  = mergedState.map(_.store.responses.elements)

  val providers = mergedState.map(_.store.providers)
}
