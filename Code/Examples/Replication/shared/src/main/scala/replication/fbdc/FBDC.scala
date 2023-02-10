package replication.fbdc

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import de.rmgk.options.{Argument, Style}
import kofre.base.{Bottom, Id, Lattice}
import kofre.datatypes.alternatives.ObserveRemoveSet
import kofre.datatypes.{AddWinsSet, CausalQueue, LastWriterWins, ObserveRemoveMap, ReplicatedList}
import kofre.dotted.{Dotted, DottedLattice, HasDots}
import kofre.syntax.{PermCausalMutate, ReplicaId}
import kofre.time.{Dots, VectorClock}
import loci.communicator.tcp.TCP
import loci.registry.Registry
import replication.DataManager
import replication.JsoniterCodecs.given

import scala.reflect.ClassTag
import kofre.base.Lattice.optionLattice
import kofre.datatypes.CausalQueue.QueueElement
import kofre.datatypes.LastWriterWins.TimedVal

import java.nio.file.Path
import java.util.Timer
import scala.annotation.nowarn

enum Req:
  def executor: Id
  case Fortune(executor: Id)
  case Northwind(executor: Id, query: String)
enum Res:
  def req: Req
  case Fortune(req: Req.Fortune, result: String)
  case Northwind(req: Req.Northwind, result: List[Map[String, String]])

class FbdcExampleData {
  val replicaId = Id.gen()
  val registry  = new Registry

  given Bottom[State] = Bottom.derived

  val dataManager =
    @nowarn given JsonValueCodec[State] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    new DataManager[State](replicaId, registry)

  def addCapability(capability: String) =
    dataManager.transform { current =>
      current.modParticipants { part =>
        part.observeRemoveMap.mutateKeyNamedCtx(replicaId)(_.add(using replicaId)(capability))
      }
    }

  type RespValue = Option[TimedVal[Res]]

  given Ordering[VectorClock]    = VectorClock.vectorClockTotalOrdering
  given DottedLattice[RespValue] = DottedLattice.liftLattice
  given HasDots[RespValue] with {
    override def dots(a: RespValue): Dots = Dots.empty
  }

  case class State(
      requests: CausalQueue[Req],
      responses: ObserveRemoveMap[String, RespValue],
      providers: ObserveRemoveMap[Id, AddWinsSet[String]]
  ) derives DottedLattice, HasDots {

    type Mod[T] = PermCausalMutate[T, T] ?=> T => Unit

    class Forward[T](select: State => T, wrap: T => State)(using
        pcm: PermCausalMutate[State, State],
    ) extends PermCausalMutate[T, T] {

      override def mutateContext(container: T, withContext: Dotted[T]): T =
        pcm.mutateContext(State.this, withContext.map(wrap))
        container

      override def query(c: T): T = select(pcm.query(State.this))

      override def context(c: T): Dots = pcm.context(State.this)
    }

    def modReq(using pcm: PermCausalMutate[State, State])(fun: Mod[CausalQueue[Req]]) = {
      val x = new Forward(_.requests, State(_, Bottom.empty, Bottom.empty))
      fun(using x)(requests)
    }

    def modRes(using
        pcm: PermCausalMutate[State, State],
        id: ReplicaId
    )(fun: Mod[ObserveRemoveMap[String, RespValue]]) = {
      val x = new Forward(_.responses, State(Bottom.empty, _, Bottom.empty))
      fun(using x)(responses)
    }

    def modParticipants(using
        pcm: PermCausalMutate[State, State]
    )(fun: Mod[ObserveRemoveMap[Id, AddWinsSet[String]]]) = {
      val x = new Forward(_.providers, State(Bottom.empty, Bottom.empty, _))
      fun(using x)(providers)
    }
  }

  val requests = dataManager.mergedState.map(_.store.requests.values)
  val myRequests =
    val r = requests.map(_.filter(_.value.executor == replicaId))
    r.observe { reqs =>
      if reqs.nonEmpty
      then
        dataManager.transform { state =>
          state.modReq { aws =>
            aws.removeBy { (req: Req) => req.executor == replicaId }
          }
        }
    }
    r
  val responses = dataManager.mergedState.map(_.store.responses.entries.toMap)

  val latestFortune = responses.map(_.get("fortune").flatten.map(_.payload).collect {
    case res: Res.Fortune => res
  })

  val latestNorthwind = responses.map(_.get("northwind").flatten.map(_.payload).collect {
    case res: Res.Northwind => res
  })

  def requestsOf[T: ClassTag] = myRequests.map(_.collect {
    case req @ QueueElement(x: T, _, _) => req.copy(value = x)
  })

  val providers = dataManager.mergedState.map(_.store.providers)

}
