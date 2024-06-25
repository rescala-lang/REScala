package replication.fbdc

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.Lattice.optionLattice
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.*
import rdts.datatypes.contextual.CausalQueue.QueueElement
import rdts.datatypes.contextual.{CausalQueue, ObserveRemoveMap, ReplicatedSet}
import rdts.dotted.{Dotted, DottedLattice, HasDots, Obrem}
import rdts.time.VectorClock
import reactives.operator.{Event, Signal}
import replication.DataManager
import replication.JsoniterCodecs.given
import replication.fbdc.State.modParticipants

import scala.collection.immutable.Queue
import scala.reflect.ClassTag

enum Req:
  def executor: Uid
  case Fortune(executor: Uid)
  case Northwind(executor: Uid, query: String)
enum Res:
  def req: Req
  case Fortune(req: Req.Fortune, result: String)
  case Northwind(req: Req.Northwind, result: List[Map[String, String]])

class Focus[Inner: Lattice, Outer](dm: ExtraDataManager[Outer])(extract: Outer => Inner, wrap: Inner => Outer) {

  def apply(fun: Inner => Inner): Unit = {
    dm.transform { outer =>
      wrap(fun(extract(outer)))
    }
  }
}

type RespValue = Option[LastWriterWins[Res]]
given Ordering[VectorClock] = VectorClock.vectorClockTotalOrdering

given HasDots[RespValue]       = HasDots.noDots
given DottedLattice[RespValue] = Dotted.lattice

case class State(
    requests: Dotted[CausalQueue[Req]],
    responses: Dotted[ObserveRemoveMap[String, RespValue]],
    providers: Dotted[ObserveRemoveMap[Uid, ReplicatedSet[String]]]
) derives Lattice, Bottom

object State:

  extension (dm: ExtraDataManager[State])
    def modReq          = Focus(dm)(_.requests, d => Bottom.empty.copy(requests = d))
    def modRes          = Focus(dm)(_.responses, d => Bottom.empty.copy(responses = d))
    def modParticipants = Focus(dm)(_.providers, d => Bottom.empty.copy(providers = d))

class FbdcExampleData {
  val replicaId = LocalUid(Uid.gen())

  val dataManager: ExtraDataManager[State] = {
    given JsonValueCodec[State] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    val dm =
      Event.fromCallback {
        new DataManager[State](replicaId, _ => (), Event.handle)
      }
    ExtraDataManager[State](dm.data, dm.event)
  }

  def addCapability(capability: String) =
    dataManager.modParticipants { part =>
      part.mod { ormap =>
        val Obrem(data, obs, rem) = ormap.transform(replicaId.uid)(_.mod(_.add(using replicaId)(capability)))
        Dotted(data, obs union rem)
      }
    }

  val requests = dataManager.mergedState.map(_.data.requests.data.values)
  val myRequests =
    val r = requests.map(_.filter(_.value.executor == replicaId.uid))
    r.observe { reqs =>
      if reqs.nonEmpty
      then
        dataManager.modReq { aws =>
          aws.mod(_.removeBy { (req: Req) => req.executor == replicaId.uid })
        }
    }
    r
  val responses = dataManager.mergedState.map(_.data.responses.data.entries.toMap)

  val latestFortune = responses.map(_.get("fortune").flatten.map(_.payload).collect {
    case res: Res.Fortune => res
  })

  val latestNorthwind = responses.map(_.get("northwind").flatten.map(_.payload).collect {
    case res: Res.Northwind => res
  })

  myRequests.observe: reqs =>
    println(s"my reqs changes to:")
    reqs.foreach(println)
    println(s"==========f")

  def requestsOf[T: ClassTag]: Signal[Queue[QueueElement[T]]] = myRequests.map(_.collect {
    case req @ QueueElement(x: T, _, _) => req.copy(value = x)
  })

  val providers = dataManager.mergedState.map(_.data.providers)

}
