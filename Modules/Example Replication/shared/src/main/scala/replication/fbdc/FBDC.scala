package replication.fbdc

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.Lattice.optionLattice
import rdts.base.{Bottom, Lattice, Uid}
import rdts.datatypes.*
import rdts.datatypes.contextual.CausalQueue.QueueElement
import rdts.datatypes.contextual.{CausalQueue, ObserveRemoveMap, ReplicatedSet}
import rdts.dotted.{Dotted, DottedLattice, HasDots}
import rdts.syntax.{DeltaBuffer, LocalUid, PermCausalMutate}
import rdts.time.VectorClock
import replication.DataManager
import replication.JsoniterCodecs.given

import scala.reflect.ClassTag

enum Req:
  def executor: Uid
  case Fortune(executor: Uid)
  case Northwind(executor: Uid, query: String)
enum Res:
  def req: Req
  case Fortune(req: Req.Fortune, result: String)
  case Northwind(req: Req.Northwind, result: List[Map[String, String]])

class Focus[Inner: Lattice, Outer](dm: DataManager[Outer])(extract: Outer => Inner, wrap: Inner => Outer) {

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

  extension (dm: DataManager[State])
    def modReq          = Focus(dm)(_.requests, d => Bottom.empty.copy(requests = d))
    def modRes          = Focus(dm)(_.responses, d => Bottom.empty.copy(responses = d))
    def modParticipants = Focus(dm)(_.providers, d => Bottom.empty.copy(providers = d))

class FbdcExampleData {
  val replicaId = LocalUid(Uid.gen())

  val dataManager =
    given JsonValueCodec[State] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    new DataManager[State](replicaId)

  def addCapability(capability: String) =
    dataManager.modParticipants { part =>
      part.observeRemoveMap.transform(replicaId.uid)(_.add(using replicaId)(capability))
    }

  val requests = dataManager.mergedState.map(_.data.requests.data.values)
  val myRequests =
    val r = requests.map(_.filter(_.value.executor == replicaId))
    r.observe { reqs =>
      if reqs.nonEmpty
      then
        dataManager.modReq { aws =>
          aws.removeBy { (req: Req) => req.executor == replicaId }
        }
    }
    r
  val responses = dataManager.mergedState.map(_.data.responses.entries.toMap)

  val latestFortune = responses.map(_.get("fortune").flatten.map(_.payload).collect {
    case res: Res.Fortune => res
  })

  val latestNorthwind = responses.map(_.get("northwind").flatten.map(_.payload).collect {
    case res: Res.Northwind => res
  })

  def requestsOf[T: ClassTag] = myRequests.map(_.collect {
    case req @ QueueElement(x: T, _, _) => req.copy(value = x)
  })

  val providers = dataManager.mergedState.map(_.data.providers)

}
