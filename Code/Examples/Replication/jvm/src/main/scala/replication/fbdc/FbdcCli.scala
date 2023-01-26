package replication.fbdc

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import de.rmgk.options.{Argument, Style}
import kofre.base.{Bottom, Id, Lattice}
import kofre.datatypes.{AddWinsSet, CausalQueue, ObserveRemoveMap, ReplicatedList}
import kofre.datatypes.alternatives.ObserveRemoveSet
import kofre.dotted.{Dotted, DottedLattice, HasDots}
import loci.communicator.tcp.TCP
import loci.registry.Registry
import replication.JsoniterCodecs.given
import replication.{DataManager}
import de.rmgk.script.extensions
import kofre.syntax.{PermCausalMutate, PermId}
import kofre.time.Dots

import java.nio.file.Path
import java.util.Timer
import scala.annotation.nowarn

enum Req:
  def executor: Id
  case Fortune(executor: Id)
enum Res:
  case Fortune(req: Req.Fortune, result: String)

case class CliConnections(
    `tcp-listen-port`: Argument[Int, Option, Style.Named] = Argument(_.text("tcp listen port")),
    `tcp-connect`: Argument[String, List, Style.Named] = Argument(_.text("connections").valueName("<ip:port>")),
    `webserver-listen-port`: Argument[Int, Option, Style.Named] = Argument(_.text("webserver listen port")),
    `webserver-static-path`: Argument[Path, Option, Style.Named] = Argument(_.text("webserver static path")),
//    `random-data-time`: Argument[Long, Option, Style.Named] =
//      Argument(_.text("add random data on a time").valueName("milliseconds"))
)

class FbdcCli(settings: CliConnections) {

  val replicaId = Id.gen()
  val registry  = new Registry

  case class State(requests: CausalQueue[Req], responses: CausalQueue[Res]) derives DottedLattice {
    def modReq(using pcm: PermCausalMutate[State, State], pi: PermId[State])(fun: PermCausalMutate[
      CausalQueue[Req],
      CausalQueue[Req]
    ] ?=> PermId[CausalQueue[Req]] ?=> CausalQueue[Req] => Unit) = {
      class Forward extends PermId[CausalQueue[Req]]
          with PermCausalMutate[
            CausalQueue[Req],
            CausalQueue[Req]
          ] {
        override def replicaId(c: CausalQueue[Req]): Id = pi.replicaId(State.this)
        override def mutateContext(
            container: CausalQueue[Req],
            withContext: Dotted[CausalQueue[Req]]
        ): CausalQueue[Req] =
          pcm.mutateContext(State.this, withContext.map(State(_, CausalQueue.empty)))
          container
        override def query(c: CausalQueue[Req]): CausalQueue[Req] = pcm.query(State.this).requests
        override def context(c: CausalQueue[Req]): Dots           = pcm.context(State.this)
      }
      val x = new Forward()
      fun(using x)(using x)(requests)
    }

    def modRes(using pcm: PermCausalMutate[State, State], pi: PermId[State])(fun: PermCausalMutate[
      CausalQueue[Res],
      CausalQueue[Res]
    ] ?=> PermId[CausalQueue[Res]] ?=> CausalQueue[Res] => Unit) = {
      class Forward extends PermId[CausalQueue[Res]]
          with PermCausalMutate[
            CausalQueue[Res],
            CausalQueue[Res]
          ] {
        override def replicaId(c: CausalQueue[Res]): Id = pi.replicaId(State.this)

        override def mutateContext(
            container: CausalQueue[Res],
            withContext: Dotted[CausalQueue[Res]]
        ): CausalQueue[Res] =
          pcm.mutateContext(State.this, withContext.map(State(CausalQueue.empty, _)))
          container

        override def query(c: CausalQueue[Res]): CausalQueue[Res] = pcm.query(State.this).responses

        override def context(c: CausalQueue[Res]): Dots = pcm.context(State.this)
      }
      val x = new Forward()
      fun(using x)(using x)(responses)
    }
  }

  given Bottom[State] = Bottom.derived

  def processFortune(r: Req.Fortune) =
    Res.Fortune(r, process"fortune".run())

  val dataManager =
    @nowarn given JsonValueCodec[State] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    new DataManager[State](replicaId, registry)

  val timer = new Timer()

  def start() =
    settings.`tcp-listen-port`.value match
      case None =>
      case Some(port) =>
        registry.listen(TCP(port))
    settings.`webserver-listen-port`.value match
      case None =>
      case Some(port) =>
        val server = new JettyServer(settings.`webserver-static-path`.value, "/", registry, "0")
        server.start(port)
    settings.`tcp-connect`.value.map { _.split(':') }.collect {
      case Array(ip, port) =>
        (ip.trim, Integer.parseInt(port))
    }.foreach((ip, port) => registry.connect(TCP(ip, port)))
//    settings.`random-data-time`.value match
//      case None     =>
//      case Some(ms) => timer.scheduleAtFixedRate(() => add(), 0, ms)
    if process"which fortune".runResult().isRight
    then
      println(s"enabling fortunes")
      enableFortuneProcessing()
    else
      println(s"fortunes not installed")
  end start

  def enableFortuneProcessing() =
    myRequests.map {
      _.collect {
        case x: Req.Fortune => x
      }
    }.observe { fortunes =>
      val resps = fortunes.map(processFortune)
      dataManager.transform { current =>
        current.modRes { reqq =>
          resps.foreach(reqq.enqueue)
        }
      }
    }

  val mergedState = dataManager.changes.fold(Bottom.empty[Dotted[State]]) { (curr, ts) => curr merge ts.anon }

  val requests   = mergedState.map(_.store.requests.elements)
  val myRequests = requests.map(_.filter(_.executor == replicaId))
  val responses  = mergedState.map(_.store.responses.elements)

//  def add() =
//    dataManager.applyLocalDelta(dataManager.currentValue.append(s"${Id.unwrap(replicaId).take(4)}: $count").anon)
//    dataManager.disseminate()
//    count = count + 1

//  def read() = dataManager.currentValue.toList

}
