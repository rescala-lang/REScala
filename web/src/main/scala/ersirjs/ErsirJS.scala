package ersirjs

import ersir.shared.Log.Log
import ersir.shared._
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import loci.communicator.ws.akka.WS
import loci.registry.Registry
import loci.serializer.circe._
import loci.transmitter.RemoteRef
import org.scalajs.dom
import rescala.Tags._
import rescala.default._
import rescala.lattices.Lattice
import rescala.lattices.sequences.RGOA
import rescala.lattices.sequences.RGOA.RGOA
import rescala.locidistribute.LociDist
import scalatags.JsDom.attrs.cls
import scalatags.JsDom.implicits._

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object ErsirJS {

  type Postings = Epoche[RGOA[Posting]]

  ReMqtt.start()

  val mqttStream: Event[Postings] =
    ReMqtt
    .topicstream("ersir/entries")
    .map(decode[Postings])
    .collect { case Right(rg) => rg }


  val emergencies      = ReMqtt.topicstream("city/alert_state")
  val currentEmergency = emergencies.latest("")

  val connectClass = ReMqtt.connected.map {
    case true => "connected"
    case _    => ""
  }

  val index = new Index(connectClass)

  val addPost = index.addPost.event
                .filter(p => p.title.nonEmpty || p.img.nonEmpty)

  val postings: Signal[Postings] =
    Events.foldAll(Epoche(RGOA(List.empty[Posting])))(state => Seq(
      mqttStream >> { rg => Lattice.merge[Postings](state, rg) },
      addPost >> { post => state.map(_.prepend(post)) },
      index.reset.event >> { rs => state.next(RGOA(Nil)) }
    ))(implicitly, "postings")


  postings.observe { crdt =>
    if (ReMqtt.isConnected()) {
      val json = crdt.asJson.noSpaces
      ReMqtt.send("ersir/entries", json)
    }
  }

  val registry = new Registry

  LociDist.distribute(postings, registry, scheduler)


  def main(args: Array[String]): Unit = {
    dom.window.document.title = "Emergencity RSS Reader"
    dom.document.body = index.gen(postings).apply(cls := currentEmergency).render
    var connecting = false
    scala.scalajs.js.timers.setInterval(1000) {
      if (!connecting && !registry.remotes.exists(_.connected)) {
        connecting = true
        lociConnect().onComplete { res =>
          Log.trace(s"loci connection try finished: $res")
          connecting = false
        }
      }
    }
  }

  def lociConnect(): Future[RemoteRef] = {
    val wsUri: String = {
      val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
      s"$wsProtocol://${dom.document.location.host}${dom.document.location.pathname}ws"
    }

    val connection: Future[RemoteRef] = registry.connect(WS(wsUri))
    Log.debug(s"connecting loci to $wsUri …")
    connection.foreach { remote =>
      Notifications.send(s"connected to server")
      remote.disconnected.foreach { _ =>
        Notifications.send(s"disconnected from server")
        Log.debug(s"loci reconnect")
      }
    }
    connection
  }

}
