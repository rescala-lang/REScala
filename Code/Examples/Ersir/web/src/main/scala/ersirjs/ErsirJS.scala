package ersirjs

import ersir.shared._
import io.circe.generic.auto._
import loci.communicator.ws.akka.WS
import loci.registry.Registry
import loci.serializer.circe._
import loci.transmitter.RemoteRef
import org.scalajs.dom
import rescala.default._
import rescala.extra.lattices.sequences.RGOA
import rescala.extra.lattices.sequences.RGOA.RGOA
import rescala.extra.distributables.LociDist

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object ErsirJS {

  type Postings = Epoche[RGOA[Posting]]

  val connectionSignal = Var(false)

  val connectClass = Signal {
    val internet = connectionSignal.value
    s"${if(internet) " hideConnectionIssues" else ""}"
  }


  val index = new Index(connectClass)

  val addPost = index.addPost.event
                .filter(p => p.title.nonEmpty || p.img.nonEmpty)

  val postings: Signal[Postings] =
    Events.foldAll(Epoche(RGOA(List.empty[Posting])))(state => Seq(
      addPost >> { post => state.map(_.prepend(post)) },
      index.reset.event >> { rs => state.next(RGOA(Nil)) }
    ))("postings")


  val registry = new Registry

  LociDist.distribute(postings, registry)(Bindings.crdtDescriptions)


  def main(args: Array[String]): Unit = {
    dom.window.document.title = "Emergencity RSS Reader"
    dom.document.body = index.gen(postings).render
    var connecting = false
    scala.scalajs.js.timers.setInterval(1000) {
      if (!connecting && !registry.remotes.exists(_.connected)) {
        connecting = true
        lociConnect().onComplete { res =>
          scribe.trace(s"loci connection try finished: $res")
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
    scribe.debug(s"connecting loci to $wsUri …")
    connection.foreach { remote =>
      connectionSignal.set(true)
      remote.disconnected.foreach { _ =>
        connectionSignal.set(false)
        scribe.debug(s"loci reconnect")
      }
    }
    connection
  }

}
