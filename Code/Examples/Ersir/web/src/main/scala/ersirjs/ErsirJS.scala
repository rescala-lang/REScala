package ersirjs

import ersir.shared._
import loci.communicator.ws.akka.WS
import loci.registry.Registry
import loci.transmitter.RemoteRef
import org.scalajs.dom
import rescala.default._
import rescala.extra.distributables.LociDist
import rescala.extra.lattices.delta.CContext.DietMapCContext
import rescala.extra.lattices.delta.Delta
import rescala.extra.lattices.delta.impl.reactive.RGA
import rescala.extra.lattices.delta.crdt.RGACRDT._

import java.util.concurrent.ThreadLocalRandom
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object ErsirJS {

  type Postings = RGA[Posting, DietMapCContext]

  val connectionSignal: Var[Boolean] = Var(false)

  val connectClass: Signal[String] = Signal {
    val internet = connectionSignal.value
    if (internet) "hideConnectionIssues" else "showConnectionIssues"
  }

  val index = new Index(connectClass)

  val addPost: Event[Posting] = index.addPost.event
    .filter(p => p.title.nonEmpty || p.img.nonEmpty)

  val deltaEvt: Evt[Delta[RGA.State[Posting, DietMapCContext]]] = Evt()

  val myID: String = ThreadLocalRandom.current().nextLong().toHexString

  val postings: Signal[Postings] =
    Events.foldAll(RGA[Posting, DietMapCContext](myID)) { rga =>
      Seq(
        addPost act rga.prepend,
        index.reset.event act { _ => rga.clear() },
        deltaEvt act rga.applyDelta
      )
    }

  val registry = new Registry

  LociDist.distributeDeltaCRDT(postings, deltaEvt, registry)(Bindings.crdtDescriptions)

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
