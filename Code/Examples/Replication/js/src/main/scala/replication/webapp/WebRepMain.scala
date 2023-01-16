package replication.webapp

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.base.{Id, Lattice}
import kofre.datatypes.ReplicatedList
import kofre.dotted.DottedLattice
import loci.registry.Registry
import org.scalajs.dom
import org.scalajs.dom.{Fetch, HttpMethod, RequestInit}
import replication.DataManager
import rescala.default.*
import rescala.extra.Tags.*
import scalatags.JsDom.attrs.id
import scalatags.JsDom.implicits.{stringAttr, stringFrag}
import scalatags.JsDom.tags.{body, h1, p}
import replication.JsoniterCodecs.given

import scala.annotation.nowarn
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

case class MetaInfo(
    connection: Signal[Int],
    reconnecting: Signal[Int]
)

object WebRepMain {

  val baseurl = ""

  def fetchbuffer(
      endpoint: String,
      method: HttpMethod = HttpMethod.GET,
      body: Option[String] = None
  ): Future[ArrayBuffer] = {

    val ri = js.Dynamic.literal(method = method).asInstanceOf[RequestInit]

    body.foreach { content =>
      ri.body = content
      ri.headers = js.Dictionary("Content-Type" -> "application/json;charset=utf-8")
    }

    Fetch.fetch(baseurl + endpoint, ri).toFuture
      .flatMap(_.arrayBuffer().toFuture)
  }

  val replicaId: Id = Id.gen()

  def main(args: Array[String]): Unit = {
    dom.document.body = body("loading data …").render

    val registry = new Registry

    @nowarn given JsonValueCodec[ReplicatedList[String]] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    val dm = new DataManager[ReplicatedList[String]](replicaId, registry)

    dm.disseminate()

    val ccm = new ContentConnectionManager(registry)
    ccm.autoreconnect()

    // ccm.remoteVersion.observe{v =>
    //  if (!(v == "unknown" || v.startsWith("error")) && v != viscel.shared.Version.str) {
    //    ServiceWorker.unregister().andThen(_ => dom.window.location.reload(true))
    //  }
    // }

    val meta =
      MetaInfo(ccm.connectionStatus, ccm.reconnecting)

    val bodySig = Signal {
      body(
        id := "index",
        Snippets.meta(meta).asModifier,
      )
    }

    val metaLoading = Snippets.meta(meta)

    val bodyParent = dom.document.body.parentElement
    bodyParent.removeChild(dom.document.body)
    bodySig.asModifier.applyTo(bodyParent)

  }
}
