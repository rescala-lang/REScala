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
import scalatags.JsDom.tags.{body, h1, p, table, form, span}
import replication.JsoniterCodecs.given
import scalatags.JsDom.tags2.{aside, article}

import scala.annotation.nowarn
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
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

  @JSExportTopLevel("Replication")
  def run() = main(Array.empty)

  def main(args: Array[String]): Unit = {
    dom.document.body = body("loading data …").render

    val registry = new Registry

    @nowarn given JsonValueCodec[ReplicatedList[String]] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    val dm = new DataManager[ReplicatedList[String]](replicaId, registry)

    dm.disseminate()

    val ccm = new ContentConnectionManager(registry)

    val bodySig = Signal {
      body(
        id := "index",
        HTML.meta(ccm, dm),
        HTML.managedData(dm),
        article(
          dm.changes.map(_ => dm.currentValue).latest(dm.currentValue).map { cv => span(cv.toList.toString) }.asModifier
        )
      )
    }

    val bodyParent = dom.document.body.parentElement
    bodyParent.removeChild(dom.document.body)
    bodySig.asModifier.applyTo(bodyParent)

  }
}
