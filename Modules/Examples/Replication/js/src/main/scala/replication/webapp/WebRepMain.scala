package replication.webapp

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import org.scalajs.dom
import org.scalajs.dom.{Fetch, HttpMethod, RequestInit}
import rdts.base.Uid
import rdts.datatypes.contextual.ReplicatedList
import reactives.default.*
import reactives.extra.Tags.*
import replication.JsoniterCodecs.given
import replication.fbdc.FbdcExampleData
import scalatags.JsDom.all.bindNode
import scalatags.JsDom.attrs.id
import scalatags.JsDom.implicits.{stringAttr, stringFrag}
import scalatags.JsDom.tags.{SeqFrag, body}
import scalatags.JsDom.tags2

import scala.annotation.nowarn
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.ArrayBuffer

import replication.JsoniterCodecs.given

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

  @JSExportTopLevel("Replication")
  def run() = main(Array.empty)

  def main(args: Array[String]): Unit = {
    dom.document.body = body("loading data …").render

    @nowarn given JsonValueCodec[ReplicatedList[String]] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    val exData = new FbdcExampleData()

    @nowarn given JsonValueCodec[exData.dataManager.CodecState] = JsonCodecMaker.make


    val ccm = new ContentConnectionManager(exData.dataManager.dataManager)

    val bodyParent = dom.document.body.parentElement
    bodyParent.removeChild(dom.document.body)
    bodyParent.appendChild(body(
      id := "index",
      tags2.main(
        HTML.providers(exData),
        HTML.connectionManagement(ccm, exData),
      )
    ).render)
    ()

  }
}
