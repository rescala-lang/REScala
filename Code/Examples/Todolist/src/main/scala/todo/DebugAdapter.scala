package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rescala.core.{ReSource, Tracing}

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("DebugAdapter")
object DebugAdapter {

  implicit val resourceCodec: JsonValueCodec[ReSource] = {
    new JsonValueCodec[ReSource]:
      override def decodeValue(in: JsonReader, default: ReSource): ReSource =
        throw IllegalStateException("deserialization not supported")
      override def encodeValue(x: ReSource, out: JsonWriter): Unit = out.writeVal(x.info.description)
      override def nullValue: ReSource                             = null
  }
  implicit val dataCodec: JsonValueCodec[Tracing.Data] = JsonCodecMaker.make

  @JSExport
  def setListener(obs: scalajs.js.Function1[String, Unit]): Unit = {
    println(s"setting listener")
    Tracing.observer = data => {
      obs(writeToString(data))
    }
  }

}
