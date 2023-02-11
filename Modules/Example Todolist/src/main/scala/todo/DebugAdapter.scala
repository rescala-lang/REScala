package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rescala.core.{ReInfo, ReSource, Tracing}
import rescala.operator.Pulse

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("DebugAdapter")
object DebugAdapter {

  given infoCodec: JsonValueCodec[ReInfo] = JsonCodecMaker.make
  given resourceCodec: JsonValueCodec[ReSource] = {
    new JsonValueCodec[ReSource]:
      override def decodeValue(in: JsonReader, default: ReSource): ReSource =
        throw IllegalStateException("deserialization not supported")
      override def encodeValue(x: ReSource, out: JsonWriter): Unit = infoCodec.encodeValue(x.info, out)
      override def nullValue: ReSource                             = null
  }

  given JsonValueCodec[Tracing.ValueWrapper] = new JsonValueCodec[Tracing.ValueWrapper]:
    override def decodeValue(in: JsonReader, default: Tracing.ValueWrapper): Tracing.ValueWrapper =
      throw IllegalStateException("deserialization not supported")
    override def encodeValue(x: Tracing.ValueWrapper, out: JsonWriter): Unit =
      x.v.asInstanceOf[Pulse[_]].toOption match
        case Some(v: scalatags.generic.Modifier[_]) => out.writeVal(x.v.toString)
        case _                                      => out.writeVal("<some html>")

    override def nullValue: Tracing.ValueWrapper = null
  given dataCodec: JsonValueCodec[Tracing.Data] = JsonCodecMaker.make

  @JSExport
  def setListener(obs: scalajs.js.Function1[Any, Unit]): Unit = {
    println(s"setting listener")
    Tracing.observer = {
      data =>
        obs(writeToString(data))
    }
  }

}
