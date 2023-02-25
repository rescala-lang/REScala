package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rescala.core.{ReInfo, ReSource, Tracing}
import rescala.structure.Pulse

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

  def debugPrinter(x: Any): String = x match
    case v: String                                => s"\"$v\""
    case v: (Boolean | Char | Short | Int | Long) => v.toString
    case o: Option[Any]                           => o.map(debugPrinter).toString
    case s: (Seq[Any])                            => s.iterator.map(debugPrinter).mkString(s"List(", ", ", ")")
    case m: scalatags.generic.Modifier[_]         => "<some html>"
    case p: Pulse[Any]                            => p.map(debugPrinter).getOrElse("")
    case p: Product => Range(0, p.productArity).map(n =>
        s"${p.productElementName(n)} = ${debugPrinter(p.productElement(n))}"
      ).mkString(s"${p.getClass.getName}(", ", ", ")")
    case other => s"<unknown: ${other.getClass}>"

  given JsonValueCodec[Tracing.ValueWrapper] = new JsonValueCodec[Tracing.ValueWrapper]:
    override def decodeValue(in: JsonReader, default: Tracing.ValueWrapper): Tracing.ValueWrapper =
      throw IllegalStateException("deserialization not supported")
    override def encodeValue(x: Tracing.ValueWrapper, out: JsonWriter): Unit =
      out.writeVal(debugPrinter(x.v))

    override def nullValue: Tracing.ValueWrapper = null
  given dataCodec: JsonValueCodec[Tracing.Data] = JsonCodecMaker.make

  @JSExport
  def setListener(obs: scalajs.js.Function1[Any, Unit]): Unit = {
    println(s"setting listener")
    Tracing.observer = {
      data => obs(writeToString(data))
    }
  }

}
