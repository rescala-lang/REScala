package benchmarks.encrdt

import benchmarks.encrdt.todolist.ToDoEntry
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.base.{Id, Time}
import kofre.base.Id.asId
import kofre.time.{ArrayRanges, Dots, Dot}
import kofre.encrdt.crdts.{AddWinsLastWriterWinsMap, DeltaAddWinsLastWriterWinsMap}

import java.util.UUID

object Codecs {
  implicit val idCodec: JsonValueCodec[Id] = JsonCodecMaker.make[String].asInstanceOf
  implicit val idKeyCodec: JsonKeyCodec[kofre.base.Id] = new JsonKeyCodec[Id]:
    override def decodeKey(in: JsonReader): Id = Id.predefined(in.readKeyAsString())
    override def encodeKey(x: Id, out: JsonWriter): Unit = out.writeKey(Id.unwrap(x))
  implicit val awlwwmapJsonCodec: JsonValueCodec[AddWinsLastWriterWinsMap.LatticeType[String, String]] =
    JsonCodecMaker.make(CodecMakerConfig.withSetMaxInsertNumber(Int.MaxValue).withMapMaxInsertNumber(Int.MaxValue))

  implicit val dotSetCodec: JsonValueCodec[Dots] = new JsonValueCodec[Dots] {
    private val optimizedArrayCausalContextCodec: JsonValueCodec[Map[Id, Array[Time]]] = JsonCodecMaker.make

    override def decodeValue(in: JsonReader, default: Dots): Dots =
      Dots(optimizedArrayCausalContextCodec.decodeValue(in, Map.empty).map {
        case (id, times) => id -> new ArrayRanges(times, times.length)
      })

    override def encodeValue(x: Dots, out: JsonWriter): Unit = optimizedArrayCausalContextCodec.encodeValue(
      x.internal.map { case (id, ranges) =>
        id -> {
          if (ranges.used == ranges.inner.length) ranges.inner
          else Array.copyOf(ranges.inner, ranges.used)
        }
      },
      out
    )

    override def nullValue: Dots = Dots.empty
  }

  implicit val deltaAwlwwmapJsonCodec: JsonValueCodec[DeltaAddWinsLastWriterWinsMap.StateType[String, String]] =
    JsonCodecMaker.make(CodecMakerConfig.withSetMaxInsertNumber(Int.MaxValue).withMapMaxInsertNumber(Int.MaxValue))

  implicit val lamportClockKeyCodec: JsonKeyCodec[Dot] = new JsonKeyCodec[Dot] {
    override def decodeKey(in: JsonReader): Dot = {
      val inputString = in.readKeyAsString()
      val index       = inputString.indexOf('@')
      Dot(inputString.substring(index + 1).asId, inputString.substring(0, index).toLong)
    }

    override def encodeKey(x: Dot, out: JsonWriter): Unit = out.writeKey(s"${x.time}@${x.replicaId}")
  }

  implicit val toDoMapCodec: JsonValueCodec[DeltaAddWinsLastWriterWinsMap.StateType[UUID, ToDoEntry]] =
    JsonCodecMaker.make(CodecMakerConfig.withSetMaxInsertNumber(Int.MaxValue).withMapMaxInsertNumber(Int.MaxValue))
}
