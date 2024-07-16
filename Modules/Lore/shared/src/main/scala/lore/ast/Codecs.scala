package lore.ast

import cats.data.NonEmptyList
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}

import java.nio.file.Path
import scala.util.{Failure, Success, Try}

object Codecs {

  given termListC: JsonValueCodec[List[Term]] = JsonCodecMaker.make

  given nelCodec[A](using
      listCodec: JsonValueCodec[List[A]]
  ): JsonValueCodec[NonEmptyList[A]] with {
    def decodeValue(in: JsonReader, default: NonEmptyList[A]) =
      NonEmptyList.fromList(listCodec.decodeValue(in, List())) match {
        case Some(value) => value
        case None        => default
      }

    def encodeValue(l: NonEmptyList[A], out: JsonWriter) =
      listCodec.encodeValue(l.toList, out)

    override def nullValue: NonEmptyList[A] = null
  }

  given JsonValueCodec[List[Type]] = JsonCodecMaker.make(
    CodecMakerConfig
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withCirceLikeObjectEncoding(true)
  )
  given typeNeListC: JsonValueCodec[NonEmptyList[Type]] = nelCodec[Type]
  given targTListC: JsonValueCodec[List[TArgT]] = JsonCodecMaker.make(
    CodecMakerConfig
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withCirceLikeObjectEncoding(true)
  )
  given JsonValueCodec[NonEmptyList[TArgT]] = nelCodec[TArgT]

  given JsonValueCodec[Path] with {
    def decodeValue(in: JsonReader, default: Path) = Try(
      Path.of(in.readString(""))
    ) match {
      case Failure(exception) => default
      case Success(value)     => value
    }

    def encodeValue(p: Path, out: JsonWriter) = out.writeVal(p.toString)

    override def nullValue: Path = null
  }

  given JsonValueCodec[Term] = JsonCodecMaker.make(
    CodecMakerConfig
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withCirceLikeObjectEncoding(true)
  )
}
