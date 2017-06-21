package rescala.core

import io.circe.{Decoder, Encoder}

import scala.annotation.implicitNotFound
import scala.util.{Success, Try}

@implicitNotFound("${T} is not serializable")
trait ReSerializable[T] {
  def serialize(value: T): String
  def deserialize(value: String): Try[T]
}

object ReSerializable {
  implicit def recirce[T: Encoder : io.circe.Decoder](): ReSerializable[T] = new ReSerializable[T] {
    override def serialize(value: T): String = implicitly[Encoder[T]].apply(value).noSpaces
    override def deserialize(value: String): Try[T] = implicitly[Decoder[T]].decodeJson(io.circe.parser.parse(value).right.get).toTry
  }

  implicit def pulseEncoder[T: Encoder](): Encoder[Pulse[T]] = io.circe.Encoder.encodeOption[T].contramap(_.toOption)
  implicit def pulseDecoder[T: Decoder](): Decoder[Pulse[T]] = io.circe.Decoder.decodeOption[T].map(Pulse.fromOption)

  implicit def pulseSerializable[T: ReSerializable](): ReSerializable[Pulse[T]] = new ReSerializable[Pulse[T]] {
    override def serialize(value: Pulse[T]): String = value.toOption.fold("")(implicitly[ReSerializable[T]].serialize)
    override def deserialize(value: String): Try[Pulse[T]] = if (value.isEmpty) Success(Pulse.empty) else implicitly[ReSerializable[T]].deserialize(value).map(Pulse.Value(_))
  }
}
