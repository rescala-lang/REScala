package rescala.fullmv.transmitter

import io.circe._
import io.circe.parser._
import io.circe.syntax._

import loci.MessageBuffer
import loci.transmitter.Serializable

import scala.util.{Failure, Success, Try}

object CirceSerialization {
  implicit def circeBasedSerializable[T](implicit enc: Encoder[T], dec: Decoder[T]): Serializable[T] = new Serializable[T] {
    override def serialize(value: T): MessageBuffer = MessageBuffer.fromString(value.asJson.noSpaces)
    override def deserialize(value: MessageBuffer): Try[T] = decode[T](value.toString(0, value.length)) match {
      case Left(error) => Failure(error)
      case Right(res) => Success(res)
    }
  }
}
