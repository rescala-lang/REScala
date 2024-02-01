package loci
package serializer

import io.circe._
import io.circe.parser._
import io.circe.syntax._

import scala.util.{Failure, Success, Try}

object circe {
  implicit def circeBasedSerializable[T]
      (implicit enc: Encoder[T], dec: Decoder[T]): Serializable[T] = new Serializable[T] {
    override def serialize(value: T) =
      MessageBuffer encodeString value.asJson.noSpaces
    override def deserialize(value: MessageBuffer) =
      Try { decode[T](value.decodeString) } match {
        case Success(Right(result)) => Success(result)
        case Success(Left(error)) => Failure(error)
        case Failure(exception) => Failure(exception)
      }
  }
}
