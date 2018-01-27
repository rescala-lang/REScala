package rescala.restoration

import io.circe.{Decoder, Encoder}
import rescala.core.{Pulse, ReSerializable}

import scala.util.{Failure, Left, Right, Success, Try}

object ReCirce {
  // build in method exists for scala 2.12, but not for 2.11
  def eitherToTry[A](e: Either[Throwable, A]): Try[A] = e match {
    case Right(b) => Success(b)
    case Left(a)  => Failure(a)
  }
  implicit def recirce[T: Encoder : Decoder]: ReSerializable[T] = new ReSerializable[T] {
    override def serialize(value: T): String = implicitly[Encoder[T]].apply(value).noSpaces
    override def deserialize(value: String): Try[T] = eitherToTry(implicitly[Decoder[T]].decodeJson(io.circe.parser.parse(value).right.get))
  }


  def pulseEncoder[T: Encoder](): Encoder[Pulse[T]] = io.circe.Encoder.encodeOption[T].contramap(_.toOption)
  def pulseDecoder[T: Decoder](): Decoder[Pulse[T]] = io.circe.Decoder.decodeOption[T].map(Pulse.fromOption)
}
