package rescala.core

import scala.annotation.implicitNotFound
import scala.util.Try

@implicitNotFound("${T} is not serializable")
trait ReSerializable[T] {
  def serialize(value: T): String
  def deserialize(value: String): Try[T]
}
