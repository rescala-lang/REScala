package loci
package serializer

import scala.util.Try
import java.nio.ByteBuffer

import com.github.plokhotnyuk.jsoniter_scala.core._

object jsoniterScala {
  implicit def jsoniteScalaBasedSerializable[T]
  (implicit codec: JsonValueCodec[T]): Serializable[T] = new Serializable[T] {
    def serialize(value: T): MessageBuffer = {
      val bytes = writeToArray(value)
      MessageBuffer.wrapByteBuffer(ByteBuffer.wrap(bytes))
    }
    def deserialize(value: MessageBuffer): Try[T] =
      Try {readFromByteBuffer(value.asByteBuffer)}
  }
}
