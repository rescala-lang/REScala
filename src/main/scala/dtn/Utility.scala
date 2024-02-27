package dtn

import java.nio.ByteBuffer

object Utility {
  def toByteArray(buffer: ByteBuffer): Array[Byte] = {
    val bytes = new Array[Byte](buffer.remaining)
    buffer.get(bytes)
    bytes
  }

  def toByteBuffer(array: Array[Byte]): ByteBuffer = {
    ByteBuffer.wrap(array)
  }
}
