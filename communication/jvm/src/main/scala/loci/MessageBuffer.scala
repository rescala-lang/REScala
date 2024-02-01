package loci

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import scala.annotation.compileTimeOnly
import scala.collection.mutable

final class MessageBuffer private (val backingArray: Array[Byte])
    extends mutable.IndexedSeq[Byte] {
  @compileTimeOnly("`backingArrayBuffer` only available in JS")
  def backingArrayBuffer: Any = ???

  @inline def length: Int = backingArray.length

  @inline def apply(index: Int) = {
    if (index < 0  || index >= length)
      throw new IndexOutOfBoundsException(s"index $index")

    backingArray(index)
  }

  @inline def update(index: Int, element: Byte) = {
    if (index < 0  || index >= length)
      throw new IndexOutOfBoundsException(s"index $index")

    backingArray(index) = element
  }

  @inline def update(offset: Int, buffer: MessageBuffer, bufferOffset: Int, count: Int) = {
    if (offset < 0 || bufferOffset < 0 || count < 0 ||
        offset > length - count || bufferOffset > buffer.length - count)
      throw new IndexOutOfBoundsException(
        s"offset $offset, length $length, " +
        s"buffer offset ${bufferOffset}, buffer length ${buffer.length}, count $count")

    System.arraycopy(buffer.backingArray, bufferOffset, backingArray, offset, count)
  }

  @inline def concat(buffer: MessageBuffer): MessageBuffer = {
    val array = new Array[Byte](length + buffer.length)
    System.arraycopy(backingArray, 0, array, 0, length)
    System.arraycopy(buffer.backingArray, 0, array, length, buffer.length)
    new MessageBuffer(array)
  }

  @inline def copy(offset: Int, count: Int): MessageBuffer = {
    if (offset < 0 || count < 0 || offset > length - count)
      throw new IndexOutOfBoundsException(s"offset $offset, count $count, length $length")

    val array = new Array[Byte](count)
    System.arraycopy(backingArray, offset, array, 0, count)
    new MessageBuffer(array)
  }

  @inline def decodeString(offset: Int, count: Int): String =
    new String(backingArray, offset, count, StandardCharsets.UTF_8)

  @inline def decodeString: String =
    decodeString(0, length)

  @inline def asByteBuffer: ByteBuffer =
    ByteBuffer.wrap(backingArray)

  override def toString: String =
    utility.MessageBufferEncoding.byteBufferToString(asByteBuffer, 0, length, fatal = true) getOrElse
      utility.MessageBufferEncoding.messageBufferToHexString(this)
}

object MessageBuffer {
  def empty: MessageBuffer = new MessageBuffer(Array.emptyByteArray)

  def allocate(length: Int): MessageBuffer = new MessageBuffer(new Array(length))

  def encodeString(string: String): MessageBuffer =
    new MessageBuffer(string.getBytes(StandardCharsets.UTF_8))

  def wrapByteBuffer(buffer: ByteBuffer): MessageBuffer =
    if (!buffer.hasArray) {
      val duplicate = buffer.duplicate()
      duplicate.position(0)
      duplicate.limit(buffer.capacity)
      val array = new Array[Byte](duplicate.remaining)
      duplicate.get(array)
      new MessageBuffer(array)
    }
    else
      new MessageBuffer(buffer.array())

  def wrapArray(array: Array[Byte]): MessageBuffer =
    new MessageBuffer(array)

  @compileTimeOnly("`wrapArrayBuffer` only available in JS")
  def wrapArrayBuffer(arrayBuffer: Any): MessageBuffer = ???
}
