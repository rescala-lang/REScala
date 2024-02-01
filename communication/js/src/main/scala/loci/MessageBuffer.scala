package loci

import java.nio.ByteBuffer
import java.nio.charset.CharacterCodingException

import scala.annotation.compileTimeOnly
import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.Dynamic._
import scala.scalajs.js.{JavaScriptException, TypeError}
import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array, TypedArrayBuffer, Uint8Array}
import scala.util.{Failure, Success, Try}

final class MessageBuffer private (val backingArrayBuffer: ArrayBuffer)
    extends mutable.IndexedSeq[Byte] {
  @compileTimeOnly("`backingArray` only available on the JVM")
  def backingArray: Array[Byte] = ???

  @inline def length = backingArrayBuffer.byteLength

  @inline def apply(index: Int) = {
    if (index < 0  || index >= length)
      throw new IndexOutOfBoundsException(s"index $index")

    array(index)
  }

  @inline def update(index: Int, element: Byte) = {
    if (index < 0  || index >= length)
      throw new IndexOutOfBoundsException(s"index $index")

    array(index) = element
  }

  @inline def update(offset: Int, buffer: MessageBuffer, bufferOffset: Int, count: Int) = {
    if (offset < 0 || bufferOffset < 0 || count < 0 ||
      offset > length - count || bufferOffset > buffer.length - count)
      throw new IndexOutOfBoundsException(
        s"offset $offset, length $length, " +
        s"buffer offset ${bufferOffset}, buffer length ${buffer.length}, count $count")

    array.set(new Int8Array(buffer.backingArrayBuffer, bufferOffset, count), offset)
  }

  @inline def concat(buffer: MessageBuffer): MessageBuffer = {
    val bufferArray = new Int8Array(length + buffer.length)
    bufferArray.set(array, 0)
    bufferArray.set(buffer.array, length)
    new MessageBuffer(bufferArray.buffer)
  }

  @inline def copy(offset: Int, count: Int): MessageBuffer = {
    if (offset < 0 || count < 0 || offset > length - count)
      throw new IndexOutOfBoundsException(s"offset $offset, count $count, length $length")

    new MessageBuffer(backingArrayBuffer.slice(offset, offset + count))
  }

  @inline def decodeString(offset: Int, count: Int): String = {
    if (offset < 0 || count < 0 || offset > length - count)
      throw new IndexOutOfBoundsException(s"offset $offset, count $count, length $length")

    byteBufferToString(offset, count, fatal = false).get
  }

  @inline def decodeString: String =
    decodeString(0, length)

  @inline def asByteBuffer: ByteBuffer =
    TypedArrayBuffer wrap backingArrayBuffer

  override def toString: String =
    byteBufferToString(0, length, fatal = true) getOrElse
      utility.MessageBufferEncoding.messageBufferToHexString(this)

  private val array = new Int8Array(backingArrayBuffer)

  private def byteBufferToString(offset: Int, count: Int, fatal: Boolean): Try[String] =
    if (count == 0)
      Success("")
    else if ((js typeOf global.TextDecoder) != "undefined") {
      val decoder = newInstance(global.TextDecoder)("utf-8", literal(fatal = fatal))
      try Success((decoder decode new Uint8Array(backingArrayBuffer, offset, count)).asInstanceOf[String])
      catch {
        case exception @ JavaScriptException(error: TypeError) =>
          Failure(
            new CharacterCodingException {
              override def getMessage: String = error.message
            } initCause exception)
      }
    }
    else
      utility.MessageBufferEncoding.byteBufferToString(asByteBuffer, offset, count, fatal = fatal)
}

object MessageBuffer {
  def empty: MessageBuffer = new MessageBuffer(new ArrayBuffer(0))

  def allocate(length: Int): MessageBuffer = new MessageBuffer(new ArrayBuffer(length))

  @compileTimeOnly("`wrapArray` only available on the JVM")
  def wrapArray(array: Array[Byte]): MessageBuffer = ???

  def encodeString(string: String): MessageBuffer =
    if (string.isEmpty)
      empty
    else if ((js typeOf global.TextEncoder) != "undefined") {
      val encoder = newInstance(global.TextEncoder)()
      new MessageBuffer((encoder encode string).asInstanceOf[Uint8Array].buffer)
    }
    else {
      val byteBuffer = utility.MessageBufferEncoding.stringToByteBuffer(string) {
        TypedArrayBuffer wrap new ArrayBuffer(_)
      }
      new MessageBuffer(byteBuffer.arrayBuffer().slice(0, byteBuffer.position))
    }

  def wrapByteBuffer(buffer: ByteBuffer): MessageBuffer =
    if (!buffer.hasArrayBuffer()) {
      val duplicate = buffer.duplicate()
      duplicate.position(0)
      duplicate.limit(buffer.capacity)
      var pos = duplicate.remaining
      val bufferArray = new Int8Array(pos)
      while (pos > 0) {
        pos -= 1
        bufferArray(pos) = buffer.get(pos)
      }
      new MessageBuffer(bufferArray.buffer)
    }
    else
      new MessageBuffer(buffer.arrayBuffer())

  def wrapArrayBuffer(arrayBuffer: ArrayBuffer): MessageBuffer =
    new MessageBuffer(arrayBuffer)
}
