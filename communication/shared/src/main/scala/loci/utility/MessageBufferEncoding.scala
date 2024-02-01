package loci
package utility

import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.{ByteBuffer, CharBuffer}

import scala.util.{Failure, Success, Try}

object MessageBufferEncoding {
  def byteBufferToString(byteBuffer: ByteBuffer, offset: Int, count: Int, fatal: Boolean): Try[String] = {
    val decoder = StandardCharsets.UTF_8.newDecoder()

    if (!fatal)
      decoder
        .onMalformedInput(CodingErrorAction.REPLACE)
        .onUnmappableCharacter(CodingErrorAction.REPLACE)

    val size = (count * decoder.maxCharsPerByte.toDouble).toInt
    val array = new Array[Char](size)
    val charBuffer = CharBuffer.wrap(array)

    byteBuffer.position(offset)
    byteBuffer.limit(offset + count)

    try {
      var result = decoder.decode(byteBuffer, charBuffer, true)

      if (!result.isUnderflow)
        result.throwException()
      result = decoder.flush(charBuffer)
      if (!result.isUnderflow)
        result.throwException()

      Success(new String(array, 0, charBuffer.position()))
    }
    catch {
      case exception: CharacterCodingException =>
        Failure(exception)
    }
  }

  def stringToByteBuffer(string: String)(allocateByteBuffer: Int => ByteBuffer): ByteBuffer = {
    val encoder = StandardCharsets.UTF_8.newEncoder()
      .onMalformedInput(CodingErrorAction.REPLACE)
      .onUnmappableCharacter(CodingErrorAction.REPLACE)

    val size = (string.length * encoder.maxBytesPerChar.toDouble).toInt
    val byteBuffer = allocateByteBuffer(size)
    val charBuffer = CharBuffer.wrap(string)

    byteBuffer.position(0)
    byteBuffer.limit(size)

    var result = encoder.encode(charBuffer, byteBuffer, true)

    if (!result.isUnderflow)
      result.throwException()
    result = encoder.flush(byteBuffer)
    if (!result.isUnderflow)
      result.throwException()

    byteBuffer
  }

  private val hex = Array(
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')

  def messageBufferToHexString(buffer: MessageBuffer): String = {
    val result = new Array[Char](3 * buffer.length)
    var i = 0
    var j = 0
    while (i < buffer.length) {
      result(j) = hex((buffer(i) & 0xF0) >> 4)
      j += 1
      result(j) = hex(buffer(i) & 0x0F)
      j += 1
      result(j) = ' '
      j += 1
      i += 1
    }

    if (result.length == 0)
      ""
    else
      new String(result, 0, result.length - 1)
  }
}
