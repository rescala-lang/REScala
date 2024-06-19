package lofi_acl.sync

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReaderException, JsonValueCodec, readFromArray, writeToArray}

import java.io.{DataInputStream, DataOutputStream}

trait MessageSerialization[MSG]:
  def writeToStream(msg: MSG, outputStream: DataOutputStream): Unit
  def readFromStream(inputStream: DataInputStream): MSG

object MessageSerialization:
  inline def derived[MSG](using JsonValueCodec[MSG]): MessageSerialization[MSG] = new MessageSerialization[MSG]:
    override def writeToStream(msg: MSG, outputStream: DataOutputStream): Unit =
      val encodedMsg = writeToArray(msg)
      outputStream.writeInt(encodedMsg.length) // Length of encoded message is prepended
      outputStream.write(encodedMsg)

    override def readFromStream(inputStream: DataInputStream): MSG =
      val lengthOfMessage = inputStream.readInt() // Length of message is sent before message itself.
      val message         = inputStream.readNBytes(lengthOfMessage)
      readFromArray[MSG](message)

class InvalidMessageException(message: String = null) extends RuntimeException(message)
