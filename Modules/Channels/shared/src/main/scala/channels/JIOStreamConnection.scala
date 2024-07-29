package channels

import de.rmgk.delay.{Async, Callback}

import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, IOException, InputStream, OutputStream}

class JIOStreamConnection(in: InputStream, out: OutputStream, doClose: () => Unit) extends Connection[MessageBuffer] {

  // socket streams

  val inputStream  = new DataInputStream(new BufferedInputStream(in))
  val outputStream = new DataOutputStream(new BufferedOutputStream(out))

  // control codes

  val sizedContent: Byte = 1

  // connection interface

  def send(data: MessageBuffer): Async[Any, Unit] = Async.fromCallback {
    println(s"sending data on tcp socket")
    try {
      val outArray = data.asArray
      outputStream.write(sizedContent)
      outputStream.writeInt(outArray.size)
      outputStream.write(outArray)
      outputStream.flush()
      println(s"sending done")
      Async.handler.succeed(())
    } catch {
      case ioe: IOException => Async.handler.fail(ioe)
    }
  }

  def close(): Unit = doClose()

  // frame parsing

  def loopHandler(handler: Handler[MessageBuffer]) =
    handleReceivedMessages(handler.getCallbackFor(this))

  def handleReceivedMessages(handler: Callback[MessageBuffer]) = {

    def readNextByte() = {
      val byte = inputStream.read
      if byte == -1
      then throw new IOException("end of connection stream")
      else byte.toByte
    }

    try while true do {
        println(s"receiving messages")
        readNextByte() match {
          case `sizedContent` =>
            val size = inputStream.readInt()

            val bytes = new Array[Byte](size)
            inputStream.readFully(bytes, 0, size)

            println(s"received message buffer of size $size")

            handler.succeed(ArrayMessageBuffer(bytes))

          case _ =>
            throw IOException("unexpected read")
        }
      }
    catch {
      case ioe: IOException =>
        doClose()
        handler.fail(ioe)
    }
  }

}
