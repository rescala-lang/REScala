package channels

import de.rmgk.delay.{Async, Callback}

import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, IOException, InputStream, OutputStream}

class SendingClosedException extends IOException

class JioInputStreamAdapter(in: InputStream) {
  val inputStream = new DataInputStream(new BufferedInputStream(in))

  def readNext(): MessageBuffer = {
    val size = inputStream.readInt()

    val bytes = new Array[Byte](size)
    inputStream.readFully(bytes, 0, size)

    ArrayMessageBuffer(bytes)

  }

  def loopReceive(handler: Callback[MessageBuffer]): Unit = {
    try
      while true do
        handler.succeed(readNext())
    catch
      case ioe: IOException =>
        handler.fail(ioe)
  }

}

class JioOutputStreamAdapter(out: OutputStream) {

  val outputStream = new DataOutputStream(new BufferedOutputStream(out))

  def send(data: MessageBuffer): Unit = {
    val outArray = data.asArray
    outputStream.writeInt(outArray.size)
    outputStream.write(outArray)
    outputStream.flush()
  }
}

class JIOStreamConnection(in: InputStream, out: OutputStream, doClose: () => Unit)
    extends Connection[MessageBuffer] {

  // socket streams

  val inputStream  = JioInputStreamAdapter(in)
  val outputStream = JioOutputStreamAdapter(out)

  // connection interface

  def send(data: MessageBuffer): Async[Any, Unit] = Async {
    println(s"sending data on jio stream")
    outputStream.send(data)
  }

  def close(): Unit = doClose()

  // frame parsing

  def loopHandler(handler: Handler[MessageBuffer]) =
    inputStream.loopReceive(handler.getCallbackFor(this))

}
