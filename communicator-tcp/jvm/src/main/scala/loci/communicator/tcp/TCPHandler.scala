package loci
package communicator
package tcp

import java.io.{BufferedInputStream, BufferedOutputStream, IOException}
import java.net.{Socket, SocketException}
import java.util.concurrent.{Executors, ScheduledFuture, ThreadFactory, TimeUnit}

import scala.collection.mutable

private object TCPHandler {
  locally(TCPHandler)

  private val executor = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
    override def newThread(runnable: Runnable) = {
      val thread = Executors.defaultThreadFactory.newThread(runnable)
      thread.setDaemon(true)
      thread
    }
  })

  def handleConnection(
      socket: Socket,
      properties: TCP.Properties,
      connectionSetup: ConnectionSetup[TCP],
      connectionEstablished: Connection[TCP] => Unit) = {

    // enable/disable Nagle's algorithm

    try socket.setTcpNoDelay(properties.noDelay) catch {
      case _: SocketException =>
        // some implementations may not allow TCP_NODELAY to be set, in which
        // case we just ignore the issue (there are, however, performance
        // implications for certain communication patterns)
    }


    // socket streams

    val inputStream = new BufferedInputStream(socket.getInputStream)
    val outputStream = new BufferedOutputStream(socket.getOutputStream)


    // heartbeat

    val delay = properties.heartbeatDelay.toMillis
    val timeout = properties.heartbeatTimeout.toMillis.toInt
    var heartbeatTask: ScheduledFuture[_] = null

    // control codes

    val head: Byte = 1
    val payload: Byte = 2
    val heartbeat: Byte = 6


    // connection interface

    var isOpen = true
    val doClosed = Notice.Steady[Unit]
    val doReceive = Notice.Stream[MessageBuffer]

    val connection = new Connection[TCP] {
      val protocol = new TCP {
        val host = socket.getInetAddress.getHostName
        val port = socket.getPort
        val setup = connectionSetup
        val authenticated = false
        val encrypted = false
        val integrityProtected = false
      }

      val closed = doClosed.notice
      val receive = doReceive.notice

      def open: Boolean = synchronized { isOpen }

      def send(data: MessageBuffer) = synchronized {
        if (isOpen)
          try {
            val size = data.length
            outputStream.write(
              Array(
                head,
                (size >> 24).toByte,
                (size >> 16).toByte,
                (size >> 8).toByte,
                size.toByte,
                payload))
            outputStream.write(data.backingArray)
            outputStream.flush()
          }
          catch { case _: IOException => close() }
        }

      def close() = {
        synchronized {
          if (isOpen) {
            def ignoreIOException(body: => Unit) =
              try body catch { case _: IOException => }

            isOpen = false

            if (heartbeatTask != null)
              heartbeatTask.cancel(true)

            ignoreIOException { socket.shutdownOutput() }
            ignoreIOException { while (inputStream.read != -1) { } }
            ignoreIOException { socket.close() }
          }
        }

        doClosed.trySet()
      }
    }

    // heartbeat

    socket.setSoTimeout(timeout)

    heartbeatTask = executor.scheduleWithFixedDelay(new Runnable {
      def run() = connection synchronized {
        if (isOpen)
          try {
            outputStream.write(heartbeat)
            outputStream.flush()
          }
          catch { case _: IOException => connection.close() }
      }
    }, delay, delay, TimeUnit.MILLISECONDS)


    connectionEstablished(connection)


    // frame parsing

    def read = {
      val byte = inputStream.read
      if (byte == -1) throw new IOException("end of connection stream")
      else byte.toByte
    }

    val arrayBuilder = mutable.ArrayBuilder.make[Byte]

    try while (true) {
      read match {
        case `heartbeat` =>
          // heartbeat

        case `head` =>
          var size =
            ((read & 0xff) << 24) |
            ((read & 0xff) << 16) |
            ((read & 0xff) << 8) |
            (read & 0xff)

          if (read == payload && size >= 0) {
            arrayBuilder.clear()
            arrayBuilder.sizeHint(size)
            while (size > 0) {
              arrayBuilder += read
              size -= 1
            }

            doReceive.fire(MessageBuffer wrapArray arrayBuilder.result())
          }
          else
            connection.close()

        case _ =>
          connection.close()
      }
    }
    catch {
      case _: IOException =>
        connection.close()
    }
  }
}
