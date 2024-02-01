package loci
package communicator
package ws.jetty

import org.eclipse.jetty.websocket.api.{Callback, Session}
import org.eclipse.jetty.websocket.api.Session.Listener

import java.nio.ByteBuffer
import java.util.concurrent.{Executors, ScheduledFuture, ThreadFactory, TimeUnit}
import scala.util.{Failure, Success}

private class Socket[P <: WS: WSProtocolFactory](val protocol: P, properties: WS.Properties)(
    connectionEstablished: Success[Connection[P]] => Unit,
    connectionFailed: Failure[Connection[P]] => Unit
) extends Listener.Abstract
    with Connection[P] {

  val doClosed = Notice.Steady[Unit]
  val doReceive = Notice.Stream[MessageBuffer]

  private val executor = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
    def newThread(runnable: Runnable) = {
      val thread = Executors.defaultThreadFactory.newThread(runnable)
      thread.setDaemon(true)
      thread
    }
  })

  private val timeout = properties.heartbeatTimeout.toMillis
  private val delay = properties.heartbeatDelay.toMillis
  private val heartbeat = "\uD83D\uDC93"

  private var heartbeatTask: ScheduledFuture[_] = _

  private var timeoutTask: ScheduledFuture[_] = _

  private def resetTimeout(): Unit = synchronized {
    if (timeoutTask != null)
      timeoutTask.cancel(true)

    timeoutTask = executor.schedule(
      new Runnable {
        def run(): Unit = Socket.this synchronized {
          getSession.close()
        }
      },
      timeout,
      TimeUnit.MILLISECONDS
    )
  }

  resetTimeout()

  override def onWebSocketOpen(session: Session): Unit = {
    synchronized {
      super.onWebSocketOpen(session)

      heartbeatTask = executor.scheduleWithFixedDelay(
        new Runnable {
          def run(): Unit = Socket.this synchronized {
            getSession.sendText(heartbeat, Callback.NOOP)
          }
        },
        delay,
        delay,
        TimeUnit.MILLISECONDS
      )
    }

    connectionEstablished(Success(this))

    resetTimeout()
    session.demand()
  }

  override def onWebSocketBinary(buffer: ByteBuffer, callback: Callback): Unit = {

    // MessageBuffer has a .wrapByteBuffer method, but that does not seem to work here, maybe because it adapts the indices?
    val data = new Array[Byte](buffer.remaining())
    buffer.get(data)

    doReceive.fire(MessageBuffer.wrapArray(data))

    resetTimeout()
    callback.succeed()
    getSession.demand()
  }

  override def onWebSocketText(message: String): Unit = {
    synchronized { super.onWebSocketText(message) }
    resetTimeout()
    getSession.demand()
  }

  override def onWebSocketClose(statusCode: Int, reason: String): Unit = {

    println(s"closing message because $reason")

    synchronized {
      heartbeatTask.cancel(true)
      timeoutTask.cancel(true)
    }

    doClosed.trySet()
  }

  override def onWebSocketError(cause: Throwable): Unit = {
    println(s"received expliit error $cause")
    connectionFailed(Failure(cause))
    close()
  }

  val closed = doClosed.notice
  val receive = doReceive.notice

  def open: Boolean = synchronized {
    val session = getSession
    session != null && session.isOpen
  }

  def send(data: MessageBuffer): Unit = synchronized {
    if (isOpen) {
      getSession.sendBinary(data.asByteBuffer, Callback.NOOP)
    }
  }

  def close(): Unit = {
    println(s"called explicit close")
    synchronized {
      val session = getSession
      if (session != null)
        session.close()
    }
    doClosed.trySet()
  }
}
