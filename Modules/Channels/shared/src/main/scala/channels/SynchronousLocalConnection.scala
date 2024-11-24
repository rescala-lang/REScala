package channels

import de.rmgk.delay.{Async, Callback, Promise, Sync}

/** Allows establishing a single direct synchronous connection. */
class SynchronousLocalConnection[T] {
  object client extends LatentConnection[T] {

    object connection extends Connection[T] {
      def send(msg: T): Async[Any, Unit] = Async {
        val cb = server.incomingMessageCallback.async.bind
        cb.succeed(msg)
      }
      override def close(): Unit = ()
    }

    def prepare(incomingHandler: Handler[T]): Async[Abort, Connection[T]] = Async {
      val callback = incomingHandler.getCallbackFor(connection)

      val serverConn = new Connection[T] {
        override def close(): Unit = ()
        override def send(message: T): Async[Any, Unit] = Sync {
          callback.succeed(message)
        }
      }

      val established = server.connectionEstablished.async.bind
      established.succeed(serverConn)
      connection
    }
  }

  object server extends LatentConnection[T] {

    val incomingMessageCallback: Promise[Callback[T]] = Promise()

    val connectionEstablished: Promise[Callback[Connection[T]]] = Promise()

    def prepare(incomingHandler: Handler[T]): Async[Abort, Connection[T]] = Async.fromCallback[Connection[T]] {
      connectionEstablished.succeed(Async.handler)
    }.map { conn =>
      incomingMessageCallback.succeed(incomingHandler.getCallbackFor(conn))
      conn
    }
  }

}
