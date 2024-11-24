package channels

import de.rmgk.delay.{Async, Callback, Promise, Sync}

/** Allows establishing a single direct synchronous connection. */
// You like callback hell? Definitely callback hell.
class SynchronousLocalConnection[T] {
  def client(id: String): LatentConnection[T] = new LatentConnection[T] {

    val toServerMessages: Promise[Callback[T]] = Promise()

    object toServer extends Connection[T] {
      def send(msg: T): Async[Any, Unit] = Async {
        val cb = toServerMessages.async.bind
        cb.succeed(msg)
      }
      override def close(): Unit = ()

      override def toString: String = s"From[$id]"
    }

    def prepare(incomingHandler: Handler[T]): Async[Abort, Connection[T]] = Async {
      val callback = incomingHandler.getCallbackFor(toServer)

      val toClient = new Connection[T] {
        override def close(): Unit = ()
        override def send(message: T): Async[Any, Unit] = Sync {
          callback.succeed(message)
        }
        override def toString: String = s"To[$id]"
      }

      val established = server.connectionEstablished.async.bind
      established.succeed(server.Establish(toClient, toServerMessages))
      toServer
    }
  }

  object server extends LatentConnection[T] {

    case class Establish(serverSendsOn: Connection[T], clientConnectionSendsTo: Promise[Callback[T]])
    val connectionEstablished: Promise[Callback[Establish]] = Promise()

    def prepare(incomingHandler: Handler[T]): Async[Abort, Connection[T]] = Async.fromCallback[Establish] {
      connectionEstablished.succeed(Async.handler)
    }.map { connChan =>
      connChan.clientConnectionSendsTo.succeed(incomingHandler.getCallbackFor(connChan.serverSendsOn))
      connChan.serverSendsOn
    }
  }

}
