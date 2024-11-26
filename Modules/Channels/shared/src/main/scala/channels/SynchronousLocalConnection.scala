package channels

import de.rmgk.delay.{Async, Callback, Promise, Sync}

/** Allows establishing direct synchronous connections between a single “server” and multiple “clients”.
  *
  * Synchronous here means that any `send` calls immediately execute the handler code no the receiving side (after a couple of callback indirections). There is no runtime and no threads and the send call will only return after the handling code has completed.
  *
  * It does not matter if the server or client is started first, connection is established immediately when the second one joins the connection.
  */
// You like callback hell? Definitely callback hell.
class SynchronousLocalConnection[T] {

  /** The server prepares by fullfillling the [[connectionEstablished]] promise, which contains a callback that allwows any number of clients to connect. The inner callback contains the [[Connection]] the server sends on, as well as a promise that the server completes immediately with it’s own receive handler. */
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

  /** Clients create both the client side and server side connection object. */
  def client(id: String): LatentConnection[T] = new LatentConnection[T] {

    /** This promise is send (unfullfilled) to the server, to be completed with the callback that handles received messages on the server side.
      * Thus, once completed, the inner callback directly executes whatever handler code was passed to the server.
      */
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

      /* This is the connection that is passed to the server, which just calls the callback defined by the handler. */
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

}
