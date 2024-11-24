package replication.example

import channels.{Abort, Connection, Handler, LatentConnection}
import de.rmgk.delay.{Async, Callback, Sync}
import rdts.base.LocalUid
import replication.{DeltaDissemination, ProtocolMessage}

class SynchronizedConnection[T] {
  object client extends LatentConnection[T] {

    object connection extends Connection[T] {
      def send(msg: T): Async[Any, Unit] = Sync {
        server.callback.succeed(msg)
      }
      override def close(): Unit = ()
    }

    def prepare(incomingHandler: Handler[T]): Async[Abort, Connection[T]] = Sync {
      val callback = incomingHandler.getCallbackFor(connection)

      val serverConn = new Connection[T] {
        override def close(): Unit = ()
        override def send(message: T): Async[Any, Unit] = Sync {
          callback.succeed(message)
        }
      }
      server.connectionEstablished.succeed(serverConn)
      connection
    }
  }

  object server extends LatentConnection[T] {

    var callback: Callback[T] = null

    var connectionEstablished: Callback[Connection[T]] = null

    def prepare(incomingHandler: Handler[T]): Async[Abort, Connection[T]] = Async.fromCallback[Connection[T]] {
      connectionEstablished = Async.handler
    }.map { conn =>
      callback = incomingHandler.getCallbackFor(conn)
      conn
    }
  }

}

class DeltaDisseminationTest extends munit.FunSuite {
  test("basics") {

    // I have no clue why this syntax is still not deprecated xD
    val dd1, dd2 = DeltaDissemination[Set[String]](LocalUid.gen(), _ => ())

    val sync = SynchronizedConnection[ProtocolMessage[Set[String]]]()

    dd1.addLatentConnection(sync.server)
    dd2.addLatentConnection(sync.client)

    dd1.applyDelta(Set("a"))
    dd2.applyDelta(Set("b"))

    assertEquals(dd1.allDeltas.toSet, dd2.allDeltas.toSet)

  }
}
