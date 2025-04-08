package channels

import channels.TestUtil.printErrors
import de.rmgk.delay.{Async, Callback}

import java.util.concurrent.{Executors, Semaphore}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

trait EchoCommunicationTest[Info](
    serverConn: ExecutionContext => (Info, LatentConnection[MessageBuffer]),
    clientConn: ExecutionContext => Info => LatentConnection[MessageBuffer]
) extends munit.FunSuite {

  // need an execution context that generates new tasks as TCP does lots of blocking
  val executor             = Executors.newCachedThreadPool()
  val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  override def afterAll(): Unit = {
    executor.shutdownNow()
  }

  test("sample communication") {

    given abort: Abort = Abort()

    val toSend                 = List("Hi", "ho", "let’s", "go")
    val messageCounter         = Semaphore(0)
    var received: List[String] = Nil

    var traced = List.empty[String]

    def trace(msg: String) = synchronized {
      traced = msg :: traced
    }

    trace(s"test starting")

    val (info, serverLatent) = serverConn(ec)

    val echoServer: Prod[Connection[MessageBuffer]] =
      serverLatent.prepare: conn =>
        printErrors: mb =>
          trace(s"server received; echoing")
          conn.send(mb).runToFuture(using ())

    val client: Prod[Connection[MessageBuffer]] =
      clientConn.apply(ec).apply(info).prepare: conn =>
        printErrors: mb =>
          trace(s"client received")
          synchronized {
            received = String(mb.asArray) :: received
          }
          messageCounter.release()

    echoServer.run:
      printErrors: conn =>
        ()

    val sending = Async: (_: Abort) ?=>
      trace(s"starting sending")
      val conn = client.bind
      trace(s"sending")
      ec.execute: () =>
        toSend.foreach: msg =>
          conn.send(ArrayMessageBuffer(msg.getBytes())).run(using ()):
            printErrors(_ => ())

    sending.run:
      printErrors: conn =>
        ()

    trace(s"test waiting")

    messageCounter.acquire(toSend.size)
    assertEquals(toSend.sorted, received.sorted, traced)
  }

}
