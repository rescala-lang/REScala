package lofi_acl.sync

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import lofi_acl.crypto.{IdentityFactory, PublicIdentity}
import lofi_acl.sync.ConnectionManagerTest.{QueueAppendingMessageReceiver, assertEventually, given}
import munit.FunSuite

import java.util.concurrent.LinkedBlockingQueue
import scala.concurrent.duration
import scala.concurrent.duration.*
import scala.language.postfixOps

class ConnectionManagerTest extends FunSuite {
  private val idA = IdentityFactory.createNewIdentity
  private val idB = IdentityFactory.createNewIdentity
  private val idC = IdentityFactory.createNewIdentity

  println(s"idA = ${idA.getPublic}")
  println(s"idB = ${idB.getPublic}")
  println(s"idC = ${idC.getPublic}")

  test("Only establish connection and don't send anything") {
    val receiverA = QueueAppendingMessageReceiver()
    val connManA  = ConnectionManager[String](idA, receiverA)
    connManA.acceptIncomingConnections()

    val receiverB = QueueAppendingMessageReceiver()
    val connManB  = ConnectionManager[String](idB, receiverB)
    connManB.connectTo("localhost", connManA.listenPort.get)
    assertEventually(1 second)(
      connManA.connectedUsers == Set(idB.getPublic) && connManB.connectedUsers == Set(idA.getPublic)
    )

    connManA.shutdown()
    connManB.shutdown()
  }

  test("connectToExpectingUserIfNoConnectionExists") {
    val receiverA = QueueAppendingMessageReceiver()
    val connManA  = ConnectionManager[String](idA, receiverA)
    connManA.acceptIncomingConnections()

    val receiverB = QueueAppendingMessageReceiver()
    val connManB  = ConnectionManager[String](idB, receiverB)
    connManB.acceptIncomingConnections()

    val receiverC = QueueAppendingMessageReceiver()
    val connManC  = ConnectionManager[String](idC, receiverC)
    connManC.acceptIncomingConnections()

    connManA.connectToExpectingUserIfNoConnectionExists("localhost", connManB.listenPort.get, idC.getPublic)
    connManA.connectToExpectingUserIfNoConnectionExists("localhost", connManC.listenPort.get, idB.getPublic)
    connManB.connectToExpectingUserIfNoConnectionExists("localhost", connManA.listenPort.get, idC.getPublic)
    connManB.connectToExpectingUserIfNoConnectionExists("localhost", connManC.listenPort.get, idA.getPublic)
    connManC.connectToExpectingUserIfNoConnectionExists("localhost", connManA.listenPort.get, idC.getPublic)
    connManC.connectToExpectingUserIfNoConnectionExists("localhost", connManB.listenPort.get, idC.getPublic)
    // The remote will accept the connection, so we need to wait for the initiator to close the connection
    Thread.sleep(100)
    assertEquals(connManA.connectedUsers, Set.empty)
    assertEquals(connManB.connectedUsers, Set.empty)
    assertEquals(connManC.connectedUsers, Set.empty)

    connManA.connectToExpectingUserIfNoConnectionExists("localhost", connManB.listenPort.get, idB.getPublic)
    connManB.connectToExpectingUserIfNoConnectionExists("localhost", connManC.listenPort.get, idC.getPublic)
    connManC.connectToExpectingUserIfNoConnectionExists("localhost", connManA.listenPort.get, idA.getPublic)
    connManC.connectToExpectingUserIfNoConnectionExists("localhost", connManB.listenPort.get, idB.getPublic)
    assertEventually(1 second)(
      connManA.connectedUsers == Set(idB.getPublic, idC.getPublic)
      && connManB.connectedUsers == Set(idA.getPublic, idC.getPublic)
      && connManC.connectedUsers == Set(idA.getPublic, idB.getPublic)
    )

    connManA.broadcast("Test")
    assertEquals(receiverB.queue.poll(1, SECONDS), ("Test", idA.getPublic))
    assertEquals(receiverC.queue.poll(1, SECONDS), ("Test", idA.getPublic))
    connManB.broadcast("Test 2")
    assertEquals(receiverA.queue.poll(1, SECONDS), ("Test 2", idB.getPublic))
    assertEquals(receiverC.queue.poll(1, SECONDS), ("Test 2", idB.getPublic))
    connManC.broadcast("Test 3")
    assertEquals(receiverA.queue.poll(1, SECONDS), ("Test 3", idC.getPublic))
    assertEquals(receiverB.queue.poll(1, SECONDS), ("Test 3", idC.getPublic))

    connManA.shutdown()
    connManB.shutdown()
  }

  test("send message from initiator to acceptor and vice versa") {
    val receiverA = QueueAppendingMessageReceiver()
    val connManA  = ConnectionManager[String](idA, receiverA)
    connManA.acceptIncomingConnections()

    val receiverB = QueueAppendingMessageReceiver()
    val connManB  = ConnectionManager[String](idB, receiverB)
    connManB.connectTo("localhost", connManA.listenPort.get)

    val receiverC = QueueAppendingMessageReceiver()
    val connManC  = ConnectionManager[String](idC, receiverC)
    connManC.acceptIncomingConnections()

    assertEventually(1 second)(
      connManA.connectedUsers == Set(idB.getPublic) && connManB.connectedUsers == Set(idA.getPublic)
    )

    assert(connManA.send(idB.getPublic, "Hello B"))
    assertEquals(receiverB.queue.poll(1, SECONDS), ("Hello B", idA.getPublic))

    assert(connManB.send(idA.getPublic, "Hello A"))
    assertEquals(receiverA.queue.poll(1, SECONDS), ("Hello A", idB.getPublic))

    connManA.shutdown()
    connManB.shutdown()
  }

  test("broadcast") {
    val receiverA = QueueAppendingMessageReceiver()
    val connManA  = ConnectionManager[String](idA, receiverA)
    connManA.acceptIncomingConnections()

    val receiverB = QueueAppendingMessageReceiver()
    val connManB  = ConnectionManager[String](idB, receiverB)
    connManB.acceptIncomingConnections()

    val receiverC = QueueAppendingMessageReceiver()
    val connManC  = ConnectionManager[String](idC, receiverC)
    connManC.acceptIncomingConnections()

    connManA.connectTo("localhost", connManB.listenPort.get)
    connManA.connectTo("localhost", connManC.listenPort.get)

    assertEventually(1 second)(
      connManA.connectedUsers == Set(idB.getPublic, idC.getPublic)
      && connManB.connectedUsers == Set(idA.getPublic)
      && connManC.connectedUsers == Set(idA.getPublic)
    )

    assert(connManA.broadcast("Hello All"))
    assertEquals(receiverB.queue.poll(1, SECONDS), ("Hello All", idA.getPublic))
    assertEquals(receiverC.queue.poll(1, SECONDS), ("Hello All", idA.getPublic))

    assert(connManB.broadcast("Hello A"))
    assertEquals(receiverA.queue.poll(1, SECONDS), ("Hello A", idB.getPublic))
    assert(receiverC.queue.isEmpty)

    assert(connManC.broadcast("Hello A"))
    assert(receiverB.queue.isEmpty)
    assertEquals(receiverA.queue.poll(1, SECONDS), ("Hello A", idC.getPublic))

    connManB.connectToExpectingUserIfNoConnectionExists("localhost", connManC.listenPort.get, idC.getPublic)
    assertEventually(1 second)(
      connManA.connectedUsers == Set(idB.getPublic, idC.getPublic)
      && connManB.connectedUsers == Set(idA.getPublic, idC.getPublic)
      && connManC.connectedUsers == Set(idA.getPublic, idB.getPublic)
    )

    assert(connManB.broadcast("Hello A and C"))
    assertEquals(receiverA.queue.poll(1, SECONDS), ("Hello A and C", idB.getPublic))
    assert(receiverB.queue.isEmpty)
    assertEquals(receiverC.queue.poll(1, SECONDS), ("Hello A and C", idB.getPublic))

    assert(connManC.broadcast("Hello A and B"))
    assertEquals(receiverA.queue.poll(1, SECONDS), ("Hello A and B", idC.getPublic))
    assertEquals(receiverB.queue.poll(1, SECONDS), ("Hello A and B", idC.getPublic))
    assert(receiverC.queue.isEmpty)

    connManA.shutdown()
    connManB.shutdown()
    connManC.shutdown()
  }

  test("end-to-end test".ignore) {
    println(s"idA= ${idA.getPublic}")
    val receiverA = QueueAppendingMessageReceiver()
    val connManA  = ConnectionManager[String](idA, receiverA)

    println(s"idB= ${idB.getPublic}")
    val receiverB = QueueAppendingMessageReceiver()
    val connManB  = ConnectionManager[String](idB, receiverB)

    println(s"idC= ${idC.getPublic}")
    val receiverC = QueueAppendingMessageReceiver()
    val connManC  = ConnectionManager[String](idC, receiverC)

    connManA.acceptIncomingConnections()

    assertEventually(100 millis)(connManA.listenPort.nonEmpty)
    connManB.connectTo("localhost", connManA.listenPort.get)

    assertEventually(1 second)(
      connManB.connectedUsers.nonEmpty
      && connManA.connectedUsers.nonEmpty
      && connManA.connectedUsers == Set(idB.getPublic)
    )
    connManC.connectToExpectingUserIfNoConnectionExists("localhost", connManA.listenPort.get, idA.getPublic)

    assertEventually(1 second) {
      connManA.connectedUsers.equals(Set(idB.getPublic, idC.getPublic))
      && connManB.connectedUsers.equals(Set(idA.getPublic))
      && connManC.connectedUsers.equals(Set(idA.getPublic))
    }

    assert(connManA.broadcast("Hello"))
    assertEquals(receiverA.queue.peek(), null)
    assertEquals(receiverB.queue.poll(1, SECONDS), ("Hello", idA.getPublic))
    assertEquals(receiverC.queue.poll(1, SECONDS), ("Hello", idA.getPublic))

    assert(connManB.send(idA.getPublic, "Test"))
    assertEquals(receiverA.queue.poll(1, SECONDS), ("Test", idB.getPublic))

    connManB.acceptIncomingConnections()
    connManC.acceptIncomingConnections()
    assertEventually(100 millis)(connManB.listenPort.nonEmpty && connManC.listenPort.nonEmpty)

    connManB.connectToExpectingUserIfNoConnectionExists("localhost", connManC.listenPort.get, idC.getPublic)
    connManC.connectToExpectingUserIfNoConnectionExists("localhost", connManB.listenPort.get, idB.getPublic)

    assertEventually(1 second) {
      connManA.connectedUsers.equals(Set(idB.getPublic, idC.getPublic)) &&
      connManB.connectedUsers.equals(Set(idA.getPublic, idC.getPublic)) &&
      connManC.connectedUsers.equals(Set(idA.getPublic, idB.getPublic))
    }

    assert(connManB.send(idC.getPublic, "Test 3"))
    assertEquals(receiverC.queue.poll(4, SECONDS), ("Test 3", idB.getPublic))
    assert(connManC.sendMultiple(idB.getPublic, "Test 4", "Test 5"))
    assertEquals(receiverB.queue.poll(4, SECONDS), ("Test 4", idC.getPublic))
    assertEquals(receiverB.queue.poll(4, SECONDS), ("Test 5", idC.getPublic))
    println("Done")

    connManA.shutdown()
    connManB.shutdown()
    connManC.shutdown()
  }
}

object ConnectionManagerTest {
  def assertEventually(timeout: Duration)(assertion: => Boolean): Unit = {
    val stopTime = System.currentTimeMillis() + timeout.toMillis

    while System.currentTimeMillis() < stopTime && !assertion do {
      Thread.`yield`()
    }

    assert(assertion)
  }

  class QueueAppendingMessageReceiver extends MessageReceiver[String] {
    val queue: LinkedBlockingQueue[(String, PublicIdentity)] = LinkedBlockingQueue[(String, PublicIdentity)]()

    override def receivedMessage(msg: String, fromUser: PublicIdentity): Unit = {
      println(s"Received $msg from $fromUser")
      queue.put((msg, fromUser))
    }

  }

  given JsonValueCodec[String]       = JsonCodecMaker.make
  given MessageSerialization[String] = MessageSerialization.derived
}
