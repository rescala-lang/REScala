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
  test("simple connection") {
    val idA       = IdentityFactory.createNewIdentity
    val receiverA = QueueAppendingMessageReceiver()
    val connManA  = ConnectionManager[String](idA, receiverA)
    connManA.acceptIncomingConnections()

    val idB       = IdentityFactory.createNewIdentity
    val receiverB = QueueAppendingMessageReceiver()
    val connManB  = ConnectionManager[String](idB, receiverB)
    connManB.connectTo("localhost", connManA.listenPort.get)
    assertEventually(1000 millis)(
      connManA.connectedUsers == Set(idB.getPublic) && connManB.connectedUsers == Set(idA.getPublic)
    )
  }

  test("end-to-end test") {
    val idA = IdentityFactory.createNewIdentity
    println(s"idA= ${idA.getPublic}")
    val receiverA = QueueAppendingMessageReceiver()
    val connManA  = ConnectionManager[String](idA, receiverA)

    val idB = IdentityFactory.createNewIdentity
    println(s"idB= ${idB.getPublic}")
    val receiverB = QueueAppendingMessageReceiver()
    val connManB  = ConnectionManager[String](idB, receiverB)

    val idC = IdentityFactory.createNewIdentity
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
    assertEquals(receiverC.queue.poll(2, SECONDS), ("Test 3", idB.getPublic))
    assert(connManC.sendMultiple(idB.getPublic, "Test 4", "Test 5"))
    assertEquals(receiverB.queue.poll(2, SECONDS), ("Test 4", idC.getPublic))
    assertEquals(receiverB.queue.poll(2, SECONDS), ("Test 5", idC.getPublic))
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
      queue.put((msg, fromUser))
    }

  }

  given JsonValueCodec[String]       = JsonCodecMaker.make
  given MessageSerialization[String] = MessageSerialization.derived
}
