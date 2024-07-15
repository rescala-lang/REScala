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
  val isGithubCi: Boolean           = Option(System.getenv("GITHUB_WORKFLOW")).exists(_.nonEmpty)
  //override def munitIgnore: Boolean = isGithubCi

  private val idA = IdentityFactory.createNewIdentity
  private val idB = IdentityFactory.createNewIdentity
  private val idC = IdentityFactory.createNewIdentity
  private val idD = IdentityFactory.createNewIdentity

  println(s"idA = ${idA.getPublic.id}")
  println(s"idB = ${idB.getPublic.id}")
  println(s"idC = ${idC.getPublic.id}")
  println(s"idD = ${idD.getPublic.id}")

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

    Thread.sleep(10)

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
    connManC.shutdown()
  }

  test("send message from initiator to acceptor and vice versa") {
    val receiverA = QueueAppendingMessageReceiver()
    val connManA  = ConnectionManager[String](idA, receiverA)
    connManA.acceptIncomingConnections()

    val receiverB = QueueAppendingMessageReceiver()
    val connManB  = ConnectionManager[String](idB, receiverB)

    val receiverC = QueueAppendingMessageReceiver()
    val connManC  = ConnectionManager[String](idC, receiverC)
    connManC.acceptIncomingConnections()

    Thread.sleep(10)

    connManB.connectTo("localhost", connManA.listenPort.get)

    assertEventually(1 second)(
      connManA.connectedUsers == Set(idB.getPublic) && connManB.connectedUsers == Set(idA.getPublic)
    )

    assert(connManA.send(idB.getPublic, "Hello B"))
    assertEquals(receiverB.queue.poll(1, SECONDS), ("Hello B", idA.getPublic))

    assert(connManB.send(idA.getPublic, "Hello A"))
    assertEquals(receiverA.queue.poll(1, SECONDS), ("Hello A", idB.getPublic))

    connManA.shutdown()
    connManB.shutdown()
    connManC.shutdown()
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

    Thread.sleep(10)

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

  test("race connectTo") {
    val receiverA = QueueAppendingMessageReceiver()
    val connManA  = ConnectionManager[String](idA, receiverA)
    connManA.acceptIncomingConnections()

    val receiverB = QueueAppendingMessageReceiver()
    val connManB  = ConnectionManager[String](idB, receiverB)
    connManB.acceptIncomingConnections()

    val receiverC = QueueAppendingMessageReceiver()
    val connManC  = ConnectionManager[String](idC, receiverC)
    connManC.acceptIncomingConnections()

    val receiverD = QueueAppendingMessageReceiver()
    val connManD  = ConnectionManager[String](idD, receiverD)
    connManD.acceptIncomingConnections()

    Thread.sleep(10)

    connManB.connectTo("localhost", connManA.listenPort.get)
    connManC.connectTo("localhost", connManA.listenPort.get)
    connManD.connectTo("localhost", connManA.listenPort.get)

    connManA.connectTo("localhost", connManB.listenPort.get)
    connManC.connectTo("localhost", connManB.listenPort.get)
    connManD.connectTo("localhost", connManB.listenPort.get)

    connManA.connectTo("localhost", connManC.listenPort.get)
    connManB.connectTo("localhost", connManC.listenPort.get)
    connManD.connectTo("localhost", connManC.listenPort.get)

    connManA.connectTo("localhost", connManD.listenPort.get)
    connManB.connectTo("localhost", connManD.listenPort.get)
    connManC.connectTo("localhost", connManD.listenPort.get)

    assertEventually(1 second)(
      connManA.connectedUsers == Set(idB.getPublic, idC.getPublic, idD.getPublic)
      && connManB.connectedUsers == Set(idA.getPublic, idC.getPublic, idD.getPublic)
      && connManC.connectedUsers == Set(idA.getPublic, idB.getPublic, idD.getPublic)
      && connManD.connectedUsers == Set(idA.getPublic, idB.getPublic, idC.getPublic)
    )

    connManA.shutdown()
    connManB.shutdown()
    connManC.shutdown()
    connManD.shutdown()
  }

  test("race connectTo ALTERNATIVE") {
    val receiverA = QueueAppendingMessageReceiver()
    val connManA  = ConnectionManager[String](idA, receiverA)
    connManA.acceptIncomingConnections()

    val receiverB = QueueAppendingMessageReceiver()
    val connManB  = ConnectionManager[String](idB, receiverB)
    connManB.acceptIncomingConnections()

    val receiverC = QueueAppendingMessageReceiver()
    val connManC  = ConnectionManager[String](idC, receiverC)
    connManC.acceptIncomingConnections()

    val receiverD = QueueAppendingMessageReceiver()
    val connManD  = ConnectionManager[String](idD, receiverD)
    connManD.acceptIncomingConnections()

    Thread.sleep(10)

    connManA.connectTo("localhost", connManB.listenPort.get)
    connManB.connectTo("localhost", connManA.listenPort.get)

    connManA.connectTo("localhost", connManC.listenPort.get)
    connManC.connectTo("localhost", connManA.listenPort.get)

    connManB.connectTo("localhost", connManC.listenPort.get)
    connManC.connectTo("localhost", connManB.listenPort.get)

    connManA.connectTo("localhost", connManD.listenPort.get)
    connManD.connectTo("localhost", connManA.listenPort.get)

    connManB.connectTo("localhost", connManD.listenPort.get)
    connManD.connectTo("localhost", connManB.listenPort.get)

    connManC.connectTo("localhost", connManD.listenPort.get)
    connManD.connectTo("localhost", connManC.listenPort.get)

    Thread.sleep(10)

    assertEventually(1 second)(
      connManA.connectedUsers == Set(idB.getPublic, idC.getPublic, idD.getPublic)
      && connManB.connectedUsers == Set(idA.getPublic, idC.getPublic, idD.getPublic)
      && connManC.connectedUsers == Set(idA.getPublic, idB.getPublic, idD.getPublic)
      && connManD.connectedUsers == Set(idA.getPublic, idB.getPublic, idC.getPublic)
    )

    connManA.shutdown()
    connManB.shutdown()
    connManC.shutdown()
    connManD.shutdown()
  }

  test("end-to-end test") {
    val receiverA = QueueAppendingMessageReceiver(idA.getPublic)
    val connManA  = ConnectionManager[String](idA, receiverA)
    connManA.acceptIncomingConnections()

    val receiverB = QueueAppendingMessageReceiver(idB.getPublic)
    val connManB  = ConnectionManager[String](idB, receiverB)
    connManB.acceptIncomingConnections()

    val receiverC = QueueAppendingMessageReceiver(idC.getPublic)
    val connManC  = ConnectionManager[String](idC, receiverC)
    connManC.acceptIncomingConnections()

    Thread.sleep(10) // Apparently we need to wait for the ServerSockets to actually accept connections

    connManB.connectTo("localhost", connManA.listenPort.get) // Establish A <-> B

    assertEventually(1 second)(
      connManB.connectedUsers.nonEmpty
      && connManA.connectedUsers.nonEmpty
      && connManA.connectedUsers == Set(idB.getPublic)
    )

    connManC.connectToExpectingUserIfNoConnectionExists("localhost", connManA.listenPort.get, idA.getPublic) // C <-> A

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

    connManB.connectToExpectingUserIfNoConnectionExists("localhost", connManC.listenPort.get, idC.getPublic)
    connManC.connectToExpectingUserIfNoConnectionExists("localhost", connManB.listenPort.get, idB.getPublic)

    assertEventually(1 second) {
      connManA.connectedUsers.equals(Set(idB.getPublic, idC.getPublic)) &&
      connManB.connectedUsers.equals(Set(idA.getPublic, idC.getPublic)) &&
      connManC.connectedUsers.equals(Set(idA.getPublic, idB.getPublic))
    }

    assert(connManB.send(idC.getPublic, "Test 3"))
    assertEquals(receiverC.queue.poll(1, SECONDS), ("Test 3", idB.getPublic))
    assert(connManC.sendMultiple(idB.getPublic, "Test 4", "Test 5"))
    assertEquals(receiverB.queue.poll(1, SECONDS), ("Test 4", idC.getPublic))
    assertEquals(receiverB.queue.poll(1, SECONDS), ("Test 5", idC.getPublic))

    connManA.shutdown()
    connManB.shutdown()
    connManC.shutdown()
  }
}

object ConnectionManagerTest {
  def assertEventually(timeout: Duration)(assertion: => Boolean): Unit = {
    val stopTime = System.currentTimeMillis() + timeout.toMillis

    // Repeated loop ensures that assertion was stable for 20ms
    while System.currentTimeMillis() < stopTime && !assertion do {
      while System.currentTimeMillis() < stopTime && !assertion do {
        while System.currentTimeMillis() < stopTime && !assertion do {
          Thread.`yield`()
        }
        Thread.sleep(10)
      }
      Thread.sleep(10)
    }

    assert(assertion)
  }

  class QueueAppendingMessageReceiver(_id: PublicIdentity = null) extends MessageReceiver[String] {
    private val localId = Option(_id)

    val queue: LinkedBlockingQueue[(String, PublicIdentity)] = LinkedBlockingQueue[(String, PublicIdentity)]()

    override def receivedMessage(msg: String, fromUser: PublicIdentity): Unit = {
      queue.put((msg, fromUser))
    }

    override def connectionEstablished(publicIdentity: PublicIdentity): Unit =
      println(s"${localId.map(_.id).getOrElse("Replica")} is now connected to ${publicIdentity.id}")

    override def connectionShutdown(publicIdentity: PublicIdentity): Unit =
      println(s"${localId.map(_.id).getOrElse("Replica")} connectionShutdown to $publicIdentity")
  }

  given JsonValueCodec[String]       = JsonCodecMaker.make
  given MessageSerialization[String] = MessageSerialization.derived
}
