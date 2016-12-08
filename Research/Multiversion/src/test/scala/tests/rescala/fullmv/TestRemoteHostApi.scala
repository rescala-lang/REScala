package tests.rescala.fullmv

import java.rmi.registry.LocateRegistry
import java.rmi.registry.Registry
import java.rmi.{ConnectException, Naming, RemoteException}
import java.rmi.server.UnicastRemoteObject

import org.scalatest.Tag
import rescala.fullmv.Transaction
import rescala.fullmv.Host
import org.scalatest.words.ResultOfStringPassedToVerb
import org.scalatest.FlatSpecLike

trait TestRemoteHostApi extends java.rmi.Remote {
  @throws[RemoteException]
  def newTransaction(data: String): Transaction
  @throws[RemoteException]
  def beginTransaction(node: Transaction): Unit
  @throws[RemoteException]
  def endTransaction(node: Transaction): Unit
}

object TestRemoteHost {
  val NAME = "foo"
  val PORT = Registry.REGISTRY_PORT
  val ADDRESS = "rmi://localhost:" + PORT + "/" + NAME
  lazy val remote: Option[TestRemoteHostApi] = try{
    Some(Naming.lookup(TestRemoteHost.ADDRESS).asInstanceOf[TestRemoteHostApi])
  } catch {
    case e: ConnectException => None
  }
}

trait TestWithRemoteHost extends FlatSpecLike {
  def ifRemote(something: ResultOfStringPassedToVerb)(code: TestRemoteHostApi => Unit) = {
    val remote = TestRemoteHost.remote
    if(remote.isDefined) something in code(remote.get) else something ignore {}
  }
  def ifRemote(something: ItVerbString)(code: TestRemoteHostApi => Unit) = {
    val remote = TestRemoteHost.remote
    if(remote.isDefined) something in code(remote.get) else something ignore {}
  }
}

object TestRemoteHostImpl {
  def main(args: Array[String]): Unit = {
    val registry = LocateRegistry.createRegistry(TestRemoteHost.PORT)
    val server = new UnicastRemoteObject() with TestRemoteHostApi {
      override def newTransaction(data: String): Transaction = Transaction(data)
      override def beginTransaction(node: Transaction): Unit = node.assertLocal.beginTransaction()
      override def endTransaction(node: Transaction): Unit = node.assertLocal.endTransaction()
    }
    registry.bind(TestRemoteHost.NAME, server)
    println("Server up. Press Enter to terminate.")

    System.in.read()

    println("Server shutting down.")
    registry.unbind(TestRemoteHost.NAME)
    UnicastRemoteObject.unexportObject(server, true)
    Host.shutdown(true)
  }
}
