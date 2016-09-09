package tests.rescala.fullmv

import java.rmi.registry.LocateRegistry
import java.rmi.registry.Registry
import java.rmi.Naming
import java.rmi.server.UnicastRemoteObject
import org.scalatest.Tag
import rescala.fullmv.Transaction
import rescala.fullmv.Host

@remote trait RemoteTestHost {
  def newTransaction(data: String): Transaction
  def beginTransaction(node: Transaction): Unit
  def endTransaction(node: Transaction): Unit
}

object RemoteTestHost extends Tag("tests.rescala.fullmv.RemoteTestHost") {
  val NAME = "foo"
  val PORT = Registry.REGISTRY_PORT
  val ADDRESS = "rmi://localhost:" + PORT + "/" + NAME
  lazy val remote = Naming.lookup(RemoteTestHost.ADDRESS).asInstanceOf[RemoteTestHost]
}

object RemoteTestHostImpl {
  def main(args: Array[String]): Unit = {
    val registry = LocateRegistry.createRegistry(RemoteTestHost.PORT)
    val server = new UnicastRemoteObject() with RemoteTestHost {
      override def newTransaction(data: String): Transaction = Transaction(data)
      override def beginTransaction(node: Transaction): Unit = node.assertLocal.beginTransaction()
      override def endTransaction(node: Transaction): Unit = node.assertLocal.endTransaction()
    }
    registry.bind(RemoteTestHost.NAME, server)
    println("Server up. Press Enter to terminate.")

    System.in.read()

    println("Server shutting down.")
    registry.unbind(RemoteTestHost.NAME)
    UnicastRemoteObject.unexportObject(server, true)
    Host.shutdown(true)
  }
}
