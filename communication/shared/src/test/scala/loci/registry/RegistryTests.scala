package loci
package registry

import communicator.{Connector, Listener}
import contexts.Pooled.Implicits.global
import transmitter.{RemoteAccessException, RemoteRef}
import serializer.Serializables._

import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Success, Try}

object RegistryTests extends Matchers {
  type Test = (Listener[Connections.Protocol], Connector[Connections.Protocol], => Boolean, => Boolean, => Unit) => Unit

  private implicit class RegistryRetryingOps(registry: Registry) {
    private def retrying[T](setup: => Boolean, count: Int = 0)(operation: => Try[T]): T = {
      val result = operation
      if ((result.isFailure || !setup) && count < 600) {
        Thread.sleep(100)
        retrying(setup, count + 1)(operation)
      }
      else
        result.get
    }

    def listenRetrying(listener: Listener[Connections.Protocol], setup: => Boolean): Unit =
      retrying(setup) { registry.listen(listener) }

    def connectRetrying(connector: Connector[Connections.Protocol], setup: => Boolean): RemoteRef =
      retrying(setup) { Await.ready(registry.connect(connector), 1.minute).value.get }
  }

  def `handle binding and lookup correctly`(
      listener: Listener[Connections.Protocol],
      connector: Connector[Connections.Protocol],
      setupListener: => Boolean = true,
      setupConnector: => Boolean = true,
      cleanup: => Unit = ()): Unit = {
    var registry0: Registry = null
    var registry1: Registry = null

    try {
      val promise = Promise[(Int, String)]()

      registry0 = new Registry
      registry0.bind("future")(promise.future)
      registry0.bind("intfun")(() => (???): Int)
      registry0.listenRetrying(listener, setupListener)

      registry1 = new Registry
      val remote = registry1.connectRetrying(connector, setupConnector)

      val result0 = registry1.lookup[concurrent.Future[(Int, String)]]("future", remote)
      val result1 = registry1.lookup[() => Int]("intfun", remote)
      val futureValue = result0
      val intfunValue = result1()

      promise.success(5 -> "yay")

      Await.ready(futureValue, 1.minute)
      futureValue.value should be (Some(Success(5 -> "yay")))

      Await.ready(intfunValue, 1.minute)
      val remoteException = intercept[RemoteAccessException] { intfunValue.value.get.get }
      remoteException.reason should matchPattern { case RemoteAccessException.RemoteException("scala.NotImplementedError", _) => }
    }
    finally {
      if (registry1 != null)
        registry1.terminate()

      if (registry0 != null) {
        registry0.remotes.headOption foreach { remote =>
          Await.ready(remote.disconnected, 1.minute)
        }
        registry0.terminate()
      }

      cleanup
    }
  }

  def `handle subjective binding and lookup correctly`(
      listener: Listener[Connections.Protocol],
      connector: Connector[Connections.Protocol],
      setupListener: => Boolean = true,
      setupConnector: => Boolean = true,
      cleanup: => Unit = ()): Unit = {
    var registry0: Registry = null
    var registry1: Registry = null

    try {
      val events = mutable.ListBuffer.empty[String]

      val valueBinding = registry.Binding[String]("value")
      val methodBinding = registry.Binding[() => String]("method")

      def value(remote: transmitter.RemoteRef) = {
        events.synchronized { events += "value called" }
        "value result"
      }

      def method(remote: transmitter.RemoteRef) = {
        events.synchronized { events += "method called" }
        "method result"
      }

      registry0 = new Registry
      registry0.bindSbj(valueBinding)(value _)
      registry0.bindSbj(methodBinding)(method _)
      registry0.listenRetrying(listener, setupListener)

      registry1 = new Registry
      val remote = registry1.connectRetrying(connector, setupConnector)

      val result0 = registry1.lookup(valueBinding, remote)
      val result1 = registry1.lookup(methodBinding, remote)

      val result0a = result0 map { result => events.synchronized { events += result } }
      val result0b = result0 map { result => events.synchronized { events += result } }
      val result1a = result1() map { result => events.synchronized { events += result } }
      val result1b = result1() map { result => events.synchronized { events += result } }

      Await.ready(Future.sequence(Seq(result0a, result0b, result1a, result1b)), 1.minute)

      events should contain theSameElementsAs Seq(
        "value called",
        "value result",
        "value result",
        "method called",
        "method result",
        "method called",
        "method result")

      events filter { _ startsWith "value" } should contain theSameElementsInOrderAs Seq(
        "value called", "value result", "value result")

      events find { _ startsWith "method" } should contain ("method called")

      events.remove(events.indexOf("method called"))
      events.remove(events.indexOf("method result"))

      events filterNot { _ startsWith "value" } should contain theSameElementsInOrderAs Seq(
        "method called", "method result")
    }
    finally {
      if (registry1 != null)
        registry1.terminate()

      if (registry0 != null) {
        registry0.remotes.headOption foreach { remote =>
          Await.ready(remote.disconnected, 1.minute)
        }
        registry0.terminate()
      }

      cleanup
    }
  }
}
