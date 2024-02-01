package loci
package registry

import contexts.Immediate.Implicits.global
import communicator.NetworkListener
import transmitter.RemoteAccessException
import serializer.Serializables._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.Success

class RegistrySpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Registry"

  it should "handle binding and lookup correctly" in {
    for (seed <- 0 to 10) {
      val listener = new NetworkListener(deferred = seed > 1, seed)

      val promise = Promise[(Int, String)]()

      val registry0 = new Registry
      registry0.bind("future")(promise.future)
      registry0.bind("intfun")(() => (???): Int)
      registry0.listen(listener)

      var futureValue: Future[(Int, String)] = null
      var intfunValue: Future[Int] = null

      val registry1 = new Registry
      registry1.connect(listener.createConnector()) foreach { remote =>
        val result0 = registry1.lookup[concurrent.Future[(Int, String)]]("future", remote)
        val result1 = registry1.lookup[() => Int]("intfun", remote)
        futureValue = result0
        intfunValue = result1()
      }

      if (seed % 2 == 0)
        promise.success(5 -> "yay")

      listener.run()

      if (seed % 2 != 0)
        promise.success(5 -> "yay")

      registry0.terminate()
      registry1.terminate()

      futureValue.value should be (Some(Success(5 -> "yay")))

      val remoteException = intercept[RemoteAccessException] { intfunValue.value.get.get }
      remoteException.reason should matchPattern { case RemoteAccessException.RemoteException("scala.NotImplementedError", _) => }
    }
  }

  it should "handle subjective binding and lookup correctly" in {
    for (seed <- 0 to 5) {
      val events = mutable.ListBuffer.empty[String]

      val listener = new NetworkListener(deferred = seed != 0, seed)

      val valueBinding = registry.Binding[String]("value")
      val methodBinding = registry.Binding[() => String]("method")

      def value(remote: transmitter.RemoteRef) = {
        events += "value called"
        "value result"
      }

      def method(remote: transmitter.RemoteRef) = {
        events += "method called"
        "method result"
      }

      val registry0 = new Registry
      registry0.bindSbj(valueBinding)(value _)
      registry0.bindSbj(methodBinding)(method _)
      registry0.listen(listener)

      val registry1 = new Registry
      registry1.connect(listener.createConnector()) foreach { remote =>
        val result0 = registry1.lookup(valueBinding, remote)
        val result1 = registry1.lookup(methodBinding, remote)

        result0 foreach { events += _ }
        result0 foreach { events += _ }
        result1() foreach { events += _ }
        result1() foreach { events += _ }
      }

      listener.run()
      registry0.terminate()
      registry1.terminate()

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
  }
}
