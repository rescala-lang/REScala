package loci
package transmitter

import serializer.Serializables._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.Success

object MarshallableSpec {
  def roundTrip[B, I, P](marshallable: Marshallable[B, I, P])(value: B) = {
    import communicator._

    case class Remote(id: Int) extends RemoteRef {
      def protocol = new Protocol with SetupInfo with SecurityInfo with SymmetryInfo with Bidirectional {
        val setup = null
        val encrypted = false
        val integrityProtected = false
        val authenticated = false
      }
      def connected = true
      def disconnect() = ()
      val disconnected = Notice.Steady[Unit].notice
    }

    val remote0 = Remote(0)
    val remote1 = Remote(1)

    val abstractions = mutable.Map.empty[String, (Abstraction, Abstraction)]

    def retrieveAbstractions(id: String, remote: Remote) = {
      val (abstraction0, abstraction1) = abstractions.getOrElseUpdate(id, {
        val abstraction0 = Abstraction(id, remote0)
        val abstraction1 = Abstraction(id, remote1)
        abstraction0.doSend.notice foreach abstraction1.doReceive.fire
        abstraction1.doSend.notice foreach abstraction0.doReceive.fire
        abstraction0 -> abstraction1
      })

      remote match {
        case `remote0` => abstraction0
        case `remote1` => abstraction1
        case _ => throw new IllegalArgumentException(remote.toString)
      }
    }

    case class Abstraction(id: String, remote: Remote) extends AbstractionRef {
      val doReceive = Notice.Stream[MessageBuffer]
      val doSend = Notice.Stream[MessageBuffer]

      def channel = new Channel {
        val receive = doReceive.notice
        val closed = Notice.Steady[Unit].notice

        def send(message: MessageBuffer) = doSend.fire(message)
        def close() = ()
        def open = true
      }

      def derive(name: String) = retrieveAbstractions(s"$id:$name", remote)
    }

    val buffer = marshallable.marshal(value, retrieveAbstractions("abstraction", remote0))

    marshallable.unmarshal(buffer, retrieveAbstractions("abstraction", remote1))
  }
}

class MarshallableSpec extends AnyFlatSpec with Matchers with NoLogging {
  import MarshallableSpec._

  behavior of "Marshallable"

  def removeModuleInformation(stackTrace: Array[StackTraceElement]): Unit =
    for (i <- stackTrace.indices)
      stackTrace(i) = new StackTraceElement(
        stackTrace(i).getClassName,
        stackTrace(i).getMethodName,
        stackTrace(i).getFileName,
        stackTrace(i).getLineNumber)

  def removeModuleInformation(throwable: Throwable): Unit = {
    val stackTrace = throwable.getStackTrace
    removeModuleInformation(stackTrace)
    throwable.setStackTrace(stackTrace)
  }

  it should "derive standard marshallables correctly" in {
    val int = Marshallable[Int]
    CompileTimeUtils.assertType[
      Marshallable[Int, Int, Future[Int]]](
      int)
    roundTrip(int)(12) should be (Success(12))


    val nothing = Marshallable[Nothing]
    CompileTimeUtils.assertType[
      Marshallable[Nothing, Nothing, Future[Nothing]]](
      nothing)


    val optionString = Marshallable[Option[String]]
    CompileTimeUtils.assertType[
      Marshallable[Option[String], Option[String], Future[Option[String]]]](
      optionString)
    roundTrip(optionString)(None) should be (Success(None))
    roundTrip(optionString)(Some("foo")) should be (Success(Some("foo")))

    CompileTimeUtils.containsCompileTimeOnly(Marshallable[Option[Nothing]]) should be (true)

    CompileTimeUtils.containsCompileTimeOnly(Marshallable[Option[Any]]) should be (true)


    val eitherIntString = Marshallable[Either[Int, String]]
    CompileTimeUtils.assertType[
      Marshallable[Either[Int, String], Either[Int, String], Future[Either[Int, String]]]](
      eitherIntString)
    roundTrip(eitherIntString)(Left(12)) should be (Success(Left(12)))
    roundTrip(eitherIntString)(Right("foo")) should be (Success(Right("foo")))

    CompileTimeUtils.containsCompileTimeOnly(Marshallable[Either[String, Nothing]]) should be (true)

    CompileTimeUtils.containsCompileTimeOnly(Marshallable[Either[Nothing, Int]]) should be (true)

    CompileTimeUtils.containsCompileTimeOnly(Marshallable[Either[String, Any]]) should be (true)

    CompileTimeUtils.containsCompileTimeOnly(Marshallable[Either[Any, Int]]) should be (true)
  }

  it should "derive future marshallable correctly" in {
    val futureInt = Marshallable[Future[Int]]
    CompileTimeUtils.assertType[
      Marshallable[Future[Int], Future[Int], Future[Int]]](
      futureInt)
    roundTrip(futureInt)(Future.successful(12)).get.value should be (Some(Success(12)))

    val successPromiseInt = Promise[Int]()
    val successFutureInt = roundTrip(futureInt)(successPromiseInt.future)
    successFutureInt.get.value should be (None)

    successPromiseInt.success(12)
    successFutureInt.get.value should be (Some(Success(12)))

    val failurePromiseInt = Promise[Int]()
    val failureFutureInt = roundTrip(futureInt)(failurePromiseInt.future)
    failureFutureInt.get.value should be (None)

    val localException = new NoSuchElementException("no int here")
    removeModuleInformation(localException)
    failurePromiseInt.failure(localException)
    val remoteException = intercept[RemoteAccessException] { failureFutureInt.get.value.get.get }
    remoteException.reason should be (RemoteAccessException.RemoteException("java.util.NoSuchElementException", "no int here"))
    remoteException.getStackTrace should equal (localException.getStackTrace)
  }
}
