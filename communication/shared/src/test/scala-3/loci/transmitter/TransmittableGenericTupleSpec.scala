package loci
package transmitter

import serializer.Serializables._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Future
import scala.util.{Success, Try}

class TransmittableGenericTupleSpec extends AnyFlatSpec with Matchers with NoLogging:
  import TransmittableSpec._

  behavior of "Transmittable for generic tuples"

  type ValueOfFuture[T] = T match
    case Future[t] => Option[Try[t]]
    case _ => T

  val valueOfFuture = [T] => (v: T) => v match
    case v: Future[?] => v.value
    case _ => v
  : ValueOfFuture[T]

  it should "derive generic tuple transmittables correctly" in {
    val flat = Transmittable[(Option[String], Option[String], Double, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]
    CompileTimeUtils.assertType[
      Transmittable.Aux[
        (Option[String], Option[String], Double, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
        (Option[String], Option[String], Double, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
        (Option[String], Option[String], Double, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
        Future[(Option[String], Option[String], Double, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)],
        Transmittables.None]](
      flat)
    roundTrip(flat)((None, Some("2"), 3.0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)) should be (
      (None, Some("2"), 3.0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23))


    CompileTimeUtils.containsCompileTimeOnly(
      Transmittable[(Option[String], Option[String], Double, Any, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]) should be (true)


    val nested = Transmittable[(Option[String], Option[String], Double, Future[Int], Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]
    CompileTimeUtils.assertType[
      Transmittable.Aux[
        (Option[String], Option[String], Double, Future[Int], Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
        (Option[String], Option[String], Double, (Option[Int], Option[String]), Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
        (Option[String], Option[String], Double, Future[Int], Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
        Future[(Option[String], Option[String], Double, Future[Int], Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)],
        Transmittables.Delegates[
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None] /
          Transmittable.Aux[
            Future[Int],
            (Option[Int], Option[String]),
            Future[Int],
            Future[Int],
            Transmittables.Message[
              Transmittable.Aux[
                (Option[Int], Option[String]),
                (Option[Int], Option[String]),
                (Option[Int], Option[String]),
                Future[(Option[Int], Option[String])],
                Transmittables.None]]] /
          Transmittable.Aux[Double, Double, Double, Future[Double], Transmittables.None] /
          Transmittable.Aux[Option[String], Option[String], Option[String], Future[Option[String]], Transmittables.None] /
          Transmittable.Aux[Option[String], Option[String], Option[String], Future[Option[String]], Transmittables.None]]]](
      nested)
    roundTrip(nested)((None, Some("2"), 3.0, Future.successful(4), 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)) map valueOfFuture should be (
      (None, Some("2"), 3.0, Some(Success(4)), 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23))
  }
end TransmittableGenericTupleSpec
