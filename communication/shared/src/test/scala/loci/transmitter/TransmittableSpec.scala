package loci
package transmitter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Future

object TransmittableSpec {
  def roundTrip[B, I, R, P, T <: Transmittables](
      transmittable: Transmittable.Aux[B, I, R, P, T])(value: B)(implicit
      contextBuilder: ContextBuilder[T]) = {
    object abstraction extends AbstractionRef {
      val remote = null
      val channel =  new Channel {
        val receive = Notice.Stream[MessageBuffer].notice
        val closed = Notice.Steady[Unit].notice
        def send(message: MessageBuffer) = ()
        def close() = ()
        def open = false
      }
      def derive(name: String) = abstraction
    }

    val intermediate = transmittable.buildIntermediate(value)(
      contextBuilder(transmittable.transmittables, abstraction, ContextBuilder.sending))

    transmittable.buildResult(intermediate)(
      contextBuilder(transmittable.transmittables, abstraction, ContextBuilder.receiving))
  }
}

class TransmittableSpec extends AnyFlatSpec with Matchers with NoLogging {
  import TransmittableSpec._

  behavior of "Transmittable"

  it should "derive standard transmittables correctly" in {
    val int = Transmittable[Int]
    CompileTimeUtils.assertType[
      Transmittable.Aux[Int, Int, Int, Future[Int], Transmittables.None]](
      int)
    roundTrip(int)(12) should be (12)


    val nothing = Transmittable[Nothing]
    CompileTimeUtils.assertType[
      Transmittable.Aux[Nothing, Nothing, Nothing, Future[Nothing], Transmittables.None]](
      nothing)


    val optionString = Transmittable[Option[String]]
    CompileTimeUtils.assertType[
      Transmittable.Aux[Option[String], Option[String], Option[String], Future[Option[String]], Transmittables.None]](
      optionString)
    roundTrip(optionString)(None) should be (None)
    roundTrip(optionString)(Some("foo")) should be (Some("foo"))

    val optionNothing = Transmittable[Option[Nothing]]
    CompileTimeUtils.assertType[
      Transmittable.Aux[Option[Nothing], Option[Nothing], Option[Nothing], Future[Option[Nothing]], Transmittables.None]](
      optionNothing)
    roundTrip(optionNothing)(None) should be (None)

    CompileTimeUtils.containsCompileTimeOnly(Transmittable[Option[Any]]) should be (true)


    val eitherIntString = Transmittable[Either[Int, String]]
    CompileTimeUtils.assertType[
      Transmittable.Aux[
        Either[Int, String],
        Either[Int, String],
        Either[Int, String],
        Future[Either[Int, String]],
        Transmittables.None]](
      eitherIntString)
    roundTrip(eitherIntString)(Left(12)) should be (Left(12))
    roundTrip(eitherIntString)(Right("foo")) should be (Right("foo"))

    val eitherStringNothing = Transmittable[Either[String, Nothing]]
    CompileTimeUtils.assertType[
      Transmittable.Aux[
        Either[String, Nothing],
        Either[String, Nothing],
        Either[String, Nothing],
        Future[Either[String, Nothing]],
        Transmittables.None]](
      eitherStringNothing)
    roundTrip(eitherStringNothing)(Left("foo")) should be (Left("foo"))

    val eitherNothingInt = Transmittable[Either[Nothing, Int]]
    CompileTimeUtils.assertType[
      Transmittable.Aux[
        Either[Nothing, Int],
        Either[Nothing, Int],
        Either[Nothing, Int],
        Future[Either[Nothing, Int]],
        Transmittables.None]](
      eitherNothingInt)
    roundTrip(eitherNothingInt)(Right(12)) should be (Right(12))

    CompileTimeUtils.containsCompileTimeOnly(Transmittable[Either[String, Any]]) should be (true)

    CompileTimeUtils.containsCompileTimeOnly(Transmittable[Either[Any, Int]]) should be (true)


    val seqString = Transmittable[Seq[String]]
    CompileTimeUtils.assertType[
      Transmittable.Aux[Seq[String], Seq[String], Seq[String], Future[Seq[String]], Transmittables.None]](
      seqString)
    roundTrip(seqString)(Seq("foo", "bar")) should be (Seq("foo", "bar"))

    val seqNothing = Transmittable[Seq[Nothing]]
    CompileTimeUtils.assertType[
      Transmittable.Aux[Seq[Nothing], Seq[Nothing], Seq[Nothing], Future[Seq[Nothing]], Transmittables.None]](
      seqNothing)

    CompileTimeUtils.containsCompileTimeOnly(Transmittable[Seq[Any]]) should be (true)


    val mapIntString = Transmittable[Map[Int, String]]
    CompileTimeUtils.assertType[
      Transmittable.Aux[
        Map[Int, String],
        Map[Int, String],
        Map[Int, String],
        Future[Map[Int, String]],
        Transmittables.None]](
      mapIntString)
    roundTrip(mapIntString)(Map(12 -> "foo", 24 -> "bar")) should be (Map(12 -> "foo", 24 -> "bar"))

    val mapStringNothing = Transmittable[Map[String, Nothing]]
    CompileTimeUtils.assertType[
      Transmittable.Aux[
        Map[String, Nothing],
        Map[String, Nothing],
        Map[String, Nothing],
        Future[Map[String, Nothing]],
        Transmittables.None]](
      mapStringNothing)
    roundTrip(mapStringNothing)(Map.empty) should be (Map.empty)

    val mapNothingInt = Transmittable[Map[Nothing, Int]]
    CompileTimeUtils.assertType[
      Transmittable.Aux[
        Map[Nothing, Int],
        Map[Nothing, Int],
        Map[Nothing, Int],
        Future[Map[Nothing, Int]],
        Transmittables.None]](
      mapNothingInt)
    roundTrip(mapNothingInt)(Map.empty) should be (Map.empty)

    CompileTimeUtils.containsCompileTimeOnly(Transmittable[Map[String, Any]]) should be (true)

    CompileTimeUtils.containsCompileTimeOnly(Transmittable[Map[Any, Int]]) should be (true)
  }

  it should "derive future transmittable correctly" in {
    val futureInt = Transmittable[Future[Int]]
    CompileTimeUtils.assertType[
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
            Transmittables.None]]]](
      futureInt)
  }
}
