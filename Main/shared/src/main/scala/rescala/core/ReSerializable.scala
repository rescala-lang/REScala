package rescala.core

import scala.annotation.implicitNotFound
import scala.util.{Success, Try}

/** Used when the state of a reactive has to be serialized.
  * By default this is disabled, but certain Schedulers may require it.*/
@implicitNotFound("${T} is not serializable")
trait ReSerializable[T] {
  def serialize(value: T): String
  def deserialize(value: String): Try[T]
}

object ReSerializable {

  // do not serialize reactives, or containers of reactives.
  implicit def resevent[R <: Reactive[_]]: ReSerializable[R] = doNotSerialize
  implicit def resarray[R <: Reactive[_]]: ReSerializable[Array[R]] = doNotSerialize
  implicit def restrav[T <: Traversable[_ <: Reactive[_]]]: ReSerializable[T] = doNotSerialize
  implicit def resopt[T <: Option[_ <: Reactive[_]]]: ReSerializable[T] = doNotSerialize

  object DoNotSerialize extends ReSerializable[Any] {
    override def serialize(value: Any): String = ???
    override def deserialize(value: String): Try[Any] = ???
  }
  object NoSerializer extends ReSerializable[Any] {
    override def serialize(value: Any): String = ???
    override def deserialize(value: String): Try[Any] = ???
  }

  def doNotSerialize[T]: ReSerializable[T] = DoNotSerialize.asInstanceOf[ReSerializable[T]]
  def serializationUnavailable[T]: _root_.rescala.core.ReSerializable[T] = NoSerializer.asInstanceOf[ReSerializable[T]]


  def pulseSerializable[T](implicit s: ReSerializable[T]): ReSerializable[Pulse[T]] = {
    if (s == null) null else if (s == doNotSerialize) doNotSerialize else if (s == serializationUnavailable) serializationUnavailable
    else new ReSerializable[Pulse[T]] {
      override def serialize(value: Pulse[T]): String = value.toOption.fold("")(s.serialize)
      override def deserialize(value: String): Try[Pulse[T]] = if (value.isEmpty) Success(Pulse.empty)
      else s.deserialize(value).map(Pulse.Value(_))
    }
  }

}


