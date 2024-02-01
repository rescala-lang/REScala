package loci
package serializer

import scala.annotation.{compileTimeOnly, implicitNotFound}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.util.Try

@implicitNotFound("${T} is not serializable")
trait Serializable[T] {
  def serialize(value: T): MessageBuffer
  def deserialize(value: MessageBuffer): Try[T]
}

object Serializable {
  @inline def apply[T](implicit serializable: Serializable[T]): Serializable[T] =
    serializable

  @compileTimeOnly("Value is not serializable")
  final implicit def resolutionFailure[T, SerializableFallback[T]]: SerializableFallback[T] =
    macro SerializableResolutionFailure[T]

  @compileTimeOnly("Value is not serializable")
  final def dummy[T]: Serializable[T] = throw new NotImplementedError
}

object SerializableResolutionFailure {
  def apply[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val serializableType = weakTypeOf[Serializable[T]]
    val tpe = serializableType.typeArgs.head
    val message = s"$tpe is not serializable${utility.implicitHints.values(c)(serializableType)}"

    q"""{
      @${termNames.ROOTPKG}.scala.annotation.compileTimeOnly($message) def resolutionFailure() = ()
      resolutionFailure()
      ${termNames.ROOTPKG}.loci.serializer.Serializable.dummy[$tpe]
    }"""
  }
}
