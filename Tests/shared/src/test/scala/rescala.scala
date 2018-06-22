import rescala.core.ReSerializable

package object rescala {
  implicit def noSerialization[T]: ReSerializable[T] = rescala.core.ReSerializable.serializationUnavailable
}
