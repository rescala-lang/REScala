import rescala.core.ReSerializable

/* workaround to disable serialization in the project */
package object rescala {
  implicit def noSerialization[T]: ReSerializable[T] = rescala.core.ReSerializable.serializationUnavailable
}

