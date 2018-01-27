import rescala.core.ReSerializable
import rescala.levelbased.SimpleStruct

/** REScala has two main abstractions. [[rescala.Event]] and [[rescala.Signal]] commonly referred to as reactives.
  * Use [[rescala.Var]] to create signal sources and [[rescala.Evt]] to create event sources.
  *
  * Events and signals can be created from other reactives by using combinators,
  * signals additionally can be created using [[rescala.Signal]] expressions.
  **/
package object rescala extends RescalaInterface[SimpleStruct] {
  override implicit def explicitEngine: Engines.SimpleEngine = rescala.Engines.default
  /** @group internal */
  implicit def doNotSerialize[T]: ReSerializable[T] = ReSerializable.serializationUnavailable

}
