import rescala.core.{Scheduler, ReSerializable}
import rescala.parrp.ParRP

/** REScala has two main abstractions. [[rescala.Event]] and [[rescala.Signal]] commonly referred to as reactives.
  * Use [[rescala.Var]] to create signal sources and [[rescala.Evt]] to create event sources.
  *
  * Events and signals can be created from other reactives by using combinators,
  * signals additionally can be created using [[rescala.Signal]] expressions.
  **/
package object rescala extends RescalaInterface[ParRP] {
  override def explicitEngine: Scheduler[ParRP] = rescala.Engines.parrp
  /** @group internal */
  implicit def noSerialization[T]: ReSerializable[T] = ReSerializable.serializationUnavailable
}
