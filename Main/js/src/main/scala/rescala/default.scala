package rescala
import rescala.core.Scheduler
import rescala.levelbased.SimpleStruct

/** REScala has two main abstractions. [[rescala.Event]] and [[rescala.Signal]] commonly referred to as reactives.
  * Use [[rescala.Var]] to create signal sources and [[rescala.Evt]] to create event sources.
  *
  * Events and signals can be created from other reactives by using combinators,
  * signals additionally can be created using [[rescala.Signal]] expressions.
  **/
object default extends rescala.interface.RescalaInterface[SimpleStruct] {
  override implicit def scheduler: Scheduler[SimpleStruct] = rescala.Schedulers.synchron
}
