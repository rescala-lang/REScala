package rescala
import rescala.core.Scheduler
import rescala.levelbased.LevelStructImpl

/** REScala has two main abstractions. [[rescala.default.Event]] and [[rescala.default.Signal]] commonly referred to as reactives.
  * Use [[rescala.default.Var]] to create signal sources and [[rescala.default.Evt]] to create event sources.
  *
  * Events and signals can be created from other reactives by using combinators,
  * signals additionally can be created using [[rescala.default.Signal]] expressions.
  **/
object default extends rescala.interface.RescalaInterface[LevelStructImpl] {
  override implicit def scheduler: Scheduler[LevelStructImpl] = rescala.Schedulers.synchron
}
