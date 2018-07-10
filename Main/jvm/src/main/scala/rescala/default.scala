package rescala

import rescala.core.Scheduler
import rescala.parrp.ParRP

/** REScala has two main abstractions. [[rescala.Event]] and [[rescala.Signal]] commonly referred to as reactives.
  * Use [[rescala.Var]] to create signal sources and [[rescala.Evt]] to create event sources.
  *
  * Events and signals can be created from other reactives by using combinators,
  * signals additionally can be created using [[rescala.Signal]] expressions.
  **/
object default extends interface.RescalaInterface[ParRP] {
  override def scheduler: Scheduler[ParRP] = rescala.Schedulers.parrp
}
