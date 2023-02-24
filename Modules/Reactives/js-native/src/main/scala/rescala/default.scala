package rescala
import rescala.interface.RescalaInterface
import rescala.core.Scheduler

/** REScala has two main abstractions. [[rescala.default.Event]] and [[rescala.default.Signal]] commonly referred to as reactives.
  * Use [[rescala.default.Var]] to create signal sources and [[rescala.default.Evt]] to create event sources.
  *
  * Events and signals can be created from other reactives by using combinators,
  * signals additionally can be created using [[rescala.default.Signal]] expressions.
  */
object default extends RescalaInterface {
  val bundle = new rescala.Schedulers.Unmanaged {}
  override type State[V] = bundle.State[V]
  override def scheduler: Scheduler[State] = bundle.scheduler
}
