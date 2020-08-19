package rescala.extra.incremental

import rescala.core._
import rescala.interface.RescalaInterface
import rescala.macros.cutOutOfUserComputation
import rescala.reactives.{Event, Signal}



trait Delta[T]
case class Addition[T](value: T) extends Delta[T]
case class Removal[T](value: T) extends Delta[T]



trait ReactiveDeltaSet[T, S <: Struct] extends ReSource[S] {

  /** the value of deltas send through the set */
  override type Value = Delta[T]


  @cutOutOfUserComputation
  def map[A](expression: T => A)(implicit ticket: CreationTicket[S]): ReactiveDeltaSet[A, S] = ???

  @cutOutOfUserComputation
  def filter[A](expression: T => Boolean)(implicit ticket: CreationTicket[S]): ReactiveDeltaSet[A, S] = ???


  @cutOutOfUserComputation
  def asEvent(implicit ticket: CreationTicket[S], api: RescalaInterface[S]): Event[Delta[T], S] = {
    api.Events.static( this) { staticTicket =>
      val delta = staticTicket.collectStatic(this)
      Some(delta)
    }
  }

  @cutOutOfUserComputation
  def aggregate[A](initial: A)(expression: (A, Delta[T]) => A)(implicit ticket: CreationTicket[S], api: RescalaInterface[S]): Signal[A, S] = {
    val event = asEvent
    api.Events.foldOne(event, initial){ expression }
  }

}



/** Source events with imperative occurrences
  *
  * @param initialState of by the event
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  */
final class SetSource[T, S <: Struct] private[rescala](initialState:  S#State[Delta[T], S], name: REName)
  extends Base[Delta[T], S](initialState, name) with ReactiveDeltaSet[T, S] {

  def add(value: T)(implicit fac: Scheduler[S]): Unit =
    fac.forceNewTransaction(this) {
      addInTx(Addition(value))(_)
    }

  def addInTx(delta: Delta[T])(implicit ticket: AdmissionTicket[S]): Unit = {
    ticket.recordChange(new InitialChange[S] {
      override val source = SetSource.this
      override def writeValue(base: Delta[T], writeCallback: Delta[T] => Unit): Boolean = {writeCallback(delta); true}
    })
  }
}

