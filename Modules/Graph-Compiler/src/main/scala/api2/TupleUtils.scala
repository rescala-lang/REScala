package api2

import reactives.core.StaticTicket
import reactives.default.*

type CEventsFromEvents[T <: Tuple] <: Tuple = T match
  case EmptyTuple     => EmptyTuple
  case Event[t] *: ts => CEvent[t] *: CEventsFromEvents[ts]

type TupleFromCEvents[T <: Tuple] <: Tuple = T match
  case EmptyTuple      => EmptyTuple
  case CEvent[t] *: ts => t *: TupleFromCEvents[ts]

type OptionsFromTuple[T <: Tuple] <: Tuple = T match
  case EmptyTuple => EmptyTuple
  case t *: ts    => Option[t] *: OptionsFromTuple[ts]

type OptionsFromEvents[T <: Tuple] <: Tuple = T match
  case EmptyTuple     => EmptyTuple
  case Event[t] *: ts => Option[t] *: OptionsFromEvents[ts]

type TupleFromEvents[T <: Tuple] <: Tuple = T match
  case EmptyTuple     => EmptyTuple
  case Event[t] *: ts => t *: TupleFromEvents[ts]

type EvtsFromTuple[T <: Tuple] <: Tuple = T match
  case EmptyTuple => EmptyTuple
  case t *: ts    => Evt[t] *: EvtsFromTuple[ts]

type Callbacks[T <: Tuple] <: Tuple = T match
  case EmptyTuple => EmptyTuple
  case t *: ts    => (Option[t] => Unit) *: Callbacks[ts]

trait TupleUtils[T <: Tuple]:
  def processCallbacks(t: OptionsFromTuple[T], cbs: Callbacks[T]): Unit
  def createEvtsWithCallbacks: (EvtsFromTuple[T], Callbacks[T])

given TupleUtils[EmptyTuple] with
  def processCallbacks(t: OptionsFromTuple[EmptyTuple], cbs: Callbacks[EmptyTuple]): Unit = ()
  def createEvtsWithCallbacks: (EvtsFromTuple[EmptyTuple], Callbacks[EmptyTuple])         = (EmptyTuple, EmptyTuple)

given [T, TS <: Tuple: TupleUtils]: TupleUtils[T *: TS] with
  def processCallbacks(t: OptionsFromTuple[T *: TS], cbs: Callbacks[T *: TS]): Unit = {
    val cbHead *: cbTail = cbs
    val tHead *: tTail   = t

    cbHead(tHead)
    summon[TupleUtils[TS]].processCallbacks(tTail, cbTail)
  }

  def createEvtsWithCallbacks: (EvtsFromTuple[T *: TS], Callbacks[T *: TS]) = {
    val evt = Evt[T]()
    val cb: Option[T] => Unit = {
      case Some(v) => evt.fire(v)
      case None    =>
    }

    val (evtTail, cbTail) = summon[TupleUtils[TS]].createEvtsWithCallbacks

    (evt *: evtTail, cb *: cbTail)
  }

trait EventTupleUtils[T <: Tuple]:
  def staticAccesses(t: T, ticket: StaticTicket[BundleState]): OptionsFromEvents[T]

given EventTupleUtils[EmptyTuple] with
  def staticAccesses(t: EmptyTuple, ticket: StaticTicket[BundleState]): OptionsFromEvents[EmptyTuple] = EmptyTuple

given [T, TS <: Tuple: EventTupleUtils]: EventTupleUtils[Event[T] *: TS] with
  def staticAccesses(t: Event[T] *: TS, ticket: StaticTicket[BundleState]): OptionsFromEvents[Event[T] *: TS] = {
    val tHead *: tTail = t
    ticket.dependStatic(tHead) *: summon[EventTupleUtils[TS]].staticAccesses(tTail, ticket)
  }

given hasToBeNamed[T, TS <: Tuple: EventTupleUtils]: EventTupleUtils[Evt[T] *: TS] with
  def staticAccesses(t: Evt[T] *: TS, ticket: StaticTicket[BundleState]): OptionsFromEvents[Evt[T] *: TS] = {
    val tHead *: tTail = t
    ticket.dependStatic(tHead) *: summon[EventTupleUtils[TS]].staticAccesses(tTail, ticket)
  }
