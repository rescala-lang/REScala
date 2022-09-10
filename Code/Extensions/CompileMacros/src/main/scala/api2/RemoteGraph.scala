package rescala.api2

import rescala.default.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.github.plokhotnyuk.jsoniter_scala.core.*

type OptionsFromTuple[T <: Tuple] <: Tuple = T match
  case EmptyTuple => EmptyTuple
  case t *: ts => Option[t] *: OptionsFromTuple[ts]

type OptionsFromEvents[T <: Tuple] <: Tuple = T match
  case EmptyTuple => EmptyTuple
  case Event[t] *: ts => Option[t] *: OptionsFromEvents[ts]

type EvtsFromTuple[T <: Tuple] <: Tuple = T match
  case EmptyTuple => EmptyTuple
  case t *: ts => Evt[t] *: EvtsFromTuple[ts]

type Callbacks[T <: Tuple] <: Tuple = T match
  case EmptyTuple => EmptyTuple
  case t *: ts => (Option[t] => Unit) *: Callbacks[ts]

trait TupleUtils[T <: Tuple]:
  def processCallbacks(t: OptionsFromTuple[T], cbs: Callbacks[T]): Unit
  def createEvtsWithCallbacks: (EvtsFromTuple[T], Callbacks[T])

given TupleUtils[EmptyTuple] with
  def processCallbacks(t: OptionsFromTuple[EmptyTuple], cbs: Callbacks[EmptyTuple]): Unit = ()
  def createEvtsWithCallbacks: (EvtsFromTuple[EmptyTuple], Callbacks[EmptyTuple]) = (EmptyTuple, EmptyTuple)

given [T, TS <: Tuple: TupleUtils]: TupleUtils[T *: TS] with
  def processCallbacks(t: OptionsFromTuple[T *: TS], cbs: Callbacks[T *: TS]): Unit = {
    val cbHead *: cbTail = cbs
    val tHead *: tTail = t

    cbHead(tHead)
    summon[TupleUtils[TS]].processCallbacks(tTail, cbTail)
  }

  def createEvtsWithCallbacks: (EvtsFromTuple[T *: TS], Callbacks[T *: TS]) = {
    val evt = Evt[T]()
    val cb: Option[T] => Unit = {
      case Some(v) => evt.fire(v)
      case None =>
    }

    val (evtTail, cbTail) = summon[TupleUtils[TS]].createEvtsWithCallbacks

    (evt *: evtTail, cb *: cbTail)
  }

trait EventTupleUtils[T <: Tuple]:
  def staticAccesses(t: T, ticket: StaticTicket): OptionsFromEvents[T]

given EventTupleUtils[EmptyTuple] with
  def staticAccesses(t: EmptyTuple, ticket: StaticTicket): OptionsFromEvents[EmptyTuple] = EmptyTuple

given [T, TS <: Tuple: EventTupleUtils]: EventTupleUtils[Event[T] *: TS] with
  def staticAccesses(t: Event[T] *: TS, ticket: StaticTicket): OptionsFromEvents[Event[T] *: TS] = {
    val tHead *: tTail = t
    ticket.dependStatic(tHead) *: summon[EventTupleUtils[TS]].staticAccesses(tTail, ticket)
  }

given named[T, TS <: Tuple: EventTupleUtils]: EventTupleUtils[Evt[T] *: TS] with
  def staticAccesses(t: Evt[T] *: TS, ticket: StaticTicket): OptionsFromEvents[Evt[T] *: TS] = {
    val tHead *: tTail = t
    ticket.dependStatic(tHead) *: summon[EventTupleUtils[TS]].staticAccesses(tTail, ticket)
  }

trait RemoteGraph {
  protected var connector: Option[RemoteGraphConnector] = None

  def setConnector(c: RemoteGraphConnector): Unit = if connector.isEmpty then connector = Some(c)
}

trait RemoteGraphWithInput[IN <: Tuple: EventTupleUtils](using JsonValueCodec[OptionsFromEvents[IN]]) extends RemoteGraph {
  val events: IN

  def startObserving(): Unit = {
    val dependencies = events.toList.map(_.asInstanceOf[ReSource])
    val grouped = Events.static(dependencies: _*) { t =>
      Some(summon[EventTupleUtils[IN]].staticAccesses(events, t))
    }

    grouped.observe(v => connector.get.write(v))
  }
}

trait RemoteGraphWithOutput[OUT <: Tuple: TupleUtils](using JsonValueCodec[OptionsFromTuple[OUT]]) extends RemoteGraph {
  def eventsFromListen(): EvtsFromTuple[OUT] = {
    val (evts, cbs) = summon[TupleUtils[OUT]].createEvtsWithCallbacks

    connector.get.read[OptionsFromTuple[OUT]] { v =>
      summon[TupleUtils[OUT]].processCallbacks(v, cbs)
    }

    evts
  }
}

trait RemoteGraphWithIO[IN <: Tuple: EventTupleUtils, OUT <: Tuple: TupleUtils]
(using JsonValueCodec[OptionsFromEvents[IN]], JsonValueCodec[OptionsFromTuple[OUT]])
  extends RemoteGraphWithInput[IN] with RemoteGraphWithOutput[OUT]
