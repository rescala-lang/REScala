package reactives.extra.incremental

import reactives.core.*
import reactives.operator.*
import reactives.operator.Interface.State

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

/** @tparam T Type of values inside Deltas
  * @tparam S Structure of Reactive Sequence source
  */
trait ReactiveDeltaSeq[T] extends Derived with DisconnectableImpl {

  override protected[reactives] def commit(base: Delta[T]): Delta[T] = Delta.noChange

  /** the value of deltas send through the set */
  override type Value = Delta[T]

  override type State[V] = Interface.State[V]

  /** Returns current ReactiveDeltaSeq as an Event
    *
    * @param ticket a creation ticket as a new event will be created which has the ReactiveDeltaSeq as dependency
    * @return
    */
  def asEvent(using ticket: CreationTicket[State]): Event[Delta[T]] = {
    Event.Impl.static(this) { staticTicket =>
      // each time a change occurs it is represented by a Delta
      // the staticTicket gets this Delta and the Event representing the ReactiveDeltaSeq will fire the Delta
      val delta = staticTicket.collectStatic(this)

      // It can be that when the event is fired no changes have occurred
      // That is why Some(delta) is used
      Some(delta)
    }
  }

  /** Based on the concept of reversible Folds
    * Used to fold the deltas basing on fold for Addition-Delta and unfold for Removal-Delta
    *
    * @param initial is the initial value the foldUndo folds to
    * @param fold    the function used when an Addition occurs
    * @param unfold  the function used when a Removal occurs
    * @param ticket  as we will create
    * @tparam A the value returned by applying fold or unfold on the value T of Deltas
    * @return
    */
  def foldUndo[A](initial: A)(fold: (A, Delta[T]) => A)(unfold: (A, Delta[T]) => A)(using
      ticket: CreationTicket[State]
  ): Signal[A] = {
    // first we create an event that fires each time a Delta occurs
    val event = asEvent

    // Than we fold the event by applying the fold- or unfold-function respectively
    // In case Nothing Changed we return the old value of fold
    // This will automatically create a Signal whose value is updated each time the event fires
//      event.fold(initial)((x: A, y: Delta[T]) => {
//        y match {
//          case Addition(_) => fold(x, y)
//          case Removal(_)  => unfold(x, y)
//          case NoChange()  => x
//        }
//      })
    ???
  }

  //  /**
  //    * Based on the concept of reversible Folds
  //    * Used to fold the deltas basing on fold for Addition-Delta and unfold for Removal-Delta
  //    *
  //    * @param initial is the initial value the foldUndo folds to
  //    * @param fold the function used when an Addition occurs
  //    * @param unfold the function used when a Removal occurs
  //    * @param ticket as we will create
  //    * @tparam A the value returned by applying fold or unfold on the value T of Deltas
  //    * @return
  //    */
  //  @cutOutOfUserComputation
  //  def flatMap[A: ReSerializable] (f: T => ReactiveDeltaSeq[A]) (using ticket: CreationTicket[State]): ReactiveDeltaSeq[A] = {
  //    // first we create an event that fires each time a Delta occurs
  //    val event = asEvent
  //
  //    // Than we fold the event by applying the fold- or unfold-function respectively
  //    // In case Nothing Changed we return the old value of fold
  //    // This will automatically create a Signal whose value is updated each time the event fires
  //    Events.foldOne(event, initial)(
  //      (x: A, y: Delta[T]) => {
  //        y match {
  //          case Addition(_) => fold(x, y)
  //          case Removal(_) => unfold(x, y)
  //          case NoChange() => x
  //        }
  //      }
  //    )
  //  }

  /** Filters the sequence , basing on filterExpression and returns the new filtered sequence
    *
    * @param filterOperation the operation used for filtering
    * @param ticket          for creating the new source
    * @return the filtered ReactiveDeltaSeq
    */
  def filter(filterOperation: T => Boolean)(using ticket: CreationTicket[State]): ReactiveDeltaSeq[T] = {

    // as a new reactive sequence will be returned after filtering we use the creation ticket to create the new source
    // The new created Source will be a FilterDeltaSeq which is basically a ReactiveDeltaSeq, which reevaluates differently when changes are propagated
    // FilterDeltaSeq depends on this (ReactiveDeltaSeq). It is initialized as an empty sequence.
    // Each time a change on ReactiveDeltaSeq occurs, if it passes the filterOperation, it is automatically added to FilterDeltaSeq
    ticket.scope.create[Delta[T], FilterDeltaSeq[T]](Set(this), Delta.noChange, needsReevaluation = false) {
      state => new FilterDeltaSeq[T](this, filterOperation)(state, ticket.info) with DisconnectableImpl
    }
  }

  /** Maps the elements of ReactiveDeltaSeq and returns a new ReactiveDeltaSeq with the mapped deltas with the old ReactiveDeltaSeq as dependency
    *
    * @param mapOperation the operation used for mapping the values of ReactiveDeltaSeq to MapDeltaSeq
    * @param ticket       Ticket for creating the new ReactiveDeltaSeq
    * @tparam A new Value type for deltas in the mapped ReactiveDeltaSeq
    * @return the mapped ReactiveDeltaSeq
    */
  def map[A](mapOperation: T => A)(using ticket: CreationTicket[State]): ReactiveDeltaSeq[A] = {

    // as a new reactive sequence will be returned after mapping we use the creation ticket to create the new source
    // The new created Source will be a MapDeltaSeq which is basically a ReactiveDeltaSeq, which reevaluates differently when changes are propagated
    // MapDeltaSeq depends on this (ReactiveDeltaSeq). It is initialized as an empty sequence.
    // Each time a change on ReactiveDeltaSeq occurs, it is mapped and automatically added to MapDeltaSeq
    ticket.scope.create[Delta[A], MapDeltaSeq[T, A]](Set(this), Delta.noChange, needsReevaluation = false) {
      state => new MapDeltaSeq[T, A](this, mapOperation)(state, ticket.info) with DisconnectableImpl
    }
  }

  /** Concatenates the ReactiveDeltaSeq with another (that) ReactiveDeltaSeq by returning a new ReactiveDeltaSeq (ConcatenateDeltaSeq)
    *
    * @param that   the ReactiveDeltaSeq which will be concatenated with this
    * @param ticket used for the creation of the concatenated ReactiveDeltaSeq
    * @return ConcatenateDeltaSeq
    */
  def ++(that: ReactiveDeltaSeq[T])(using ticket: CreationTicket[State]): ReactiveDeltaSeq[T] = {

    // as a new reactive sequence will be returned after concatenating we use the creation ticket to create the new source
    // The new created Source will be a ConcatenateDeltaSeq which is basically a ReactiveDeltaSeq, which reevaluates differently when changes are propagated
    // ConcatenateDeltaSeq depends on this (ReactiveDeltaSeq) and the one being concatenated (that). It is initialized as an empty sequence.
    // Each time a change on this or that occurs, it is automatically added to ConcatenateDeltaSeq
    ticket.scope.create[Delta[T], ConcatenateDeltaSeq[T]](
      Set(this, that),
      Delta.noChange,
      needsReevaluation = false
    ) {
      state => new ConcatenateDeltaSeq[T](this, that)(state, ticket.info)
    }
  }

  /** Returns the sizeOfSeq of the ReactiveDeltaSeq
    *
    * @param ticket for creating the Signal holding the value of sizeOfSeq
    * @param resInt needed by REScala API for Signal/Event holding Ints //TODO check
    * @return
    */
  def size(using ticket: CreationTicket[State]): Signal[Int] =
    foldUndo(0)((counted: Int, _) => counted + 1)((counted: Int, _) => counted - 1)

  /** Counts number of elements fulfilling the condition provided
    *
    * @param fulfillsCondition the condition values of deltas have to fulfill to be taken in consideration
    * @param ticket            for creating the Signal holding the value of counted elements
    * @param resInt            needed by REScala API for Signal/Event holding Ints
    * @return
    */
  def count(fulfillsCondition: T => Boolean)(using
      ticket: CreationTicket[State]
  ): Signal[Int] =
    foldUndo(0) { (counted, x) => if fulfillsCondition(x.value) then counted + 1 else counted } { (counted, x) =>
      if fulfillsCondition(x.value) then counted - 1 else counted
    }

  /** To check if an element is in the sequence
    *
    * @param element element to search for
    * @param ticket  for creating the Signal holding the boolean value
    * @param resInt  needed by REScala API for Signal/Event holding Ints
    * @return
    */
  def contains(element: T)(using
      ticket: CreationTicket[State],
      ord: Ordering[T]
  ): Signal[Boolean] = { exists { (seqElement: T) => ord.equiv(element, seqElement) } }

  /** To check if elements fulfilling the condition exists
    *
    * @param fulfillsCondition the condition values of deltas have to fulfill to be taken in consideration
    * @param ticket            for creating the Signal holding the boolean value
    * @param resInt            needed by REScala API for Signal/Event holding Ints
    * @return
    */
  def exists(fulfillsCondition: T => Boolean)(using ticket: CreationTicket[State]): Signal[Boolean] = {
    // count all elements fulfilling the condition of existence
    val instancesNumber = count(fulfillsCondition)
    // if more than one found
    Signal.static(instancesNumber)(st => st.dependStatic(instancesNumber) > 0)(using ticket)
  }

  /** @param ticket used for creation of new sources
    * @param ord    the ordering needed to compare values of deltas for finding the minimum
    * @param res    ...
    * @return Signal holding the optional minimum (as it could be None if the seqeunce is empty)
    */
  def min(using ticket: CreationTicket[State], ord: Ordering[T]): Signal[Option[T]] = {
    val minimum = foldUndo(mutable.IndexedSeq.empty[(T, T)])(
      // fold operation
      (trackingSequence: mutable.IndexedSeq[(T, T)], delta: Delta[T]) => {
        if trackingSequence.isEmpty then {
          (delta.value, delta.value) +: trackingSequence
        } else {
          var min = trackingSequence.head._2 // current minimum
          if ord.compare(delta.value, min) < 0 then // update if added element is smaller
            min = delta.value
          (delta.value, min) +: trackingSequence // prepend to the tracking-sequence
        }
      }
    )(
      // unfold operation
      (trackingSequence: mutable.IndexedSeq[(T, T)], delta: Delta[T]) => {
        // index of element, being removed
        val deletionIndex = trackingSequence.indexWhere(element => ord.compare(element._1, delta.value) == 0)
        if deletionIndex < 0 then
          throw new Exception("min: Element not found in the sequence")

        if deletionIndex > 0 then { // must be more than two elements to make sense to change minimum
          var min = trackingSequence(deletionIndex)._2
          if deletionIndex == trackingSequence.size - 1 then // last element
            min = trackingSequence(deletionIndex - 1)._1     // new min will be same as the element on the left
          else
            min =
              trackingSequence(
                deletionIndex + 1
              )._2 // new min will be same as the min stored in the tuple on the right
          breakable {
            for i <- (deletionIndex - 1) to 0 by -1 do {
              val element = trackingSequence(i)
              if ord.compare(element._1, min) < 0 then // if no more update needed, stop
                break()
              trackingSequence.update(i, (element._1, min)) // otherwise update the minimum
            }
          }
        }
        trackingSequence.take(deletionIndex) ++ trackingSequence.drop(deletionIndex + 1)
      }
    )
    Signal.static(minimum)(_.dependStatic(minimum).headOption.map(_._2))
  }

  /** @param ticket used for creation of new sources
    * @param ord    the ordering needed to compare values of deltas for finding the minimum
    * @param res    ...
    * @return Signal holding the optional minimum (as it could be None if the seqeunce is empty)
    */
  def max(using ticket: CreationTicket[State], ord: Ordering[T]): Signal[Option[T]] = {
    val seqMaximum = foldUndo(mutable.IndexedSeq.empty[(T, T)])((seq: mutable.IndexedSeq[(T, T)], delta: Delta[T]) => {
      if seq.isEmpty then {
        (delta.value, delta.value) +: seq
      } else {
        var max = seq.head._2
        if ord.gt(delta.value, max) then
          max = delta.value
        (delta.value, max) +: seq
      }
    })((trackingSequence: mutable.IndexedSeq[(T, T)], delta: Delta[T]) => {
      val deletionIndex = trackingSequence.indexWhere(element => ord.equiv(element._1, delta.value))
      if deletionIndex < 0 then
        throw new Exception("max: Element not found in the sequence")

      if deletionIndex > 0 then { // must be more than two elements to make sense to change maxValue
        var max = trackingSequence(deletionIndex)._2
        if deletionIndex == trackingSequence.size - 1 then // last element
          max = trackingSequence(deletionIndex - 1)._1
        else
          max = trackingSequence(deletionIndex + 1)._2

        // after setting the new min, update the minimum of the elements on the left till minimum has different value
        breakable {
          for i <- (0 until deletionIndex).reverse do {
            val element = trackingSequence(i)
            if ord.gteq(element._1, max) then
              break()
            trackingSequence.update(i, (element._1, max))
          }
        }
      }
      trackingSequence.take(deletionIndex) ++ trackingSequence.drop(deletionIndex + 1)
    })

    Signal.static(seqMaximum)(_.dependStatic(seqMaximum).headOption.map(tuple => tuple._2))
  }

}

/** @param left
  * @param right
  * @param initialState
  * @param name
  * @tparam T Type of values in Deltas
  * @tparam S Structure of Source
  */
class ConcatenateDeltaSeq[T](left: ReactiveDeltaSeq[T], right: ReactiveDeltaSeq[T])(
    initialState: IncSeq.SeqState[T],
    name: ReInfo
) extends Base[Delta[T]](initialState, name)
    with ReactiveDeltaSeq[T] with DisconnectableImpl {

  /** @param input
    * @return
    */
  override protected[reactives] def guardedReevaluate(input: ReIn): Rout = {
    val leftDelta = input.collectStatic(left)
    leftDelta match {
      case NoChange() =>
        val rightDelta = input.collectStatic(right)
        input.withValue(rightDelta)

      case _ =>
        input.withValue(leftDelta)

    }
    input
  }
}

/** Class used for filtering ReactiveDeltaSeq
  *
  * @param in           the ReactiveDeltaSeq to filter
  * @param expression   filterExpression with return type boolean used for filtering elements inside the sequence
  * @param initialState ...
  * @param name         Name of the new filtered sequence
  * @tparam T Value inside Delta
  * @tparam S Structure of Delta
  */
class FilterDeltaSeq[T](in: ReactiveDeltaSeq[T], expression: T => Boolean)(
    initialState: IncSeq.SeqState[T],
    name: ReInfo
) extends Base[Delta[T]](initialState, name) with Derived
    with ReactiveDeltaSeq[T] {

  /** @param input Basing ReIn Ticket filters the ReactiveDeltaSeq using the filterExpression define above. That it uses withValue to write the new Sequence
    * @return Returns the new Sequence
    */
  override protected[reactives] def guardedReevaluate(input: ReIn): Rout = {
    val filteredDeltas = input.collectStatic(in).filter(expression)
    input.withValue(filteredDeltas)
    input
  }
}

/** Class used for filtering ReactiveDeltaSeq
  *
  * @param in           the ReactiveDeltaSeq to filter
  * @param op           mapOperation to map sequence
  * @param initialState ...
  * @param name         Name of the new filtered sequence
  * @tparam T Value inside Delta
  * @tparam S Structure of Delta
  */
class MapDeltaSeq[T, A](in: ReactiveDeltaSeq[T], op: T => A)(
    initialState: IncSeq.SeqState[A],
    name: ReInfo
) extends Base[Delta[A]](initialState, name)
    with ReactiveDeltaSeq[A] {

  /** @param input Basing ReIn Ticket maps the ReactiveDeltaSeq using the fold defined above. That it uses withValue to write the new Sequence
    * @return Returns the new Sequence
    */
  override protected[reactives] def guardedReevaluate(input: ReIn): Rout = {
    val mappedDeltas = input.collectStatic(in).map(op)
    input.withValue(mappedDeltas)
    input
  }
}

///**
//  * Class used for filtering ReactiveDeltaSeq
//  *
//  * @param in           the ReactiveDeltaSeq to filter
//  * @param op           mapOperation to map sequence
//  * @param initialState ...
//  * @param name         Name of the new filtered sequence
//  * @tparam T Value inside Delta
//  * @tparam S Structure of Delta
//  */
//class FlatMapDeltaSeq[T, A]
//(in: ReactiveDeltaSeq[T], f: T => ReactiveDeltaSeq[A,S])
//(initialState: IncSeq.SeqState[A], name: ReName)
//  extends Base[Delta[A]](initialState, name)
//    with ReactiveDeltaSeq[A] {
//
//  /**
//    *
//    * @param input Basing ReIn Ticket maps the ReactiveDeltaSeq using the fold defined above. That it uses withValue to write the new Sequence
//    * @return Returns the new Sequence
//    */
//  override protected[rescala] def reevaluate(input: ReIn): Rout = {
//    val delta = input.collectStatic(in).map(op)
//    input.withValue(mappedDeltas)
//    input
//  }
//}

/** Source events with imperative occurrences
  *
  * @param initialState of by the event
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  */
class IncSeq[T] private[reactives] (initialState: IncSeq.SeqState[T], name: ReInfo)
    extends Base[Delta[T]](initialState, name)
    with ReactiveDeltaSeq[T] {

  private val elements: mutable.Map[T, Int] = mutable.HashMap()

  override protected[reactives] def guardedReevaluate(input: ReIn): Rout = ??? // TODO what comes here...

  def add(value: T)(using fac: Scheduler[State]): Unit =
    fac.forceNewTransaction(this) {
      addInTx(Addition(value))(using _)
    }

  def remove(value: T)(using fac: Scheduler[State]): Unit =
    fac.forceNewTransaction(this) {
      addInTx(Removal(value))(using _)
    }

  def addInTx(delta: Delta[T])(using ticket: AdmissionTicket[State]): Unit = {
    (delta: @unchecked) match {
      case Addition(value) => {
        val counter = elements.getOrElse(value, 0)
        if counter == 0 then
          elements.put(value, 1)
        else
          elements.put(value, counter + 1)
      }
      case Removal(value) => {
        val counter = elements.getOrElse(value, 0)
        if counter > 1 then
          elements.put(value, counter - 1)
        else if counter == 1 then
          elements.remove(value)
        else
          throw new Exception(s"Cannot remove element as it cannot be found")
      }
    }
    ticket.recordChange(new InitialChange[State] {
      override val source: IncSeq.this.type = IncSeq.this
      override def writeValue(b: Delta[T], v: Delta[T] => Unit): Boolean = {
        v(delta)
        true
      }
    })
  }

  def printMap(): Unit = {
    elements.foreach(t => print(s"${t._1}, "))
  }

}

object IncSeq {

  type SeqState[T] = State[Delta[T]]

  def apply[T](using ticket: CreationTicket[State]): IncSeq[T] = empty[T]

  def empty[T](using ticket: CreationTicket[State]): IncSeq[T] = fromDelta(Delta.noChange[T])

  private def fromDelta[T](init: Delta[T])(using ticket: CreationTicket[State]): IncSeq[T] =
    ticket.scope.createSource[Delta[T], IncSeq[T]](init)(new IncSeq[T](
      _,
      ticket.info
    ))
}
