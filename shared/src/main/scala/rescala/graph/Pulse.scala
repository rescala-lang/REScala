package rescala.graph

import rescala.graph.Pulse.{Change, Exceptional, NoChange, Stable}
import rescala.reactives.RExceptions.EmptySignalControlThrowable

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * Pulse that stores a current value and can also indicate a potentially change to an updated value.
  * A pulse may indicate that no current value has been set yet but updates must always contain a value.
  *
  * @tparam P Stored value type of the Pulse
  */
sealed trait Pulse[+P] {

  /**
    * Checks if the pulse indicates a change
    *
    * @return True if the pulse indicates a change, false if not
    */
  final def isChange: Boolean = this match {
    case NoChange | Stable(_) => false
    case _ => true
  }

  /**
    * If the pulse indicates a change: Applies a function to the updated value of the pulse and returns a new pulse
    * indicating a change to this updated value.
    * If the pulse doesn't indicate a change: Returns an empty pulse indicating no change.
    *
    * @param f Function to be applied on the updated pulse value
    * @tparam Q Result type of the applied function
    * @return Pulse indicating the update performed by the applied function or an empty pulse if there is no updated value
    */
  def map[Q](f: P => Q): Pulse[Q] = this match {
    case Change(value) => Change(f(value))
    case Stable(_) => NoChange
    case NoChange => NoChange
    case ex@Exceptional(_) => ex
  }

  /**
    * If the pulse indicates a change: Applies a function to the updated value. The function has to return a new pulse
    * that is returned by this function.
    * If the pulse doesn't indicate a change: Returns an empty pulse indicating no change.
    *
    * @param f Function to be applied on the updated pulse value
    * @tparam Q Value type of the pulse returned by the applied function
    * @return Pulse returned by the applied function or an empty pulse if there is no updated value
    */
  def flatMap[Q](f: P => Pulse[Q]): Pulse[Q] = this match {
    case Change(value) => f(value)
    case Stable(_) => NoChange
    case NoChange => NoChange
    case ex@Exceptional(_) => ex
  }

  /**
    * If the pulse indicates a change: Applies a filter function to the updated value of the pulse.
    * Based on the filter function, the updated value is retained or an empty pulse is returned.
    * If the pulse doesn't indicate a change: Returns an empty pulse indicating no change.
    *
    * @param p Filter function to be applied to the updated pulse value
    * @return A pulse with the updated pulse value if the filter function returns true, an empty pulse otherwise
    */
  def filter(p: P => Boolean): Pulse[P] = this match {
    case c@Change(value) if p(value) => c
    case Change(_) => NoChange
    case Stable(_) => NoChange
    case NoChange => NoChange
    case ex@Exceptional(_) => ex
  }

  /**
    * If the pulse indicates a change: A new pulse with the updated value set as current value and indicating no
    * change is created.
    * If the pulse doesn't indicate a change: The current pulse is returned without modification.
    *
    * @return New pulse with a potential update set as current value
    */
  /* this is overridden for Change to return Stable */
  def stabilize: Pulse[P] = this

  /** converts the pulse to an option of try */
  def toOptionTry(takeInitialValue: Boolean = false): Option[Try[P]] = this match {
    case Change(up) => Some(Success(up))
    case Stable(current) if takeInitialValue => Some(Success(current))
    case NoChange => None
    case Pulse.empty => None
    case Exceptional(t) => Some(Failure(t))
  }

  def getE: Option[P] = this match {
    case Change(update) => Some(update)
    case NoChange | Stable(_) => None
    case Exceptional(t) => throw t
  }

  def getS(onNoChange: => Nothing): P = this match {
    case Stable(value) => value
    case Change(value) => value
    case Exceptional(t) => throw t
    case NoChange => onNoChange
  }
}


/** Object containing utility functions for using pulses */
object Pulse {
  /**
    * Transforms an optional value into a pulse. If the option doesn't contain a value, an empty pulse indicating no
    * change is returned. Otherwise, a pulse with the option's value set as updated value is returned.
    *
    * @param opt Option to transform into a pulse
    * @tparam P Value type of both option and returned pulse
    * @return Pulse with the option's value set as updated value, or an empty pulse if the option doesn't have a value.
    */
  def fromOption[P](opt: Option[P]): Pulse[P] = opt.fold[Pulse[P]](NoChange)(Change.apply)

  /** Transforms the given values into a pulse indicating change and containing they as current and updated values.
    * If updated and current value are equal, a pulse indicating a stable value is returned. */
  def diff[P](newValue: P, oldValue: P): Pulse[P] = {
    if (newValue == oldValue) Stable(oldValue)
    else Change(newValue)
  }

  /** Transforms the given pulse and an updated value into a pulse indicating a change from the pulse's value to
    * the given updated value. */
  def diffPulse[P](newValue: P, oldPulse: Pulse[P]): Pulse[P] = oldPulse match {
    case NoChange => Change(newValue)
    case Stable(value) => diff(newValue, value)
    case Change(update) => diff(newValue, update)
    case ex@Exceptional(t) => Change(newValue)
  }

  /** wrap a pulse generating function to store everntual exceptions into an exceptional pulse */
  def tryCatch[P](f: => Pulse[P]): Pulse[P] = try f catch {
    case EmptySignalControlThrowable => Pulse.empty
    case NonFatal(t) => Exceptional(t)
  }

  /** the pulse representing an empty signal */
  val empty = Exceptional(EmptySignalControlThrowable)

  /** Pulse indicating a current stable value */
  final case class Stable[+P](value: P) extends Pulse[P]

  /** Pulse indicating no change */
  case object NoChange extends Pulse[Nothing]

  /** Pulse indicating a change
    *
    * @param update Updated value stored by the pulse */
  final case class Change[+P](update: P) extends Pulse[P] {
    override def stabilize: Pulse[P] = Stable(update)
  }

  /** Pulse indicating an exception */
  final case class Exceptional(throwable: Throwable) extends Pulse[Nothing]
}
