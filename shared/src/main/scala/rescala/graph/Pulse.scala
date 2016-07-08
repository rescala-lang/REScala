package rescala.graph

/**
  * Pulse that stores a current value and can also indicate a potentially change to an updated value.
  * A pulse may indicate that no current value has been set yet but updates must always contain a value.
  *
  * @tparam P Stored value type of the Pulse
  */
sealed trait Pulse[+P] {

  import rescala.graph.Pulse._

  /**
    * Applies a function to the updated value of the pulse if it indicates a change, or another default function if not.
    *
    * @param ifNone Default function applied if the pulse indicates no change
    * @param ifChange Function to be applied on the pulse's update if it indicates a change
    * @tparam Q Result type of both functions
    * @return Result of the applied function
    */
  def fold[Q](ifNone: => Q, ifChange: P => Q): Q

  /**
    * Extracts the current value of the pulse if it contains one.
    *
    * @return Option of the current pulse value
    */
  def current: Option[P]

  /**
    * Extracts the updated value of the pulse if it indicates a change or no value if not.
    *
    * @return Option of the updated pulse value
    */
  final def toOption: Option[P] = fold(None, Some.apply)

  /**
    * Checks if the pulse indicates a change
    *
    * @return True if the pulse indicates a change, false if not
    */
  final def isChange: Boolean = fold(false, _ => true)

  /**
    * If the pulse indicates a change: Applies a function to the updated value of the pulse and returns a new pulse
    *   indicating a change to this updated value.
    * If the pulse doesn't indicate a change: Returns an empty pulse indicating no change.
    *
    * @param f Function to be applied on the updated pulse value
    * @tparam Q Result type of the applied function
    * @return Pulse indicating the update performed by the applied function or an empty pulse if there is no updated value
    */
  // TODO: Is the loss of the current value actually intended behavior?
  final def map[Q](f: P => Q): Pulse[Q] = fold(none, f.andThen(change))

  /**
    * If the pulse indicates a change: Applies a function to the updated value. The function has to return a new pulse
    *   that is returned by this function.
    * If the pulse doesn't indicate a change: Returns an empty pulse indicating no change.
    *
    * @param f Function to be applied on the updated pulse value
    * @tparam Q Value type of the pulse returned by the applied function
    * @return Pulse returned by the applied function or an empty pulse if there is no updated value
    */
  final def flatMap[Q](f: P => Pulse[Q]): Pulse[Q] = fold(none, f)

  /**
    * If the pulse indicates a change: Applies a filter function to the updated value of the pulse.
    *   Based on the filter function, the updated value is retained or an empty pulse is returned.
    * If the pulse doesn't indicate a change: Returns an empty pulse indicating no change.
    *
    * @param p Filter function to be applied to the updated pulse value
    * @return A pulse with the updated pulse value if the filter function returns true, an empty pulse otherwise
    */
  // TODO: Is the loss of the current value actually intended behavior?
  final def filter(p: P => Boolean): Pulse[P] = fold(none, up => if (p(up)) change(up) else none)

  /**
    * If the pulse indicates a change: A new pulse with the updated value set as current value and indicating no
    *   change is created.
    * If the pulse doesn't indicate a change: The current pulse is returned without modification.
    *
    * @return New pulse with a potential update set as current value
    */
  final def keep: Pulse[P] = fold(this, unchanged)
}

/**
  * Object containing utility functions for using pulses
  */
object Pulse {
  /**
    * Transforms an optional value into a pulse. If the option doesn't contain a value, an empty pulse indicating no
    * change is returned. Otherwise, a pulse with the option's value set as updated value is returned.
    *
    * @param opt Option to transform into a pulse
    * @tparam P Value type of both option and returned pulse
    * @return Pulse with the option's value set as updated value, or an empty pulse if the option doesn't have a value.
    */
  def fromOption[P](opt: Option[P]): Pulse[P] = opt.fold[Pulse[P]](NoChange())(Diff(_))

  /**
    * Transforms the given value into a pulse indicating change and containing it as updated value.
    *
    * @param value Value to to transform into a pulse
    * @tparam P Type of the value
    * @return Pulse containing the given value as updated value
    */
  def change[P](value: P): Pulse[P] = Diff(value)

  /**
    * Transforms the given value into a pulse indicating no change and containing it as current value.
    *
    * @param value Value to to transform into a pulse
    * @tparam P Type of the value
    * @return Pulse containing the given value as current value
    */
  def unchanged[P](value: P): Pulse[P] = NoChange(Some(value))

  /**
    * Transforms the given values into a pulse indicating change and containing they as current and updated values.
    * The current value may also be null, resulting in it being marked as unset in the pulse.
    * If updated and current value are equal, a pulse indicating no change is returned.
    *
    * @param newValue Value to store as updated value in the pulse
    * @param oldValue Value to store as current value in the pulse
    * @tparam P Type of the values
    * @return Pulse storing the given values as updated and current value.
    */
  def diff[P](newValue: P, oldValue: P): Pulse[P] =
    if (null == oldValue) change(newValue)
    else if (newValue == oldValue) unchanged(oldValue)
    else Diff(newValue, Some(oldValue))

  /**
    * Transforms the given pulse and an updated value into a pulse indicating a change from the pulse's value to
    * the given updated value.
    *
    * @param newValue Value to store as updated value in the pulse
    * @param oldPulse Pulse to use as current value in the pulse
    * @tparam P Type of the pulse and the value
    * @return Pulse storing the given value as updated and the old pulse's value as current value.
    */
  def diffPulse[P](newValue: P, oldPulse: Pulse[P]): Pulse[P] = oldPulse match {
    case NoChange(None) => change(newValue)
    case NoChange(Some(value)) => diff(newValue, value)
    case Diff(update, current) => diff(newValue, update)
  }

  /**
    * Empty pulse indicating no change and storing no current value
    */
  val none: Pulse[Nothing] = NoChange()

  /**
    * Pulse indicating no change and only storing a single current value.
    *
    * @param current Current value stored by the pulse
    * @tparam P Stored value type of the Pulse
    */
  final case class NoChange[+P](override val current: Option[P] = None) extends Pulse[P] {
    override def fold[Q](ifNone: => Q, ifChange: (P) => Q): Q = ifNone
  }

  /**
    * Pulse indicating a change from the stored current value to a new updated value
    * @param update Updated value stored by the pulse
    * @param current Current value stored by the pulse
    * @tparam P Stored value type of the Pulse
    */
  final case class Diff[+P](update: P, override val current: Option[P] = None) extends Pulse[P] {
    override def fold[Q](ifNone: => Q, ifChange: (P) => Q): Q = ifChange(update)
  }
}
