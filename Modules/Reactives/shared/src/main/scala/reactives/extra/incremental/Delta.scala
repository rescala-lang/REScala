package reactives.extra.incremental

/** Deltas represent a change that will happen on reactive sequences. This could be adding, removing or no change at all
  * @tparam T type of the value the Delta holds
  */
sealed trait Delta[+T] {

  /** Filters the value of the Delta.
    * If accepted by the filter function the Delta is returned, otherwise NoChange is returned
    *
    * @param accept is the function used to filter.
    * @return the Delta or NoChange if not accepted
    */
  def filter(accept: T => Boolean): Delta[T] =
    this match {
      case a @ Addition(value) if accept(value) => a
      case r @ Removal(value) if accept(value)  => r
      case _                                    => Delta.noChange[T]
    }

  /** Maps the value of the Delta.
    * This will happen for an Addition or Removal only
    * @param mapOperation maps the value of Delta by returning a new one with type A
    * @tparam A the type of the mapped value
    * @return
    */
  def map[A](mapOperation: T => A): Delta[A] =
    this match {
      case Addition(value) => Addition(mapOperation(value))
      case Removal(value)  => Removal(mapOperation(value))
      case _               => Delta.noChange[A]
    }

  /** @return the value the Delta is holding */
  def value: T =
    this match {
      case Addition(v) => v
      case Removal(v)  => v
      case _           => throw new Exception("NoChange have no value")
    }

}

/** @param v the value that is added
  * @tparam T type of the value the Delta holds
  */
case class Addition[T](v: T) extends Delta[T]

/** @param v the value that is removed
  * @tparam T type of the value the Delta holds
  */
case class Removal[T](v: T) extends Delta[T]

/** @tparam T type of the value the Delta holds */
case class NoChange[T]() extends Delta[T]

/** Object representing the Delta trait */
object Delta {
  def noChange[T]: NoChange[T] = NoChange[T]()
}
