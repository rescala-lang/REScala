package ex2013reswing

/** Provides lazy values that can be checked for if they already hold a defined
  * value, i.e. if they have already been accessed
  */
final class Lazy[+T](init: => T) {
  private var defined    = false
  private lazy val value = init

  def isDefined  = defined
  def apply(): T = { defined = true; value }
}

object Lazy {
  def apply[T](value: => T) = new Lazy(value)
}
