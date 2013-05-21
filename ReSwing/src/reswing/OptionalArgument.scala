package reswing

/**
 * Represents optional values similar to [[scala.Option]].
 * 
 * This type is intended to be used to pass optional arguments
 * for constructing ReSwing components.
 * Therefore this type supports implicit conversion from
 * the wrapped type `T` to the `OptionalArgument[T]` type.
 */
sealed abstract class OptionalArgument[+T] {
  protected def empty: Boolean
  protected def value: T
  @inline def foreach[U](f: T => U) = if (!empty) f(value)
}

final case class Argument[+T](v: T) extends OptionalArgument[T] {
  protected def empty = false
  protected def value = v
}

final case object NoArgument extends OptionalArgument[Nothing] {
  protected def empty = true
  protected def value = throw new NoSuchElementException
}

object OptionalArgument {
  implicit def toReSwingOption[T](v: T) = Argument(v)
}
