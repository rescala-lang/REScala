package reswing

sealed abstract class OptionalArgument[+T] {
  protected def empty: Boolean
  protected def value: T
  @inline def foreach[U](f: T => U) = if (!empty) f(value)
}

final case class ReSwingSome[+T](v: T) extends OptionalArgument[T] {
  protected def empty = false
  protected def value = v
}

final case object ReSwingNone extends OptionalArgument[Nothing] {
  protected def empty = true
  protected def value = throw new NoSuchElementException
}

object OptionalArgument {
  implicit def toReSwingOption[T](v: T) = ReSwingSome(v)
}
