package lofi_acl.ardt.base

trait Bottom[T] {
  val empty: T
  def isEmpty(value: T): Boolean = value == empty
}

object Bottom {
  inline def apply[T](using bottom: Bottom[T]): Bottom[T] = bottom
}
