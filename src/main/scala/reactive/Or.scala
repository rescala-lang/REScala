package reactive

import scala.annotation.targetName

case class Or[V](left: Event[V], right: Event[V]) extends Event[V] {
  override def inputs: List[ReSource] = List(left, right)
}

extension [V] (e: Event[V])
  @targetName("or")
  def ||(other: Event[V]): Or[V] = Or(e, other)