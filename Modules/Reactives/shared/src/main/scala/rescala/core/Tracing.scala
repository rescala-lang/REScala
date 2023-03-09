package rescala.core

object Tracing {

  case class ValueWrapper(v: Any)

  sealed trait Data
  case class Create(resource: ReSource, inputs: Set[ReSource], value: ValueWrapper) extends Data
  case class Discover(source: ReSource, sink: ReSource)        extends Data
  case class Drop(source: ReSource, sink: ReSource)            extends Data
  case class Value(source: ReSource, value: ValueWrapper)      extends Data

  var observer: Data => Unit = null

  @inline def observe(data: => Data): Unit = if (observer != null) observer(data)
}
