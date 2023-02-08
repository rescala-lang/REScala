package rescala.core

object Tracing {

  sealed trait Data
  case class Create(resource: ReSource, inputs: Set[ReSource]) extends Data
  case class Discover(source: ReSource, sink: ReSource)        extends Data
  case class Drop(source: ReSource, sink: ReSource)            extends Data
  case class Value(source: ReSource, value: String)            extends Data

  var observer: Data => Unit = null

  @inline def observe(data: => Data): Unit = if (observer != null) observer(data)
}
