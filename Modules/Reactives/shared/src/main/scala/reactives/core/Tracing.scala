package reactives.core

object Tracing {

  case class ValueWrapper(v: Any)
  case class RawWrapper(v: Any)

  sealed trait Data
  case class Create(resource: ReSource, inputs: Set[ReSource], value: ValueWrapper) extends Data
  case class Discover(source: ReSource, sink: ReSource)                             extends Data
  case class Drop(source: ReSource, sink: ReSource)                                 extends Data
  case class Value(source: ReSource, value: ValueWrapper)                           extends Data
  case class DomAssociation(reSource: ReSource, node: RawWrapper)                   extends Data
  case class Transaction(id: Int, phase: String)                                    extends Data

  var observer: (Data => Unit) | Null = null

  @inline def observe(data: => Data): Unit = if observer != null then observer.nn(data)
}
