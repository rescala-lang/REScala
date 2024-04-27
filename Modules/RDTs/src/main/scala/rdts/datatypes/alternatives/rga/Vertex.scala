package rdts.datatypes.alternatives.rga

import rdts.base.Uid
import rdts.datatypes.alternatives.rga.Vertex.Timestamp
import rdts.time.Time

case class Vertex(timestamp: Timestamp, id: Uid)

object Vertex {
  type Timestamp = Long

  val start: Vertex = Vertex(-1, Uid.predefined("start"))
  val end: Vertex   = Vertex(0, Uid.predefined("end"))

  def fresh[A](): Vertex = Vertex(Time.current(), Uid.gen())
}
