package kofre.rga

import kofre.base.Defs
import kofre.rga.Vertex.Timestamp

case class Vertex(timestamp: Timestamp, id: Defs.Id)

object Vertex {
  type Timestamp = Long

  val start: Vertex = Vertex(-1, Defs.predefined("start"))
  val end: Vertex   = Vertex(0, Defs.predefined("end"))

  def fresh[A](): Vertex = Vertex(Defs.genTimestamp(), Defs.genId())
}
