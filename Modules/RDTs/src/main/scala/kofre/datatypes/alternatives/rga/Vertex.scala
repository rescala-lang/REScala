package kofre.datatypes.alternatives.rga

import kofre.base.Uid
import Vertex.Timestamp
import kofre.time.Time

case class Vertex(timestamp: Timestamp, id: Uid)

object Vertex {
  type Timestamp = Long

  val start: Vertex = Vertex(-1, Uid.predefined("start"))
  val end: Vertex   = Vertex(0, Uid.predefined("end"))

  def fresh[A](): Vertex = Vertex(Time.current(), Uid.gen())
}
