package kofre.sequences

import kofre.IdUtil
import kofre.sequences.Vertex.Timestamp

case class Vertex(timestamp: Timestamp, id: IdUtil.Id)

object Vertex {
  type Timestamp = Long

  val start: Vertex = Vertex(-1, "start")
  val end: Vertex   = Vertex(0, "end")

  def fresh[A](): Vertex = Vertex(IdUtil.genTimestamp(), IdUtil.genId())
}
