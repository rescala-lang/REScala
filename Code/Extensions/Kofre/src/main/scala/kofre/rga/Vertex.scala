package kofre.rga

import kofre.IdUtil
import kofre.rga.Vertex.Timestamp

case class Vertex(timestamp: Timestamp, id: IdUtil.Id)

object Vertex {
  type Timestamp = Long

  val start: Vertex = Vertex(-1, "start")
  val end: Vertex   = Vertex(0, "end")

  def fresh[A](): Vertex = Vertex(IdUtil.genTimestamp(), IdUtil.genId())
}
