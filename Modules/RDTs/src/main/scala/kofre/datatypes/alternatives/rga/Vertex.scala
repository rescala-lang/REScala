package kofre.datatypes.alternatives.rga

import kofre.base.{Uid, Time}
import Vertex.Timestamp

case class Vertex(timestamp: Timestamp, id: Uid)

object Vertex {
  type Timestamp = Long

  val start: Vertex = Vertex(-1, Uid.predefined("start"))
  val end: Vertex   = Vertex(0, Uid.predefined("end"))

  def fresh[A](): Vertex = Vertex(Time.current(), Uid.gen())
}
