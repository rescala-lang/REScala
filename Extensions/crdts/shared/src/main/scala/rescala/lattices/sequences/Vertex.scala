package rescala.lattices.sequences

import io.circe._
import io.circe.generic.extras._
import rescala.lattices.IdUtil
import rescala.lattices.sequences.Vertex.Timestamp


case class Vertex(timestamp: Timestamp, id: IdUtil.Id)

object Vertex {
  type Timestamp = Long

  val start: Vertex = Vertex(-1, "start")
  val end: Vertex = Vertex(0, "end")

  def fresh[A](): Vertex = Vertex( IdUtil.genTimestamp, IdUtil.genId)

  implicit val config: Configuration = Configuration.default

  implicit val vertexEncoder: Encoder[Vertex] = semiauto.deriveEncoder[Vertex]
  implicit val vertexDecoder: Decoder[Vertex] = semiauto.deriveDecoder[Vertex]

  implicit val vertexKeyEncoder: KeyEncoder[Vertex] = {
    new KeyEncoder[Vertex] {
      override def apply(vertex: Vertex): String = {
        vertexEncoder.apply(vertex).noSpaces
      }
    }
  }

  implicit val vertexKeyDecoder: KeyDecoder[Vertex] = {
    new KeyDecoder[Vertex] {
      override def apply(key: String): Option[Vertex] = {
        io.circe.parser.decode[Vertex](key)(vertexDecoder).toOption
      }
    }
  }

}
