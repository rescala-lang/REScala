package rescala.extra.replication

import io.circe._
import kofre.rga.Vertex

import scala.util.Right

object CirceCodecs {

  implicit val vertexEncoder: Encoder[Vertex] =
    Encoder.forProduct2("timestamp", "id")(v => (v.timestamp, v.id))
  implicit val vertexDecoder: Decoder[Vertex] =
    Decoder.forProduct2("timestamp", "id")(Vertex.apply)

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
        // to option not available in 2.11
        io.circe.parser.decode[Vertex](key)(vertexDecoder) match {
          case Right(b) => Some(b)
          case _        => None
        }
      }
    }
  }

}
