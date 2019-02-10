package rescala.crdts.statecrdts
package sequences

import io.circe._
import io.circe.generic.extras._
import io.circe.generic.extras.auto._
import rescala.crdts.statecrdts.sequences.Vertex.Timestamp

sealed trait Vertex[+A] {
  def timestamp: Timestamp
}


case object StartVertex extends Vertex[Nothing] {
  override def timestamp: Timestamp = -1
}

case object EndVertex extends Vertex[Nothing] {
  override def timestamp: Timestamp = 0
}

case class ValueVertex[+A](value: A, timestamp: Timestamp, id: String = StateCRDT.genId) extends Vertex[A]

object Vertex {
  type Timestamp = Long

  def apply[A](value: A): ValueVertex[A] = new ValueVertex[A](value, genTimestamp)

  def genTimestamp: Timestamp = System.currentTimeMillis

  implicit val config: Configuration = Configuration.default


  def vertexEncoder[A: Encoder]: Encoder[Vertex[A]] = semiauto.deriveEncoder[Vertex[A]]
  def vertexDecoder[A: Decoder]: Decoder[Vertex[A]] = semiauto.deriveDecoder[Vertex[A]]

  //noinspection ConvertExpressionToSAM
  implicit def vertexKeyEncoder[A: Encoder]: KeyEncoder[Vertex[A]] = {
    new KeyEncoder[Vertex[A]] {
      override def apply(vertex: Vertex[A]): String = {
        vertexEncoder[A].apply(vertex).noSpaces
      }
    }
  }

  //noinspection ConvertExpressionToSAM
  implicit def vertexKeyDecoder[A: Decoder]: KeyDecoder[Vertex[A]] = {
    new KeyDecoder[Vertex[A]] {
      override def apply(key: String): Option[Vertex[A]] = {
        io.circe.parser.decode[Vertex[A]](key)(vertexDecoder[A]).toOption
      }
    }
  }

}
