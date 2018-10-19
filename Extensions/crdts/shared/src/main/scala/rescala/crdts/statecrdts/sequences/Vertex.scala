package rescala.crdts.statecrdts
package sequences

import io.circe._
import rescala.crdts.statecrdts.sequences.Vertex.Timestamp

sealed trait Vertex[+A] {
  val timestamp: Timestamp
  val id: String = StateCRDT.genId

  override def equals(obj: scala.Any): Boolean = obj match {
    case v: Vertex[A] => v.id == this.id
    case _ => false
  }
}

case object startVertex extends Vertex[Nothing] {
  override val timestamp: Timestamp = -1
  override val id = "start"
  override def toString = "start"
}

case object endVertex extends Vertex[Nothing] {
  override val timestamp: Timestamp = 0
  override val id = "end"
  override def toString = "end"
}

case class ValueVertex[+A](value: A, timestamp: Timestamp) extends Vertex[A] {
  override def toString: String = s"$value{$timestamp}"
}

object Vertex {
  type Timestamp = Long

  def apply[A](value: A): ValueVertex[A] = new ValueVertex[A](value, genTimestamp)

  def genTimestamp: Timestamp = System.currentTimeMillis

  /*
  implicit def vertexEncoder[A: Encoder] = new Encoder[Vertex[A]] {
    final def apply(v: Vertex[A]): Json = {
      val value: Json = v match {
        case startVertex => Json.fromString("start")
        case endVertex => Json.fromString("end")
        case _ => v.value match {
          case value => value.asJson
        }
      }
      Json.obj(("value", value), ("timestamp", v.timestamp.asJson))
    }
  }
    */

  /*
  implicit def vertexDecoder[A: Decoder] = new Decoder[Vertex[A]] {
    final def apply(c: HCursor): Decoder.Result[Vertex[A]] = c.downField()
      for {
        value <- c.downField("value").as[A]
        timestamp <- c.downField("timestamp").as[Timestamp]
      } yield {
        timestamp match {
          case -1 => Vertex.start[A]
          case 0 => Vertex.end[A]
          case _ => new Vertex[A](Some(value), timestamp)
        }
      }
  }
    */


  //noinspection ConvertExpressionToSAM
  implicit def vertexKeyEncoder[A]: KeyEncoder[Vertex[A]] = {
    new KeyEncoder[Vertex[A]] {
      override def apply(vertex: Vertex[A]): String = vertex match {
        case `startVertex` => "start(-1)"
        case `endVertex` => "end(0)"
        case v: ValueVertex[A] => s"${v.value}(${v.timestamp})"
      }
    }
  }

  //noinspection ConvertExpressionToSAM
  implicit def vertexKeyDecoder[A]: KeyDecoder[Vertex[A]] = {
    new KeyDecoder[Vertex[A]] {
      override def apply(key: String): Option[Vertex[A]] = {
        val a: Array[String] = key.split("\\(|\\)")
        a(1).toLong match {
          case -1 => Some(startVertex)
          case 0 => Some(endVertex)
          case _ => Some(new ValueVertex[A]
          (a(0).asInstanceOf[A], a(1).toLong))
        }
      }
    }
  }

}
