package rescala.crdts.statecrdts
package sequences

import io.circe._
import rescala.crdts.statecrdts.sequences.Vertex.Timestamp

case class Vertex[+A](value: Option[A], timestamp: Timestamp) {
  override def toString: String = value match {
    case Some(a) => s"$a{$timestamp}"
    case _ => s"None{$timestamp}"
  }
}

object Vertex {
  type Timestamp = Long

  def start[A]: Vertex[A] = new Vertex(None, -1) {
    override def toString = "start"
  }

  def end[A]: Vertex[A] = new Vertex(None, 0) {
    override def toString = "end"
  }

  def apply[A](value: A): Vertex[A] = new Vertex[A](Some(value), genTimestamp)

  def genTimestamp: Timestamp = System.currentTimeMillis

  //noinspection ConvertExpressionToSAM
  implicit def vertexKeyEncoder[A]: KeyEncoder[Vertex[A]] = {
    new KeyEncoder[Vertex[A]] {
      override def apply(vertex: Vertex[A]): String = vertex.toString
    }
  }

  //noinspection ConvertExpressionToSAM
  implicit def vertexKeyDecoder[A]: KeyDecoder[Vertex[A]] = {
    new KeyDecoder[Vertex[A]] {
      override def apply(key: String): Option[Vertex[A]] = {
        val a: Array[String] = key.split("\\{|\\}")
        println(a.toList)
        a(0) match {
          case "start" => Some(Vertex.start[A])
          case "end" => Some(Vertex.end[A])
          case _ => Some(Vertex[A]
            (Some(a(0).asInstanceOf[A]), a(1).toLong))
        }
      }
    }
  }

}
