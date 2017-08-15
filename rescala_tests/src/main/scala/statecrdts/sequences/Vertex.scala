package statecrdts
package sequences

import statecrdts.sequences.Vertex.Timestamp

case class Vertex[+A](value: A, timestamp: Timestamp) extends Serializable {
  override def toString: String = s"$value{$timestamp}"
}

object Vertex {
  type Timestamp = Long
  val start: Vertex[Any] = Vertex[Any]("start", -1)
  val end: Vertex[Any] = Vertex[Any]("end", 0)

  def apply[A](value: A): Vertex[A] = new Vertex(value, genTimestamp)

  def genTimestamp: Timestamp = System.currentTimeMillis
}