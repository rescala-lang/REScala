package rescala.crdts.statecrdts
package sequences

import rescala.crdts.statecrdts.sequences.Vertex.Timestamp

sealed trait Vertex {
  val timestamp: Timestamp
}

case object startVertex extends Vertex {
  override val timestamp: Timestamp = -1
}
case object endVertex extends Vertex {
  override val timestamp: Timestamp = 0
}

case class ValueVertex[+A](value: A, timestamp: Timestamp) extends Vertex {
  override def toString: String = s"$value{$timestamp}"
}

object Vertex {
  type Timestamp = Long

  def start: startVertex.type = startVertex
  def end: endVertex.type = endVertex

  def apply[A](value: A): ValueVertex[A] = new ValueVertex(value, genTimestamp)

  def genTimestamp: Timestamp = System.currentTimeMillis
}

