import java.net.InetAddress

package object statecrdts {
  type Identifier = String
  type Removed = Boolean
  type Timestamp = Long

  def sleep(): Unit = Thread sleep 0

  def genId: String = host + "::" + java.util.UUID.nameUUIDFromBytes(BigInt(System.currentTimeMillis).toByteArray)
  def genTimestamp: Timestamp = System.currentTimeMillis

  def host: InetAddress = InetAddress.getLocalHost // hostname + IP

  class Vertex[+A](val value: A, val timestamp: Timestamp) extends Serializable{
    override def toString: String = s"$value{$timestamp}"

    override def equals(obj: Any): Boolean = obj match {
      case v: Vertex[A] => v.value == this.value && v.timestamp == this.timestamp
    }
  }

  object Vertex {
    def apply[A](value: A): Vertex[A] = new Vertex(value, genTimestamp)

    case object start2

    case object start extends Vertex[Any](None, -1) {
      override def toString: String = "start"
    }

    case object end extends Vertex[Any](None, 0) {
      override def toString: String = "end"
    }

  }
}