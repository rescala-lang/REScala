import java.net.InetAddress

package object statecrdts {
  type Identifier = String
  type Removed = Boolean
  type Timestamp = Long

  def genId: String = host + "::" + java.util.UUID.nameUUIDFromBytes(BigInt(System.currentTimeMillis).toByteArray)

  def host: InetAddress = InetAddress.getLocalHost // hostname + IP

  def genTimestamp: Timestamp = System.currentTimeMillis

  case class Vertex[+A](value: A, timestamp: Timestamp) extends Serializable {
    override def toString: String = s"$value{$timestamp}"
  }

  class Vertex2[A](value: A) {
    var next: Vertex2[A] = this

    def this(value: A, next: Vertex2[A]) {
      this(value)
      this.next = next
    }
  }

  object Vertex {
    val start: Vertex[Any] = Vertex[Any]("start", -1)
    val end: Vertex[Any] = Vertex[Any]("end", 0)

    def apply[A](value: A): Vertex[A] = new Vertex(value, genTimestamp)

    case object start2
  }

}