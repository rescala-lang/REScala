import java.net.InetAddress

package object statecrdts {
  type Identifier = String
  type Removed = Boolean
  type Timestamp = Long

  def sleep(): Unit = Thread sleep 0

  def genId: String = host + "::" + java.util.UUID.nameUUIDFromBytes(BigInt(System.currentTimeMillis).toByteArray)

  def genTimestamp: Timestamp = System.currentTimeMillis

  def host: InetAddress = InetAddress.getLocalHost // hostname + IP

  case class Vertex[+A](val value: A, val timestamp: Timestamp) extends Serializable {
    override def toString: String = s"$value{$timestamp}"
  }

  object Vertex {
    def apply[A](value: A): Vertex[A] = new Vertex(value, genTimestamp)

    case object start2

    val start = Vertex[Any]("start", -1)

    val end = Vertex[Any]("end", 0)
  }

}