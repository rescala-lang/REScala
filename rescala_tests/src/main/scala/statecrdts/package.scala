import java.net.InetAddress

package object statecrdts {
  type Identifier = String
  type Removed = Boolean
  type Timestamp = Long

  def sleep(): Unit = Thread sleep 2000

  def genId: String = host + "::" + java.util.UUID.nameUUIDFromBytes(BigInt(System.currentTimeMillis).toByteArray)

  def host: InetAddress = InetAddress.getLocalHost // hostname + IP

  class Vertex[+A](val value: A) {
    override def toString: String = value.toString
  }

  object Vertex {
    def apply[A](value: A): Vertex[A] = new Vertex(value)

    case object start2

    case object start extends Vertex[Any](None) {
      override def toString: String = "start"
    }

    case object end extends Vertex[Any](None) {
      override def toString: String = "end"
    }

  }
}