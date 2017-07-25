import java.net.InetAddress

package object statecrdts {
  type Identifier = String
  type Removed = Boolean
  type Timestamp = Long

  class Vertex[+A](val value: A) {
    override def toString: String = value.toString
  }

  case object start extends Vertex(AnyRef) {
    override def toString: String = "start"
  }

  case object end extends Vertex(AnyRef) {
    override def toString: String = "end"
  }

  object Vertex {
    def apply[A](value: A): Vertex[A] = new Vertex(value)
  }

  def sleep(): Unit = Thread sleep 2000

  def host: InetAddress = InetAddress.getLocalHost // hostname + IP
  def genId: String = host + "::" + java.util.UUID.nameUUIDFromBytes(BigInt(System.currentTimeMillis).toByteArray)
}