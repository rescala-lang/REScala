package replication

import rdts.base.Uid
import rdts.time.Dots

object ProtocolMessage {

  /** `knows` has to be a subset of the dots known at the sender.
    * The sender of the request should then eventually receive all known missing dots.
    */
  case class Request(sender: Uid, knows: Dots) extends ProtocolMessage[Nothing]

  /** Guarantees that for two payloads a and b, that if a.dots <= b.dots,
    * then a.data <= b.data according to the lattice of T
    */
  case class Payload[+T](senders: Set[Uid], dots: Dots, data: T) extends ProtocolMessage[T] {
    def addSender(s: Uid) = copy(senders = senders + s)
  }
  object Payload {
    def apply[T](sender: Uid, dots: Dots, data: T): Payload[T] = Payload(Set(sender), dots, data)

    // this kinda makes sense, but kinda does not
    // given [T: Lattice]: Lattice[Payload[T]] = Lattice.derived
  }

  case class Ping(time: Long) extends ProtocolMessage[Nothing]
  case class Pong(time: Long) extends ProtocolMessage[Nothing]
}

sealed trait ProtocolMessage[+T]
