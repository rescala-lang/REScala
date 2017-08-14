import akka.actor.ActorRef
import rescala._
import statecrdts.StateCRDT

/**
  * Created by julian on 25.07.17.
  */
package object distributionengine {

  def subscribe[A](name: String, initial: A)(implicit engine: ActorRef): Signal[A] = subscribe(name).map {
    case None => initial
    case a: A => a
  }

  def subscribe(name: String)(implicit engine: ActorRef): Signal[Any] = {
    val evt = Evt[StateCRDT]
    engine ! SubscribeVar(name, evt)
    evt.latestOption().map {
      // return latest crdt or None if we didn't receive a value yet
      case Some(crdt) => crdt.value
      case _ => None
    }
  }

  // Message types:
  final case class PublishVar(varName: String, pVar: Publishable[_ <: StateCRDT])

  final case class PublishReadOnly(varName: String, pVar: Publishable[_ <: StateCRDT])

  final case class SubscribeVar(varName: String, event: Evt[StateCRDT])

  final case class SyncVar(cVar: Publishable[_ <: StateCRDT])

  final case class UpdateMessage(varName: String, crdt: StateCRDT, hostRef: ActorRef)

  final case class QueryMessage(varName: String, host: ActorRef)

  final case class RegisterMessage(varName: String, host: ActorRef)

  // make publishables usable in signal expressions
  implicit def publishableAsSignal[A <: StateCRDT](pub: Publishable[A]): Signal[A#valueType] = pub.valueSignal

  final case class SyncAllMessage()

  final case class SetDelay(time: Long)
}
