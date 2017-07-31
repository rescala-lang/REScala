import akka.actor.ActorRef
import statecrdts.StateCRDT

/**
  * Created by julian on 25.07.17.
  */
package object distributionengine {

  // Message types:
  final case class PublishVar(cVar: Publishable[_ <: StateCRDT])

  final case class SyncVar(cVar: Publishable[_ <: StateCRDT])

  final case class UpdateMessage(varName: String, payload: Any, hostRef: ActorRef)

  final case class QueryMessage(varName: String, host: ActorRef)

  final case class RegisterMessage(varName: String, host: ActorRef)

  final case class SyncAllMessage()

}
