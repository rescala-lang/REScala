import akka.actor.ActorRef
import statecrdts.StateCRDT

/**
  * Created by julian on 25.07.17.
  */
package object distributionengine {

  // Message types:
  final case class PublishEvt(cVar: Publishable[_ <: StateCRDT])

  final case class UpdateMessage(varName: String, value: StateCRDT, hostRef: ActorRef)

  final case class QueryMessage(varName: String)

  final case class RegisterMessage(varName: String, host: ActorRef)

  final case class LookupMessage(varName: String)

}
