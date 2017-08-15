import akka.actor.ActorRef
import pvars.distributionengine.DistributionEngine.SubscribeVar
import rescala._
import statecrdts.StateCRDT

package object pvars {
  val DistributionEngine = distributionengine.DistributionEngine
  val Vertex = statecrdts.sequences.Vertex

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

  // make publishables usable in signal expressions
  implicit def publishableAsSignal[A <: StateCRDT](pub: Publishable[A]): Signal[A#valueType] = pub.valueSignal
}