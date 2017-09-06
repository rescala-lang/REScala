package rescala.crdts

import akka.actor.ActorRef
import rescala._
import rescala.crdts.statecrdts.StateCRDT

import scala.util.hashing.Hashing.Default


package object pvars {
  val Vertex = statecrdts.sequences.Vertex

  object subscribe {
    def apply[A](name: String, default: A)(implicit engine: ActorRef) = DistributionEngine.subscribe(name,default)(engine)
    def apply[A](name: String)(implicit engine: ActorRef) = DistributionEngine.subscribe(name)(engine)
  }

  // make publishables usable in signal expressions
  implicit def publishableAsSignal[A <: StateCRDT](pub: Publishable[A]): Signal[A#valueType] = pub.valueSignal
}
