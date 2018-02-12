package rescala.crdts

import akka.actor.ActorRef
import rescala._

import scala.language.{implicitConversions}


package object pvars {
  object subscribe {
    def apply[A](name: String, default: A)(implicit engine: ActorRef) =  {locally((name, engine)); default}//DistributionEngine.subscribe(name,default)(engine)
//    def apply[A](name: String)(implicit engine: ActorRef) = DistributionEngine.subscribe(name)(engine)
  }

  // make publishables usable in signal expressions
  implicit def publishableAsSignal[A, F](pub: Publishable[A, F]): Signal[A] = pub.valueSignal
}
