package distributionengine

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import rescala._
import rescala.parrp.ParRP
import statecrdts.{CIncOnlyCounter, StateCRDT}

import scala.concurrent.Await
import scala.concurrent.duration._

trait Publishable[A <: StateCRDT] {
  val name: String
  val initial: A
  val internalChanges: Event[A]
  val externalChanges: Event[A]
  lazy val changes: reactives.Event[A, ParRP] = internalChanges || externalChanges
  lazy val signal: Signal[A] = changes.fold(initial) { (c1, c2) =>
    c1.merge(c2) match {
      case a: A => a
    }
  }
  def value: A#valueType = signal.now.value
}

case class DistributedGCounter(name: String, private val start: Int) extends Publishable[CIncOnlyCounter] {
  val initial = CIncOnlyCounter(start)
  val internalChanges: rescala.Evt[CIncOnlyCounter] = Evt[CIncOnlyCounter]
  val externalChanges: rescala.Evt[CIncOnlyCounter] = Evt[CIncOnlyCounter]
  def increase: Int = {
    internalChanges(signal.now.increase)
    value
  }
}

object DistributedGCounter {
  def apply(engine: ActorRef, name: String, start: Int): DistributedGCounter = {
    val c = new DistributedGCounter(name, start)
    implicit val timeout = Timeout(60.second)
    val sendMessage = engine ? PublishEvt(c)
    Await.ready(sendMessage, Duration.Inf) // make publish a blocking operation
    c
  }
}