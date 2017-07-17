package stateCrdts

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import rescala._
import rescala.parrp.ParRP
import stateCrdts.DistributionEngine._

import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Created by julian on 17.07.17.
  */
class CVar[A <: StateCRDT](val name: String, private val initial: A) {
  val internalChanges: Evt[A] = Evt[A]()
  val externalChanges: Evt[A] = Evt[A]()
  val changes: reactives.Event[A, ParRP] = internalChanges || externalChanges
  val toSignal: Signal[A] = changes.fold(initial) { (c1, c2) =>
    c1.merge(c2) match {
      case a: A => a
    }
  }

  def value: A#valueType = toSignal.now.value
}

case class CCounter(override val name: String, private val start: Int) extends CVar[CIncOnlyCounter](name, CIncOnlyCounter(start)) {
  def increase: Int = {
    println("Sending increase event")
    internalChanges(toSignal.now.increase)
    value
  }
}

object CCounter {
  def apply(engine: ActorRef, name: String, start: Int): CCounter = {
    val c = new CCounter(name, start)
    implicit val timeout = Timeout(60.second)
    val sendMessage = engine ? PublishEvt(c.asInstanceOf[CVar[StateCRDT]])
    Await.ready(sendMessage, Duration.Inf) // make publish a blocking operation
    c
  }
}