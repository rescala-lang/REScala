package de.ckuessner
package counter.actors

import encrdt.lattices.CounterLattice

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

object Counter {
  case class Value(value: Int)

  sealed trait Command

  private case class SynchronizationCommand(cmd: SynchronizationAdapter.Command[CounterLattice]) extends Command

  case class Update(delta: Int) extends Command

  case class Query(replyTo: ActorRef[Value]) extends Command

  def apply(implicit syncServiceKey: ServiceKey[SynchronizationAdapter.Command[CounterLattice]]): Behavior[Command] =
    Behaviors.setup(context =>
      new Counter(
        context,
        new SynchronizationAdapter(context, syncServiceKey, context.system.address.toString, CounterLattice()),
        syncServiceKey
      )
    )

  def apply(replicaId: String,
            syncAdapter: ActorContext[_] => SynchronizationAdapter[CounterLattice],
            syncServiceKey: ServiceKey[SynchronizationAdapter.Command[CounterLattice]]
           ): Behavior[Command] = {

    Behaviors.setup(context => new Counter(context, syncAdapter(context), syncServiceKey))
  }
}

class Counter(context: ActorContext[Counter.Command],
              syncAdapter: SynchronizationAdapter[CounterLattice],
              val syncServiceKey: ServiceKey[SynchronizationAdapter.Command[CounterLattice]])
  extends AbstractBehavior[Counter.Command](context) {

  import Counter._

  private val syncMessageAdapter: ActorRef[SynchronizationAdapter.Command[CounterLattice]] =
    context.messageAdapter(syncCommand => SynchronizationCommand(syncCommand))
  context.system.receptionist ! Receptionist.Register(syncServiceKey, syncMessageAdapter)

  private val joinHandler = context.spawnAnonymous[Receptionist.Listing](
    Behaviors.receiveMessagePartial {
      case syncServiceKey.Listing(reachablePeers: Set[ActorRef[SynchronizationAdapter.Command[CounterLattice]]]) =>
        syncMessageAdapter ! SynchronizationAdapter.PeersChanged(reachablePeers)
        Behaviors.same
    }
  )
  Receptionist.subscribe(syncServiceKey, joinHandler)


  override def onMessage(msg: Counter.Command): Behavior[Counter.Command] = msg match {
    case Update(delta) =>
      syncAdapter.state = syncAdapter.state.updated(syncAdapter.replicaId, delta)
      this
    case Query(replyTo) =>
      replyTo ! Value(syncAdapter.state.query())
      this
    case SynchronizationCommand(syncCmd) =>
      syncAdapter.handleCommand(syncCmd, syncMessageAdapter)
      this
  }
}