package rescala.parrp

import java.util

import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph._
import rescala.locking._
import rescala.propagation.Turn
import rescala.twoversion.{CommonPropagationImpl, PropagationStructImpl}

import scala.collection.mutable

trait LSStruct extends GraphStruct {
  override type Type[P, S <: Struct] = LSPropagationStruct[P, S]
  override type Ticket[S <: Struct] = ATicket[S]

}


class LSPropagationStruct[P, S <: Struct](current: P, transient: Boolean, val lock: TurnLock[LSInterTurn], initialIncoming: Set[Reactive[S]])
  extends PropagationStructImpl[P, S](current, transient, initialIncoming) {

  override def set(value: P)(implicit ticket: S#Ticket[S]): Unit = {
    val turn = ticket.turn()
    assert(turn match {
      case pessimistic: LockSweep =>
        val wlo: Option[Key[LSInterTurn]] = Option(lock).map(_.getOwner)
        assert(wlo.fold(true)(_ eq pessimistic.key),
          s"buffer owned by $owner, controlled by $lock with owner ${wlo.get}" +
            s" was written by $turn who locks with ${pessimistic.key}, by now the owner is ${lock.getOwner}")
        true
      case _ =>
        throw new IllegalStateException(s"locksweep buffer used with wrong turn")
    })
    super.set(value)
  }

  var willWrite: LockSweep = null
  var hasWritten: LockSweep = null
  var counter: Int = 0
  var anyInputChanged: LockSweep = null
  var hasChanged: LockSweep = null
}

trait LSInterTurn {
  def append(reactives: mutable.Set[Reactive[LSStruct]]): Unit
}

class LockSweep(backoff: Backoff, priorTurn: Option[LockSweep]) extends CommonPropagationImpl[LSStruct] with LSInterTurn {

  private type TState = LSStruct


  override private[rescala] def makeStructState[P](initialValue: P, transient: Boolean, initialIncoming: Set[Reactive[TState]], hasState: Boolean): LSStruct#Type[P, TState] = {
    val lock = new TurnLock[LSInterTurn]
    new LSPropagationStruct[P, LSStruct](initialValue, transient, lock, initialIncoming)
  }


  val queue = new util.ArrayDeque[Reactive[TState]]()

  final val key: Key[LSInterTurn] = new Key(this)

  var discovered: mutable.Map[Key[LSInterTurn], mutable.Set[Reactive[TState]]] = mutable.HashMap.empty

  /** lock all reactives reachable from the initial sources
    * retry when acquire returns false */
  override def preparationPhase(initialWrites: Traversable[Reactive[TState]]): Unit = {
    implicit val ticket = makeTicket()
    val stack = new java.util.ArrayDeque[Reactive[TState]](10)
    initialWrites.foreach(stack.offer)

    val locked = new util.ArrayList[Reactive[TState]]

    val priorKey = priorTurn.map(_.key).orNull

    while (!stack.isEmpty) {
      val reactive = stack.pop()
      if ((priorKey ne null) && reactive.state.lock.isOwner(priorKey)) throw new IllegalStateException(s"$this tried to lock reactive $reactive owned by its parent $priorKey")

      if (reactive.state.lock.tryLock(key) eq key) {
        if (reactive.state.willWrite == this) {
          reactive.state.counter += 1
        }
        else {
          locked.add(reactive)
          reactive.state.counter = 1
          reactive.state.willWrite = this
          reactive.state.outgoing(this).foreach {stack.offer}
        }
      }
      else {
        val it = locked.iterator()
        while (it.hasNext) {
          val curr = it.next()
          curr.state.willWrite = null
        }
        locked.clear()
        key.reset()
        stack.clear()
        initialWrites.foreach(stack.offer)
        backoff.backoff()
      }
    }
    initialWrites.foreach{ source =>
      source.state.counter = 0
      source.state.anyInputChanged = this
      source.state.hasChanged = this
      enqueue(source)
    }

  }

  override def propagationPhase(): Unit = {
    while (!queue.isEmpty) {
      evaluate(queue.pop())
    }
  }

  def done(head: Reactive[TState], hasChanged: Boolean): Unit = {
    head.state.hasWritten = this
    if (hasChanged) head.state.hasChanged = this
    head.state.outgoing(this).foreach { r =>
      r.state.counter -= 1
      if (hasChanged) r.state.anyInputChanged = this
      if (r.state.counter <= 0) enqueue(r)
    }
  }

  def enqueue(head: Reactive[TState]): Unit = {
    queue.push(head)
  }

  def evaluate(head: Reactive[TState]): Unit = {
    val ticket = makeTicket()
    if (head.state.anyInputChanged != this) done(head, hasChanged = false)
    else {
      head.reevaluate(ticket) match {
        case Static(value: Option[head.Value]) =>
          val hasChanged = value.isDefined && head.state.base(ticket) != value.get
          if (hasChanged) head.state.set(value.get)(ticket)
          done(head, hasChanged)

        case Dynamic(value, deps) =>
          val diff = DepDiff(deps, head.state.incoming(this))
          applyDiff(head, diff)
          head.state.counter = recount(diff.novel.iterator)
          val hasChanged = value.isDefined && head.state.base(ticket) != value.get

          if (head.state.counter == 0) {
            if (hasChanged) head.state.set(value.get)(ticket)
            done(head, hasChanged)
          }

      }
    }
  }

  def recount(reactives: Iterator[Reactive[TState]]): Int = {
    reactives.count(r => r.state.hasWritten != this && r.state.willWrite == this)
  }

  /**
    * creating a signal causes some unpredictable reactives to be used inside the turn.
    * these will have their locks be acquired dynamically see below for how that works.
    * the newly created reactive on the other hand can not be locked by anything, so we just grab the lock
    * (we do need to grab it, so it can be transferred to some other waiting transaction).
    * it is important, that the locks for the dependencies are acquired BEFORE the constructor for the new reactive.
    * is executed, because the constructor typically accesses the dependencies to create its initial value.
    */
  override def create[T <: Reactive[TState]](dependencies: Set[Reactive[TState]], dynamic: Boolean)(f: => T): T = {
    implicit val ticket = makeTicket()
    dependencies.map(acquireShared)
    val reactive = f
    val owner = reactive.state.lock.tryLock(key)
    assert(owner eq key, s"$this failed to acquire lock on newly created reactive $reactive")
    reactive.state.willWrite = this

    reactive.state.anyInputChanged = this
    if (dynamic) {
      evaluate(reactive)
    }
    else {
      dependencies.foreach(discover(reactive))
      reactive.state.counter = recount(dependencies.iterator)
      val inputsChanged = dependencies.exists(_.state.hasChanged == this)
      if (reactive.state.counter == 0) {
        if (inputsChanged) evaluate(reactive)
        else done(reactive, hasChanged = true)
      }
    }
    reactive
  }


  /** registering a dependency on a node we do not personally own does require some additional care.
    * we let the other turn update the dependency and admit the dependent into the propagation queue
    * so that it gets updated when that turn continues
    * the responsibility for correctly passing the locks is moved to the commit phase */
  override def discover(sink: Reactive[TState])(source: Reactive[TState]): Unit = {
    implicit def turn: Turn[TState] = this

    val owner = acquireShared(source)
    if (owner ne key) {
      if (source.state.willWrite != owner.turn) {
        source.state.discover(sink)
      }
      else if (!source.state.outgoing.contains(sink)) {
        source.state.discover(sink)
        discovered.getOrElseUpdate(owner, mutable.Set.empty).add(sink)
        key.lockKeychain {_.addFallthrough(owner)}
      }
    }
    else {
      source.state.discover(sink)
    }
  }

  /** this is for cases where we register and then unregister the same dependency in a single turn */
  override def drop(sink: Reactive[TState])(source: Reactive[TState]): Unit = {
    implicit def turn: Turn[TState] = this

    val owner = acquireShared(source)
    if (owner ne key) {
      source.state.drop(sink)
      if (source.state.willWrite != owner.turn) {
        key.lockKeychain(_.removeFallthrough(owner))
        if (!sink.state.incoming.exists(_.state.lock.isOwner(owner))) {
          discovered.getOrElseUpdate(owner, mutable.Set.empty).remove(sink)
        }
      }
    }
    else source.state.drop(sink)
  }


  /** this is called after the turn has finished propagating, but before handlers are executed */
  override def releasePhase(): Unit = {
    discovered.foreach { case (owner, reactive) =>
      owner.turn.append(reactive)
    }
    key.releaseAll()
  }


  override def append(appendees: mutable.Set[Reactive[TState]]): Unit = {
    val stack = new java.util.ArrayDeque[Reactive[TState]](10)

    appendees.foreach {stack.push}

    while (!stack.isEmpty) {
      val reactive = stack.pop()

      if (reactive.state.willWrite == this) {
        reactive.state.counter += 1
      }
      else {
        reactive.state.counter = 1
        reactive.state.willWrite = this
        reactive.state.outgoing(this).foreach { r =>
          stack.push(r)
        }
      }
    }

    appendees.foreach { appendee =>
      appendee.state.counter = recount(appendee.state.outgoing(this))
      if (appendee.state.counter == 0) enqueue(appendee)
    }

  }


  /** allow turn to handle dynamic access to reactives */
  override def dynamicDependencyInteraction(dependency: Reactive[TState]): Unit = acquireShared(dependency)

  def acquireShared(reactive: Reactive[TState]): Key[LSInterTurn] = Keychains.acquireShared(reactive.state.lock, key)
}
