package rescala.parrp

import java.util

import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph._
import rescala.locking._
import rescala.propagation.{CommonPropagationImpl, Turn}

import scala.collection.mutable

object LSStruct extends PulsingGraphStruct {
  override type SporeP[P, R] = LSSporeP[P, R]
}


class LSSporeP[P, R](current: Pulse[P], transient: Boolean, val lock: TurnLock[LSInterTurn], initialIncoming: Set[R])
  extends PropagationSporeImpl[P, R](current, transient, initialIncoming) {

  override def set(value: Pulse[P])(implicit turn: Turn[_]): Unit = {
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
  def append(reactives: mutable.Set[Reactive[LSStruct.type]]): Unit
}

class LockSweep(backoff: Backoff, priorTurn: Option[LockSweep]) extends CommonPropagationImpl[LSStruct.type] with LSInterTurn {

  private type TState = LSStruct.type

  override def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): LSStruct.SporeP[P, R] = {
    val lock = new TurnLock[LSInterTurn]
    new LSSporeP[P, R](initialValue, transient, lock, initialIncoming)
  }


  val queue = new util.ArrayDeque[Reactive[TState]]()

  final val key: Key[LSInterTurn] = new Key(this)

  var discovered: mutable.Map[Key[LSInterTurn], mutable.Set[Reactive[TState]]] = mutable.HashMap.empty

  /** lock all reactives reachable from the initial sources
    * retry when acquire returns false */
  override def preparationPhase(initialWrites: Traversable[Reactive[TState]]): Unit = {
    val stack = new java.util.ArrayDeque[Reactive[TState]](10)
    initialWrites.foreach(stack.offer)

    val locked = new util.ArrayList[Reactive[TState]]

    val priorKey = priorTurn.map(_.key).orNull

    while (!stack.isEmpty) {
      val reactive = stack.pop()
      if ((priorKey ne null) && reactive.bud.lock.isOwner(priorKey)) throw new IllegalStateException(s"$this tried to lock reactive $reactive owned by its parent $priorKey")

      if (reactive.bud.lock.tryLock(key) eq key) {
        if (reactive.bud.willWrite == this) {
          reactive.bud.counter += 1
        }
        else {
          locked.add(reactive)
          reactive.bud.counter = 1
          reactive.bud.willWrite = this
          reactive.bud.outgoing(this).foreach {stack.offer}
        }
      }
      else {
        val it = locked.iterator()
        while (it.hasNext) {
          val curr = it.next()
          curr.bud.willWrite = null
        }
        locked.clear()
        key.reset()
        stack.clear()
        initialWrites.foreach(stack.offer)
        backoff.backoff()
      }
    }
    initialWrites.foreach{ source =>
      source.bud.counter = 0
      source.bud.anyInputChanged = this
      source.bud.hasChanged = this
      enqueue(source)
    }

  }

  override def propagationPhase(): Unit = {
    while (!queue.isEmpty) {
      evaluate(queue.pop())
    }
  }

  def done(head: Reactive[TState], hasChanged: Boolean): Unit = {
    head.bud.hasWritten = this
    if (hasChanged) head.bud.hasChanged = this
    head.bud.outgoing(this).foreach { r =>
      r.bud.counter -= 1
      if (hasChanged) r.bud.anyInputChanged = this
      if (r.bud.counter <= 0) enqueue(r)
    }
  }

  def enqueue(head: Reactive[TState]): Unit = {
    queue.push(head)
  }

  def evaluate(head: Reactive[TState]): Unit = {
    if (head.bud.anyInputChanged != this) done(head, hasChanged = false)
    else {
      head.reevaluate()(this) match {
        case Static(hasChanged) => done(head, hasChanged)

        case Dynamic(hasChanged, diff) =>
          applyDiff(head, diff)
          head.bud.counter = recount(diff.novel.iterator)

          if (head.bud.counter == 0) done(head, hasChanged)

      }
    }
  }

  def recount(reactives: Iterator[Reactive[TState]]): Int = {
    reactives.count(r => r.bud.hasWritten != this && r.bud.willWrite == this)
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
    dependencies.map(acquireShared)
    val reactive = f
    val owner = reactive.bud.lock.tryLock(key)
    assert(owner eq key, s"$this failed to acquire lock on newly created reactive $reactive")
    reactive.bud.willWrite = this

    reactive.bud.anyInputChanged = this
    if (dynamic) {
      evaluate(reactive)
    }
    else {
      dependencies.foreach(discover(reactive))
      reactive.bud.counter = recount(dependencies.iterator)
      val inputsChanged = dependencies.exists(_.bud.hasChanged == this)
      if (reactive.bud.counter == 0) {
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

    val owner = acquireShared(source)
    if (owner ne key) {
      if (source.bud.willWrite != owner.turn) {
        source.bud.discover(sink)(this)
      }
      else if (!source.bud.outgoing(this).contains(sink)) {
        source.bud.discover(sink)(this)
        discovered.getOrElseUpdate(owner, mutable.Set.empty).add(sink)
        key.lockKeychain {_.addFallthrough(owner)}
      }
    }
    else {
      source.bud.discover(sink)(this)
    }
  }

  /** this is for cases where we register and then unregister the same dependency in a single turn */
  override def drop(sink: Reactive[TState])(source: Reactive[TState]): Unit = {

    val owner = acquireShared(source)
    if (owner ne key) {
      source.bud.drop(sink)(this)
      if (source.bud.willWrite != owner.turn) {
        key.lockKeychain(_.removeFallthrough(owner))
        if (!sink.bud.incoming(this).exists(_.bud.lock.isOwner(owner))) {
          discovered.getOrElseUpdate(owner, mutable.Set.empty).remove(sink)
        }
      }
    }
    else source.bud.drop(sink)(this)
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

      if (reactive.bud.willWrite == this) {
        reactive.bud.counter += 1
      }
      else {
        reactive.bud.counter = 1
        reactive.bud.willWrite = this
        reactive.bud.outgoing(this).foreach { r =>
          stack.push(r)
        }
      }
    }

    appendees.foreach { appendee =>
      appendee.bud.counter = recount(appendee.bud.outgoing(this))
      if (appendee.bud.counter == 0) enqueue(appendee)
    }

  }


  /** allow turn to handle dynamic access to reactives */
  override def dynamicDependencyInteraction(dependency: Reactive[TState]): Unit = acquireShared(dependency)

  def acquireShared(reactive: Reactive[TState]): Key[LSInterTurn] = Keychains.acquireShared(reactive.bud.lock, key)
}
