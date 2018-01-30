package rescala.parrp

import java.util

import rescala.core.Initializer.Param
import rescala.core._
import rescala.locking._
import rescala.twoversion.{PropagationStructImpl, TwoVersionPropagationImpl, TwoVersionStruct}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait LSStruct extends TwoVersionStruct {
  override type State[P, S <: Struct] = LSPropagationStruct[P, S]
}


class LSPropagationStruct[P, S <: Struct](current: P, transient: Boolean, val lock: TurnLock[LSInterTurn])
  extends PropagationStructImpl[P, S](current, transient) {

  var willWrite: LockSweep = null
  var hasWritten: LockSweep = null
  var counter: Int = 0
  def isGlitchFreeReady: Boolean = counter == 0
  var anyInputChanged: LockSweep = null
  var isPropagating: LockSweep = null
}

trait LSInterTurn {
  def append(reactives: mutable.Set[Reactive[LSStruct]]): Unit
}

class LockSweep(backoff: Backoff, priorTurn: Option[LockSweep]) extends TwoVersionPropagationImpl[LSStruct] with LSInterTurn {

  private type TState = LSStruct

  val queue = new util.ArrayDeque[Reactive[TState]]()

  final val key: Key[LSInterTurn] = new Key(this)

  var discovered: mutable.Map[Key[LSInterTurn], mutable.Set[Reactive[TState]]] = mutable.HashMap.empty



  override protected def makeDerivedStructState[P, N](valuePersistency: Param[P]): LSStruct#State[P, TState] = {
    val lock = new TurnLock[LSInterTurn]
    val owner = lock.tryLock(key)
    assert(owner eq key, s"$this failed to acquire lock on newly created reactive")
    new LSPropagationStruct(valuePersistency.initialValue, valuePersistency.isTransient, lock)
  }

  /**
    * creating a signal causes some unpredictable reactives to be used inside the turn.
    * these will have their locks be acquired dynamically see below for how that works.
    * the newly created reactive on the other hand can not be locked by anything, so we just grab the lock
    * (we do need to grab it, so it can be transferred to some other waiting transaction).
    * it is important, that the locks for the dependencies are acquired BEFORE the constructor for the new reactive.
    * is executed, because the constructor typically accesses the dependencies to create its initial value.
    */
  protected def ignite(reactive: Reactive[TState], incoming: Set[ReSource[TState]], ignitionRequiresReevaluation: Boolean): Unit = {
    incoming.foreach { dep =>
      acquireShared(dep)
      discover(dep, reactive)
    }
    writeIndeps(reactive, incoming)
    if(ignitionRequiresReevaluation || incoming.exists(_.state.isPropagating == this)) {
      reactive.state.anyInputChanged = this
      reactive.state.willWrite = this
      if (reactive.state.isGlitchFreeReady) {
        evaluate(reactive)
      }
    }
  }


  override def writeState(pulsing: ReSource[TState])(value: pulsing.Value): Unit = {
    assert({
        val wlo: Option[Key[LSInterTurn]] = Option(pulsing.state.lock.getOwner)
        wlo.fold(true)(_ eq key)},
          s"buffer ${pulsing.state}, controlled by ${pulsing.state.lock} with owner ${pulsing.state.lock.getOwner}" +
            s" was written by $this who locks with $key, by now the owner is ${pulsing.state.lock.getOwner}")
    super.writeState(pulsing)(value)
  }


  /** lock all reactives reachable from the initial sources
    * retry when acquire returns false */
  override def preparationPhase(initialWrites: Traversable[ReSource[TState]]): Unit = {
    val stack = new java.util.ArrayDeque[ReSource[TState]](10)
    initialWrites.foreach(stack.offer)

    val locked = ArrayBuffer[ReSource[TState]]()

    val priorKey = priorTurn.map(_.key).orNull

    while (!stack.isEmpty) {
      val reactive = stack.pop()
      if ((priorKey ne null) && reactive.state.lock.isOwner(priorKey)) throw new IllegalStateException(s"$this tried to lock reactive $reactive owned by its parent $priorKey")

      if (reactive.state.lock.tryLock(key) eq key) {
        if (reactive.state.willWrite == this) {
          reactive.state.counter += 1
        }
        else {
          locked += reactive
          reactive.state.counter = 1
          reactive.state.willWrite = this
          reactive.state.outgoing().foreach {stack.offer}
        }
      }
      else {
        locked.foreach{ _.state.willWrite = null }
        locked.clear()
        key.reset()
        stack.clear()
        initialWrites.foreach(stack.offer)
        backoff.backoff()
      }
    }
  }


  final override def initialize(ic: InitialChange[TState]): Unit = {
    ic.source.state.counter = 0
    ic.source.state.anyInputChanged = this
    ic.source.state.hasWritten = this
    writeState(ic.source)(ic.value)
    done(ic.source, propagate = true)
  }

  override def propagationPhase(): Unit = {
    while (!queue.isEmpty) {
      evaluate(queue.pop())
    }
  }

  def done(head: ReSource[TState], propagate: Boolean): Unit = {
    if(propagate) head.state.isPropagating = this

    head.state.outgoing().foreach { r =>
      r.state.counter -= 1
      if (propagate) r.state.anyInputChanged = this
      if (r.state.counter <= 0) enqueue(r)
    }
  }

  def enqueue(head: Reactive[TState]): Unit = {
    queue.push(head)
  }

  def evaluate(head: Reactive[TState]): Unit = {
    if (head.state.anyInputChanged != this) done(head, propagate = false)
    else {
      val dt = makeDynamicReevaluationTicket[head.Value](head.state.base(token))
      val reevRes = head.reevaluate(dt)
      val collectedDependencies = reevRes.getDependencies()

      collectedDependencies.foreach(commitDependencyDiff(head, head.state.incoming()))
      if (head.state.isGlitchFreeReady) {
        reevRes.forValue(writeState(head))
        reevRes.forEffect(observe)
        head.state.hasWritten = this

        done(head, reevRes.propagate)
      }

    }

  }

  override def writeIndeps(node: Reactive[TState], indepsAfter: Set[ReSource[TState]]): Unit = {
    super.writeIndeps(node, indepsAfter)
    recount(node)
  }

  def recount(reactive: Reactive[TState]): Unit = {
    reactive.state.counter = reactive.state.incoming().count(r => r.state.hasWritten != this && r.state.willWrite == this)
  }


  /** registering a dependency on a node we do not personally own does require some additional care.
    * we let the other turn update the dependency and admit the dependent into the propagation queue
    * so that it gets updated when that turn continues
    * the responsibility for correctly passing the locks is moved to the commit phase */
  override def discover(source: ReSource[TState], sink: Reactive[TState]): Unit =  {
    val owner = acquireShared(source)
    if (owner ne key) {
      if (source.state.willWrite != owner.turn) {
        source.state.discover(sink)
      }
      else if (!source.state.outgoing().exists(_ == sink)) {
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
  override private[rescala] def drop(source: ReSource[TState], sink: Reactive[TState]) = {
    val owner = acquireShared(source)
    if (owner ne key) {
      source.state.drop(sink)
      if (source.state.willWrite != owner.turn) {
        key.lockKeychain(_.removeFallthrough(owner))
        if (!sink.state.incoming().exists(_.state.lock.isOwner(owner))) {
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
        reactive.state.outgoing().foreach { r =>
          stack.push(r)
        }
      }
    }

    appendees.foreach { appendee =>
      recount(appendee)
      if (appendee.state.counter == 0) enqueue(appendee)
    }

  }


  /** allow turn to handle dynamic access to reactives */
  override def dynamicDependencyInteraction(dependency: ReSource[TState]): Unit = acquireShared(dependency)

  def acquireShared(reactive: ReSource[TState]): Key[LSInterTurn] = Keychains.acquireShared(reactive.state.lock, key)
}
