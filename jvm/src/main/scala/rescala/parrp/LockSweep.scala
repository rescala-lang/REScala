package rescala.parrp

import java.util
import java.util.Collections

import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph._
import rescala.locking._
import rescala.propagation.{CommonPropagationImpl, Turn}

object LSStruct extends PropagationStruct {
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

  var writeSet: LockSweep = null
  var color: LockSweep = null

}

trait LSInterTurn {
  def append(reactives: util.ArrayList[Reactive[LSStruct.type]]): Unit
}

class LockSweep(backoff: Backoff) extends CommonPropagationImpl[LSStruct.type] with LSInterTurn {

  private type TState = LSStruct.type

  override def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): LSStruct.SporeP[P, R] = {
    val lock = new TurnLock[LSInterTurn]
    new LSSporeP[P, R](initialValue, transient, lock, initialIncoming)
  }

  val sorted = new util.ArrayList[Reactive[TState]]

  final val key: Key[LSInterTurn] = new Key(this)

  var currentIndex = 0

  var discovered: List[(Key[LSInterTurn], Reactive[TState], Reactive[TState])] = Nil

  /** lock all reactives reachable from the initial sources
    * retry when acquire returns false */
  override def preparationPhase(initialWrites: List[Reactive[TState]]): Unit = {
    val stack = new java.util.ArrayDeque[Reactive[TState]](10)
    initialWrites.foreach(stack.push)

    while (!stack.isEmpty) {
      val reactive = stack.pop()

      if (reactive.bud.lock.tryLock(key) eq key) {
        if (reactive.bud.writeSet == this) {
          sorted.add(reactive)
        }
        else {
          reactive.bud.writeSet = this
          // we add the reactive again, so we can enter it into the `sorted` when the childern are processed
          stack.push(reactive)
          reactive.bud.outgoing(this).foreach { r =>
            if (!r.bud.lock.isOwner(key)) stack.push(r)
          }
        }
      }
      else {
        val it = sorted.iterator()
        while (it.hasNext) it.next().bud.writeSet = null
        sorted.clear()
        key.reset()
        backoff.backoff()
        val sit = stack.iterator()
        while(sit.hasNext) {
          val curr = sit.next()
          if (curr.bud.writeSet == this) curr.bud.writeSet = null
        }
        stack.clear()
        initialWrites.foreach(stack.push)
      }
    }
    Collections.reverse(sorted)
  }

  override def propagationPhase(): Unit = {
    while (currentIndex < sorted.size()) {
      evaluate(sorted.get(currentIndex))
      currentIndex += 1
    }
  }

  def find(start: Reactive[TState]): util.ArrayList[Reactive[TState]] = {
    val stack = new java.util.ArrayDeque[Reactive[TState]](sorted.size())
    stack.push(start)
    val result = new util.ArrayList[Reactive[TState]]()

    while (!stack.isEmpty) {
      val reactive = stack.pop()

      if (reactive.bud.color == this) {
        result.add(reactive)
      }
      else {
        reactive.bud.color = this
        // we add the reactive again, so we can enter it into the `sorted` when the childern are processed
        stack.push(reactive)
        reactive.bud.outgoing(this).foreach { r =>
          if (r.bud.color != this) stack.push(r)
        }
      }
    }
    Collections.reverse(result)
    val it = result.iterator()
    while (it.hasNext) it.next().bud.color = null
    result
  }

  def insert(insertees: util.List[Reactive[TState]], atFirst: Set[Reactive[TState]]): Unit = {
    val queueIT = sorted.listIterator(sorted.size())
    while (queueIT.previousIndex() > currentIndex) {
      val current = queueIT.previous()
      if (atFirst.contains(current)) {
        queueIT.next()
        val inserteeIT = insertees.iterator()
        while (inserteeIT.hasNext) queueIT.add(inserteeIT.next())
        val secondInserteeIT = insertees.iterator()
        val secondQueueIT = sorted.listIterator(currentIndex)
        while (secondInserteeIT.hasNext && secondQueueIT.hasNext) {
          val toRemove = secondInserteeIT.next
          while (secondQueueIT.next() != toRemove) {}
          secondQueueIT.remove()
        }
        currentIndex -= 1
        return
      }
    }
  }

  def evaluate(head: Reactive[TState]): Unit = {

    head.reevaluate()(this) match {
      case Static(hasChanged) =>
      case Dynamic(hasChanged, diff) =>
        diff.removed foreach drop(head)
        diff.added foreach discover(head)
        val insertees = find(head)
        insert(insertees, diff.novel)
    }
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

    if (dynamic) {
      sorted.add(reactive)
    }
    else {
      dependencies.foreach(discover(reactive))
      insert(util.Arrays.asList(reactive), dependencies)
    }
    reactive
  }




  /** registering a dependency on a node we do not personally own does require some additional care.
    * we let the other turn update the dependency and admit the dependent into the propagation queue
    * so that it gets updated when that turn continues
    * the responsibility for correctly passing the locks is moved to the commit phase */
  def discover(sink: Reactive[TState])(source: Reactive[TState]): Unit = {

    val owner = acquireShared(source)
    if (owner ne key) {
      if (source.bud.writeSet == null) {
        source.bud.discover(sink)(this)
      }
      else if (!source.bud.outgoing(this).contains(sink)) {
        source.bud.discover(sink)(this)
        discovered ::= ((owner, sink, source))
        key.lockKeychain { _.addFallthrough(owner) }
      }
    }
    else {
      source.bud.discover(sink)(this)
    }
  }

  /** this is for cases where we register and then unregister the same dependency in a single turn */
  def drop(sink: Reactive[TState])(source: Reactive[TState]): Unit = {


    val owner = acquireShared(source)
    if (owner ne key) {
      source.bud.drop(sink)(this)
      if (source.bud.writeSet == null) {
        key.lockKeychain(_.removeFallthrough(owner))
        if (!sink.bud.incoming(this).exists(_.bud.lock.isOwner(owner))) {
          discovered = discovered.filter(_ != ((owner, sink, source)))
        }
      }
    }
    else source.bud.drop(sink)(this)
  }


  /** this is called after the turn has finished propagating, but before handlers are executed */
  override def releasePhase(): Unit = {
    discovered.reverse.foreach { case (other, sink, source) =>
      other.turn.append(find(sink))
    }
    key.releaseAll()
  }


  override def append(reactives: util.ArrayList[Reactive[TState]]): Unit = {
    sorted.addAll(reactives)
  }



  /** allow turn to handle dynamic access to reactives */
  override def dependencyInteraction(dependency: Reactive[TState]): Unit = acquireShared(dependency)

  def acquireShared(reactive: Reactive[TState]): Key[LSInterTurn] = Keychains.acquireShared(reactive.bud.lock, key)
}
