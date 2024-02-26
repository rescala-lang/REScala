package reactives.parrp

import reactives.core.Scheduler
import reactives.locking.*
import reactives.operator.Interface
import reactives.scheduler.Levelbased

object ParRPDefault extends ParRP {
  val scheduler = parrpWithBackoff(() => new Backoff())
  type BundleState[V] = State[V]
}
trait ParRP extends Levelbased {

  override type State[V] = ParRPState[V]

  def parrpWithBackoff(backOff: () => Backoff): Scheduler[State] =
    new TwoVersionScheduler[ParRPTransaction] {
      override protected def makeTransaction(priorTx: Option[ParRPTransaction]): ParRPTransaction =
        new ParRPTransaction(backOff(), priorTx)
      override def schedulerName: String = "ParRP"
    }

  class ParRPState[V](initialValue: V, val lock: ReLock[ParRPInterTurn]) extends LevelState[V](initialValue)

  trait ParRPInterTurn {

    def discover(sink: ReSource, source: Derived): Unit
    def drop(sink: ReSource, source: Derived): Unit

    def forget(reactive: Derived): Unit
    def admit(reactive: Derived): Unit

  }

  class ParRPTransaction(backoff: Backoff, priorTurn: Option[ParRPTransaction])
      extends LevelBasedTransaction
      with ParRPInterTurn {

    override def writeState(pulsing: ReSource)(value: pulsing.Value): Unit = {
      assert(
        {
          val wlo: Option[Key[ParRPInterTurn]] = Option(pulsing.state.lock.getOwner)
          wlo.fold(true)(_ eq key)
        },
        s"buffer ${pulsing.state}, controlled by ${pulsing.state.lock} with owner ${pulsing.state.lock.getOwner}" +
        s" was written by $this who locks with $key, by now the owner is ${pulsing.state.lock.getOwner}"
      )
      super.writeState(pulsing)(value)
    }

    override def toString: String = s"ParRP(${key.id})"

    final val key: Key[ParRPInterTurn] = new Key(this)

    override protected def makeDerivedStructState[V](initialValue: V): ParRPState[V] = {
      val lock  = new ReLock[ParRPInterTurn]
      val owner = lock.tryLock(key)
      assert(owner eq key, s"$this failed to acquire lock on newly created reactive")
      new ParRPState(initialValue, lock)
    }

    /** this is called after the turn has finished propagating, but before handlers are executed */
    override def releasePhase(): Unit = key.releaseAll()

    /** allow turn to handle dynamic access to reactives */
    override def beforeDynamicDependencyInteraction(dependency: ReSource): Unit = { acquireShared(dependency); () }

    /** lock all reactives reachable from the initial sources
      * retry when acquire returns false
      */
    override def preparationPhase(initialWrites: Set[ReSource]): Unit = {
      val toVisit                 = new java.util.ArrayDeque[ReSource](10)
      val offer: ReSource => Unit = toVisit.addLast
      initialWrites.foreach(offer)
      val priorKey = priorTurn.fold[Key[ParRPInterTurn]](null)(_.key)

      while (!toVisit.isEmpty) {
        val reactive = toVisit.pop()
        val owner    = reactive.state.lock.getOwner
        if ((priorKey ne null) && (owner eq priorKey))
          throw new IllegalStateException(s"$this tried to lock reactive $reactive owned by its parent $priorKey")
        if (owner ne key) {
          if (reactive.state.lock.tryLock(key) eq key)
            reactive.state.outgoing.foreach { offer }
          else {
            key.reset()
            backoff.backoff()
            toVisit.clear()
            initialWrites.foreach(offer)
          }
        }
      }
    }

    override def forget(reactive: Derived): Unit = levelQueue.remove(reactive)
    override def admit(reactive: Derived): Unit  = levelQueue.enqueue(reactive.state.level())(reactive)

    /** registering a dependency on a node we do not personally own does require some additional care.
      * we let the other turn update the dependency and admit the dependent into the propagation queue
      * so that it gets updated when that turn continues
      * the responsibility for correctly passing the locks is moved to the commit phase
      */
    override def discover(source: ReSource, sink: Derived): Unit = {
      val owner = acquireShared(source)
      if (owner ne key) {
        owner.turn.discover(source, sink)
        if (source.state.lock.isWriteLock) {
          owner.turn.admit(sink)
          key.lockKeychain { _.addFallthrough(owner) }
        }
      } else {
        super.discover(source, sink)
      }
    }

    /** this is for cases where we register and then unregister the same dependency in a single turn */
    override def drop(source: ReSource, sink: Derived): Unit = {
      val owner = acquireShared(source)
      if (owner ne key) {
        owner.turn.drop(source, sink)
        if (source.state.lock.isWriteLock) {
          key.lockKeychain(_.removeFallthrough(owner))
          if (
            !sink.state.incoming.exists { inc =>
              val lock = inc.state.lock
              lock.isOwner(owner) && lock.isWriteLock
            }
          ) owner.turn.forget(sink)
        }
      } else super.drop(source, sink)
    }

    def acquireShared(reactive: ReSource): Key[ParRPInterTurn] = Keychains.acquireShared(reactive.state.lock, key)
  }
}
