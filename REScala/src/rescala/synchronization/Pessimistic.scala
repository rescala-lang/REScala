package rescala.synchronization

import rescala.graph.Reactive
import rescala.propagation.{TurnImpl, LevelQueue}

import scala.annotation.tailrec


class Pessimistic extends TurnImpl {

  final val key: Key = new Key((sink, source) => {
    register(sink)(source)
    levelQueue.enqueue(-42)(sink)
  })

  var lazyDependencyUpdates: Set[(Reactive, Reactive)] = Set()

  var evaluated: List[Reactive] = Nil

  override def evaluate(r: Reactive): Unit = {
    assert(r.lock.hasWriteAccess(key))
    evaluated ::= r
    super.evaluate(r)
  }

  /** registering a dependency on a node we do not personally own does require some additional care.
    * we move responsibility to the commit phase */
  override def register(sink: Reactive)(source: Reactive): Unit = {
    source.lock.acquireDynamic(key)
    if (source.lock.hasWriteAccess(key)) super.register(sink)(source)
    else {
      if (!source.dependants.get.contains(sink))
        lazyDependencyUpdates += source -> sink
    }
  }

  /** this is for cases where we register and then unregister the same dependency in a single turn */
  override def unregister(sink: Reactive)(source: Reactive): Unit = {
    source.lock.acquireDynamic(key)
    super.unregister(sink)(source)
    lazyDependencyUpdates -= (source -> sink)
  }


  /** after the normal commit, we register the lazy dependency updates and put the new downstream into the others queue
    * (it is a new dependency, that has conceptually been there before the other turn, so it needs to be evaluated) */
  override def commitPhase(): Unit = {
    super.commitPhase()
    lazyDependencyUpdates.foreach { case (source, sink) =>
      val other = source.lock.getOwner
      other.handleDependencyChange(sink, source)
    }
  }

  /** creating a signal causes some unpredictable reactives to be used inside the turn.
    * these will have their locks be acquired dynamically see below for how that works.
    * the newly created reactive on the other hand can not be locked by anything, so we just grab the lock
    * (we do need to grab it, so it can be transferred to some other waiting transaction).
    * it is important, that the locks for the dependencies are acquired BEFORE the constructor for the new reactive.
    * is executed, because the constructor typically accesses the dependencies to create its initial value. */
  override def create[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    dependencies.foreach(_.lock.acquireDynamic(key))
    val reactive = f
    reactive.lock.lock(key)
    super.create(dependencies)(reactive)
  }

  /** similar to create, except for the ensure level and evaluate calls */
  override def createDynamic[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    dependencies.foreach(_.lock.acquireDynamic(key))
    val reactive = f
    reactive.lock.lock(key)
    super.createDynamic(dependencies)(reactive)
  }

  /** lock all reactives reachable from the initial sources */
  def lockReachable(): Unit = {
    lazy val lq = new LevelQueue(evaluate)

    def evaluate(reactive: Reactive): Unit = {
      reactive.lock.acquireWrite(key)
      reactive.dependants.get.foreach(lq.enqueue(-42))
    }
    initialReactives.foreach(lq.enqueue(-42))
    lq.evaluateQueue()
  }

  /** this is called after the initial closure of the turn has been executed,
    * that is the eval queue is populated with the sources */
  override def lockPhase(): Unit = lockReachable()

  /** this is called after the turn has finished propagating, but before handlers are executed */
  override def realeasePhase(): Unit = key.releaseAll()
}

