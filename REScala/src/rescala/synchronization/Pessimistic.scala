package rescala.synchronization

import rescala.graph.Reactive
import rescala.propagation.{AbstractTurn, LevelQueue}

import scala.annotation.tailrec


class Pessimistic extends AbstractTurn {

  @volatile final var key: Key = new Key((sink, source) => {
    register(sink)(source)
    levelQueue.enqueue(-42)(sink)
  })

  var lazyDependencyUpdates: Set[(Reactive, Reactive)] = Set()

  /** registering a dependency on a node we do not personally own does require some additional care.
    * we move responsibility to the commit phase */
  override def register(sink: Reactive)(source: Reactive): Unit = {
    if (source.lock.hasWriteAccess(key)) super.register(sink)(source)
    else {
      if (!source.dependants.get.contains(sink))
        lazyDependencyUpdates += source -> sink
    }
  }

  /** this is for cases where we register and then unregister the same dependency in a single turn */
  override def unregister(sink: Reactive)(source: Reactive): Unit = {
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
    dependencies.foreach(acquireDynamic)
    val reactive = f
    reactive.lock.lock(key)
    super.create(dependencies)(reactive)
  }

  /** similar to create, except for the ensure level and evaluate calls */
  override def createDynamic[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    dependencies.foreach(acquireDynamic)
    val reactive = f
    reactive.lock.lock(key)
    super.createDynamic(dependencies)(reactive)
  }

  /** a dynamically acquired reactive will never have its value or level changed, only its dependecies.
    * as from a conceptual level it is only read.
    *
    * the check for accessibilty is not strictly necessary as request will handle that as well,
    * which is necessary, because accessibility state may change between the call to isAccessible and request
    * if the current owner of the lock decides to share it with us.
    * but that case seems very unlikely, so the test should provide a solid shortcut */
  def acquireDynamic(reactive: Reactive): Unit = {
    if (!reactive.lock.hasReadAccess(key)) {
      reactive.lock.request(key)
    }
  }

  def acquireWrite(reactive: Reactive): Unit = {
    acquireDynamic(reactive)
    if (!reactive.lock.hasWriteAccess(key)) {
      key.withMaster {
        // traverse our own waiting queue and append us at the end
        key.append(key)
        // release locks so that whatever waits for us can continue
        key.releaseAll()
      }
      // can now safely wait as we will get the lock eventually
      reactive.lock.lock(key)
    }
  }


  /** so i did improve whats noted in the todos below … at least i hope i did.
    * TODO: this probably needs improvement … well it definitely does
    * TODO: the problem is, that lockOrdered tries to lock the reactive,
    * TODO: which does not consider shared locks.
    * TODO: so we might actually run into problems if someone tries to share a lock with us
    * TODO: while we do our initial locking …
    *
    * tried to solve this by acquiring the master lock during initial locking,
    * so that nothing can be shared with us.
    * this still has problems, because evaluating the initial closure of the turn may create new reactives,
    * which causes dynamic locking to happen and screw us here. */
  @tailrec
  private def lockReachable(remaining: List[Reactive]): Unit = remaining match {
    case Nil =>
    case head :: rest =>
      if (!head.lock.hasWriteAccess(key)) {
        acquireWrite(head)
        lockReachable(head.dependants.get.toList ::: rest)
      }
      else lockReachable(rest)
  }

  /** this is called after the initial closure of the turn has been executed,
    * that is the eval queue is populated with the sources */
  override def lockingPhase(): Unit = {
    lockReachable(initialSources)
    initialSources = (initialSources zip initialValues).filter(_._2()).unzip._1
  }

  /** this is called after the turn has finished propagating, but before handlers are executed */
  override def realeasePhase(): Unit = key.releaseAll()
}

