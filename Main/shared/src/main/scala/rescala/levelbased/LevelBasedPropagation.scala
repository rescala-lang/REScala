package rescala.levelbased

import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph.{DepDiff, Reactive}
import rescala.twoversion.CommonPropagationImpl

/**
  * Further implementation of level-based propagation based on the common propagation implementation.
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait LevelBasedPropagation[S <: LevelStruct] extends CommonPropagationImpl[S] with LevelQueue.Evaluator[S] {
  private var _evaluated = List.empty[Reactive[S]]

  val levelQueue = new LevelQueue[S](this)(this)

  def evaluate(head: Reactive[S], ticket: S#Ticket[S]): Unit = {

    def requeue(changed: Boolean, level: Int, redo: Boolean): Unit =
      if (redo) levelQueue.enqueue(level, changed)(head)
      else if (changed) head.state.outgoing(this).foreach(levelQueue.enqueue(level, changed))

    head.reevaluate(ticket) match {
      case Static(value) =>
        if (value.isDefined) {
          head.state.set(value.get, this)
          requeue(changed = true, level = -42, redo = false)
        }
      case Dynamic(value, deps) =>
        val diff = DepDiff(deps, head.state.incoming(this))
        applyDiff(head, diff)
        val newLevel = maximumLevel(diff.novel) + 1
        val redo = head.state.level(this) < newLevel
        val hasChanged = value.isDefined
        if (!redo && hasChanged) {
          head.state.set(value.get, this)
        }
        requeue(hasChanged, newLevel, redo)
    }
    _evaluated ::= head

  }

  private def maximumLevel(dependencies: Set[Reactive[S]]): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.state.level(this)))


  override def create[T <: Reactive[S]](dependencies: Set[Reactive[S]], dynamic: Boolean)(f: => T): T = {
    implicit val ticket: S#Ticket[S] = makeTicket()

    val reactive = f
    val level = ensureLevel(reactive, dependencies)
    if (dynamic) evaluate(reactive, ticket)
    else {
      dependencies.foreach(discover(reactive))
      if (level <= levelQueue.currentLevel() && dependencies.exists(_evaluated.contains)) {
        evaluate(reactive, ticket)
      }
    }
    reactive
  }

  def ensureLevel(dependant: Reactive[S], dependencies: Set[Reactive[S]])(implicit ticket: S#Ticket[S]): Int =
    if (dependencies.isEmpty) 0
    else {
      val newLevel = dependencies.map(_.state.level(this)).max + 1
      dependant.state.updateLevel(newLevel)(this)
    }

  /** allow turn to handle dynamic access to reactives */
  override def dynamicDependencyInteraction(dependency: Reactive[S]): Unit = ()

  override def preparationPhase(initialWrites: Traversable[Reactive[S]]): Unit = {
    implicit val ticket: S#Ticket[S] = makeTicket()

    initialWrites.foreach { reactive =>
      levelQueue.enqueue(reactive.state.level(this))(reactive)
    }
  }

  def propagationPhase(): Unit = levelQueue.evaluateQueue()


}
