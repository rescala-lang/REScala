package rescala.core

import rescala.core.Node.InDep

import scala.language.existentials // stupid Disconnectable implementation requires stupid self-type stupid!

/**
  * Indicator for the result of a re-evaluation of a reactive value.
  */
sealed trait ReevaluationResult[S <: Struct]{
  val node: Reactive[S]
  val valueChanged: Boolean
  val indepsChanged: Boolean
  val indepsAfter: Set[InDep[S]]

  def commitDependencyDiff(): Unit

  // superior design option, but sadly incompatible with fullMV, need to figure out a solution still..
//  def commitValueChange(): Set[Reactive[S]]
  def commitTuple: (WriteableReactive[Pulse.Change[P], S], Pulse.Change[P]) forSome { type P }
}

final case class ReevaluationResultImpl[P, S <: Struct](turn: ReevaluationStateAccess[S], node: WriteableReactive[Pulse.Change[P], S] with Reactive[S], value: Pulse[P], indepsChanged: Boolean, indepsAfter: Set[InDep[S]], indepsAdded: Set[InDep[S]], indepsRemoved: Set[InDep[S]]) extends ReevaluationResult[S] {
  override val valueChanged: Boolean = value.isChange
  override def commitDependencyDiff(): Unit = {
    if(indepsChanged) {
      indepsRemoved.foreach(turn.drop(_, node))
      indepsAdded.foreach(turn.discover(_, node))
      turn.writeIndeps(node, indepsAfter)
    }
  }

  // yes there's an instanceOf, but that is no different than Option.get because it is/should be called only if valueChanged.
  // But, as no equivalent method exists here and I'd get shouted at if I added it, it's an instanceOf :P
//  override def commitValueChange(): Set[Reactive[S]] = {
//    if(valueChanged) {
//      turn.reevOutChanged(node, value.asInstanceOf[Pulse.Change[P]])
//    } else {
//      turn.reevOutUnchanged(node)
//    }
//  }
  override def commitTuple: (WriteableReactive[Pulse.Change[P], S], Pulse.Change[P]) = (node, value.asInstanceOf[Pulse.Change[P]])
}

object ReevaluationResult {

  /**
    * Result of the static re-evaluation of a reactive value.
    */
  def Static[P, S <: Struct](turn: ReevaluationStateAccess[S], node: WriteableReactive[Pulse[P], S] with Reactive[S], value: Pulse[P], unchangedIndeps: Set[InDep[S]]): ReevaluationResultImpl[P, S] =  ReevaluationResultImpl(turn, node, value, indepsChanged = false, unchangedIndeps, Set.empty, Set.empty)

  /**
    * Result of the dynamic re-evaluation of a reactive value.
    * When using a dynamic dependency model, the dependencies of a value may change at runtime if it is re-evaluated
    */
  def Dynamic[P, S <: Struct](turn: ReevaluationStateAccess[S], node: WriteableReactive[Pulse[P], S] with Reactive[S], value: Pulse[P], indepsAfter: Set[InDep[S]], indepsAdded: Set[InDep[S]], indepsRemoved: Set[InDep[S]]): ReevaluationResultImpl[P, S] = ReevaluationResultImpl(turn, node, value, indepsAdded.nonEmpty || indepsRemoved.nonEmpty, indepsAfter, indepsAdded, indepsRemoved)
}


trait Disconnectable[S <: Struct] extends Reactive[S] {
  self: WriteableReactive[Pulse[Nothing], S] =>
  @volatile private var disconnected = false

  final def disconnect()(implicit engine: Engine[S]): Unit = {
    engine.transaction(this) { turn =>
      disconnected = true
    }
  }


  abstract final override protected[rescala] def reevaluate(turn: Turn[S], before: Value, indeps: Set[InDep[S]]): ReevaluationResult[S] = {
    if (disconnected) {
      ReevaluationResult.Dynamic[Nothing, S](turn, this, Pulse.NoChange, Set.empty, Set.empty, indeps)
    }
    else {
      super.reevaluate(turn, before, indeps)
    }
  }

}
