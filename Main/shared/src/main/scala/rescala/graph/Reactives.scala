package rescala.graph

import rescala.engine.TurnSource
import rescala.propagation.Turn
import rescala.reactives.RExceptions.EmptySignalControlThrowable

import scala.annotation.compileTimeOnly

/**
  * A reactive value is something that can be reevaluated
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Reactive[S <: Struct] {
  final override val hashCode: Int = Globals.nextID().hashCode()

  /**
    * Spore that is used to internally manage the reactive evaluation of this value
    *
    * @return Spore for this value
    */
  protected[rescala] def state: S#StructType[_, Reactive[S]]

  /**
    * Reevaluates this value when it is internally scheduled for reevaluation
    *
    * @param turn Turn that handles the reevaluation
    * @return Result of the reevaluation
    */
  protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S]

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString: String = name
}


/** helper class to initialise engine and select lock */
abstract class Base[+P, S <: Struct](struct: S#StructType[P, Reactive[S]]) extends Pulsing[P, S] {
  final override protected[rescala] def state: S#StructType[_, Reactive[S]] = struct

  final protected[this] override def set(value: Pulse[P])(implicit turn: Turn[S]): Unit = if (value.isChange) struct.set(value) else if (hasChanged) struct.set(stable)
  final protected[rescala] override def stable(implicit turn: Turn[S]): Pulse[P] = struct.base
  final protected[rescala] override def pulse(implicit turn: Turn[S]): Pulse[P] = struct.get
}

/**
  * A pulsing value is a reactive value that stores a pulse with it's old and new value
  *
  * @tparam P Value type stored by the pulse of the reactive value
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Pulsing[+P, S <: Struct] extends Reactive[S] {
  protected[this] def set(value: Pulse[P])(implicit turn: Turn[S]): Unit
  final private[rescala] def hasChanged(implicit turn: Turn[S]): Boolean = stable != pulse
  protected[rescala] def stable(implicit turn: Turn[S]): Pulse[P]
  protected[rescala] def pulse(implicit turn: Turn[S]): Pulse[P]
}


/**
  * A reactive value that has a current state that can be read
  *
  * @tparam A Value type stored by the reactive value
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait Stateful[+A, S <: Struct] extends Pulsing[A, S] {
  // only used inside macro and will be replaced there
  @compileTimeOnly("Signal.apply can only be used inside of Signal expressions")
  final def apply(): A = throw new IllegalAccessException(s"$this.apply called outside of macro")


  final def now(implicit ticket: TurnSource[S]): A = ticket { turn =>
    turn.dynamicDependencyInteraction(this)
    try { regRead(turn) }
    catch { case EmptySignalControlThrowable => throw new NoSuchElementException(s"Signal $this is empty") }
  }

  // access value as static dependency
  private[rescala] final def regRead(implicit turn: Turn[S]): A = pulse(turn).get
}
