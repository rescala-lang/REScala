package rescala.stm

import rescala.core.{Reactive, Struct, ValuePersistency}
import rescala.levelbased.{LevelBasedPropagation, LevelStruct}
import rescala.twoversion.Token

import scala.concurrent.stm.{InTxn, atomic}

class STMTurn extends LevelBasedPropagation[STMTurn] with LevelStruct {
  override type State[P, S <: Struct] = STMStructType[P, S]

  /** used to create state containers of each reactive */
  override protected def makeStructState[P](valuePersistency: ValuePersistency[P]): State[P, STMTurn] = {
    new STMStructType(valuePersistency.initialValue, valuePersistency.isTransient)
  }
  override def releasePhase(): Unit = ()
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
  override val token = Token(inTxn)
  override def dynamicDependencyInteraction(dependency: Reactive[STMTurn]): Unit = ()
  override def preparationPhase(initialWrites: Traversable[Reactive[STMTurn]]): Unit = ()
}


