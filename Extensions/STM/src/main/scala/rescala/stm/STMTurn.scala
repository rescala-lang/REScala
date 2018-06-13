package rescala.stm

import rescala.core.Initializer.InitValues
import rescala.core.{ReSource, Struct}
import rescala.levelbased.{LevelBasedPropagation, LevelStruct}
import rescala.twoversion.Token

import scala.concurrent.stm.{InTxn, atomic}

class STMTurn extends LevelBasedPropagation[STMTurn] with LevelStruct {
  override type State[V, S <: Struct] = STMStructType[V, S]

  /** used to create state containers of each reactive */
  override protected def makeDerivedStructState[V](valuePersistency: InitValues[V]): State[V, STMTurn] = {
    new STMStructType(valuePersistency)
  }
  override def releasePhase(): Unit = ()
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
  override val token = Token(inTxn)
  override def dynamicDependencyInteraction(dependency: ReSource[STMTurn]): Unit = ()
  override def preparationPhase(initialWrites: Set[ReSource[STMTurn]]): Unit = ()
}


