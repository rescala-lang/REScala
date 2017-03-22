package rescala.stm

import rescala.graph.{Reactive, Struct}
import rescala.levelbased.{LevelBasedPropagation, LevelStruct}

import scala.concurrent.stm.{InTxn, atomic}

class STMTurn extends LevelBasedPropagation[STMTurn] with LevelStruct {
  override type Type[P, S <: Struct] = STMStructType[P, S]



  /** used to create state containers of each reactive */
  override private[rescala] def makeStructState[P](initialValue: P, transient: Boolean, initialIncoming: Set[Reactive[STMTurn]], hasState: Boolean): Type[P, STMTurn] = {
    new STMStructType[P, STMTurn](initialValue, transient, initialIncoming)
  }
  override def releasePhase(): Unit = ()
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
}


