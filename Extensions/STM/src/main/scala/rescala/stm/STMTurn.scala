package rescala.stm

import rescala.graph.Pulse.NoChange
import rescala.graph.{Change, Pulse, Reactive, Struct}
import rescala.levelbased.{LevelBasedPropagation, LevelStruct}
import rescala.twoversion.Token

import scala.concurrent.stm.{InTxn, atomic}

class STMTurn extends LevelBasedPropagation[STMTurn] with LevelStruct {
  override type State[P, S <: Struct] = STMStructType[P, S]

  /** used to create state containers of each reactive */
  override protected def makeStructState[P](valueOrTransient: Option[Change[P]], hasAccumulatingState: Boolean = false): State[Pulse[P], STMTurn] = {
    new STMStructType[Pulse[P], STMTurn](valueOrTransient.getOrElse(NoChange), valueOrTransient.isEmpty)
  }
  override def releasePhase(): Unit = ()
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
  override val token = Token(inTxn)
}


