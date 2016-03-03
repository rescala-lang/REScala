package rescala.stm

import rescala.graph.{Pulse, Buffer, Reactive}
import rescala.propagation.LevelBasedPropagation

import scala.concurrent.stm.{InTxn, atomic}

class STMTurn extends LevelBasedPropagation[STMStruct.type] {

  /** used to create state containers of each reactive */
  override def bufferFactory: STMStruct.type = STMStruct
  override def releasePhase(): Unit = ()
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)

  type S = STMStruct.type

  override def pulses[P](budP: S#SporeP[P, Reactive[S]]): Buffer[Pulse[P]] = budP.pulses.asInstanceOf[Buffer[Pulse[P]]]
  override def incoming[R](bud: S#Spore[R]): Set[R] = bud.incoming(this)
  override def updateIncoming[R](bud: S#Spore[R], newDependencies: Set[R]): Unit = bud.updateIncoming(newDependencies)(this)

}


