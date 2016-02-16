package rescala.stm

import rescala.graph.{Buffer, BufferedStruct, Pulse}

object STMStruct extends BufferedStruct {
  override type Spore[R] = STMSporeP[_, R]

  override def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): SporeP[P, R] = {
    new STMSporeP[P, R](new STMBuffer[Pulse[P]](initialValue, if (transient) Buffer.transactionLocal else Buffer.keepPulse), initialIncoming)
  }

  class STMSporeP[P, R](override val pulses: STMBuffer[Pulse[P]], initialIncoming: Set[R]) extends BufferedSporeP[P, R](initialIncoming) {
    override def buffer[A](default: A, commitStrategy: (A, A) => A): STMBuffer[A] = new STMBuffer[A](default, commitStrategy)
  }

}
