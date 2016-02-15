package rescala.stm

import rescala.graph.{Buffer, BufferedSpores, Pulse}

import scala.language.implicitConversions




object STMSpores extends BufferedSpores {
  override type Struct[R] = STMStructP[_, R]

  override def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): StructP[P, R] = {
    new STMStructP[P, R](new STMBuffer[Pulse[P]](initialValue, if (transient) Buffer.transactionLocal else Buffer.keepPulse), initialIncoming)
  }

  class STMStructP[P, R](override val pulses: STMBuffer[Pulse[P]], initialIncoming: Set[R]) extends BufferedStructP[P, R](initialIncoming) {
    override def buffer[A](default: A, commitStrategy: (A, A) => A): STMBuffer[A] = new STMBuffer[A](default, commitStrategy)
  }

}
