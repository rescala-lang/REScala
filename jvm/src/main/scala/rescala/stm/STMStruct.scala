package rescala.stm

import rescala.graph.Struct.TraitSporeP
import rescala.graph.{Committable, Buffer, BufferedStruct, Pulse}
import rescala.propagation.Turn

import scala.concurrent.stm.{InTxn, Ref}

object STMStruct extends BufferedStruct {
  override type Spore[R] = STMSporeP[_, R]

  override def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): SporeP[P, R] = {
    new STMSporeP[P, R](initialValue, transient, initialIncoming)
  }

  class STMSporeP[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]) extends TraitSporeP[P, R] with Buffer[Pulse[P]] with Committable {

    implicit def inTxn(implicit turn: Turn[_]): InTxn = turn match {
      case stmTurn: STMTurn => stmTurn.inTxn
      case _ => throw new IllegalStateException(s"$turn has invalid type for $this")
    }

    val _level: Ref[Int] = Ref(0)
    val _outgoing: Ref[Set[R]] = Ref(Set.empty)
    val _incoming: Ref[Set[R]] = Ref(initialIncoming)

    override val pulses: Buffer[Pulse[P]] = this
    override def incoming(implicit turn: Turn[_]): Set[R] = _incoming.get
    override def level(implicit turn: Turn[_]): Int = _level.get
    override def drop(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing.transformAndGet(_ - reactive)
    override def updateLevel(i: Int)(implicit turn: Turn[_]): Int = _level.transformAndGet(math.max(_, i))
    override def outgoing(implicit turn: Turn[_]): Set[R] = _outgoing.get
    override def updateIncoming(reactives: Set[R])(implicit turn: Turn[_]): Unit = _incoming.set(reactives)
    override def discover(reactive: R)(implicit turn: Turn[_]): Unit = _outgoing.transformAndGet(_ + reactive)


    private val current: Ref[Pulse[P]] = Ref(initialValue)
    private val update: Ref[Option[Pulse[P]]] = Ref(None)

    override def transform(f: (Pulse[P]) => Pulse[P])(implicit turn: Turn[_]): Pulse[P] = {
      val value = f(get)
      set(value)
      value
    }
    override def set(value: Pulse[P])(implicit turn: Turn[_]): Unit = {
      update.set(Some(value))
      turn.schedule(this)
    }
    override def base(implicit turn: Turn[_]) = current.get
    override def get(implicit turn: Turn[_]): Pulse[P] = update.get.getOrElse(current.get)
    override def release(implicit turn: Turn[_]): Unit = {
      update.set(None)
    }

    override def commit(implicit turn: Turn[_]): Unit = {
      val updateValue: Option[Pulse[P]] = update.get
      if (!transient && updateValue.isDefined) current.set(updateValue.get.keep)
      release(turn)
    }
  }

}
