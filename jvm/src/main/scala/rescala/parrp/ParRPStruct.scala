package rescala.parrp

import rescala.graph._
import rescala.propagation.Turn

import scala.language.implicitConversions

object ParRPStruct extends LevelStruct {
  override type Spore[R] = ParRPSporeP[_, R]

  override def bud[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): SporeP[P, R] = {
    val lock = new TurnLock
    new ParRPSporeP[P, R](initialValue, transient, lock, initialIncoming)
  }


}


class ParRPSporeP[P, R](current: Pulse[P], transient: Boolean, val lock: TurnLock, initialIncoming: Set[R]) extends LevelSporeImpl[P, R](current, transient, initialIncoming) {
  override def set(value: Pulse[P])(implicit turn: Turn[_]): Unit = {
    assert(turn match {
      case pessimistic: ParRP =>
        val wlo: Option[Key] = Option(lock).map(_.getOwner)
        assert(wlo.fold(true)(_ eq pessimistic.key),
          s"buffer owned by $owner, controlled by $lock with owner ${wlo.get}" +
            s" was written by $turn who locks with ${pessimistic.key}, by now the owner is ${lock.getOwner}")
        true
      case _ =>
        throw new IllegalStateException(s"parrp buffer used with wrong turn")
    })
    super.set(value)
  }
}