package rescala.parrp

import rescala.graph._
import rescala.locking.{TurnLock, Key}
import rescala.propagation.Turn

import scala.language.implicitConversions

object ParRPStruct extends LevelStruct {
  override type SporeP[P, R] = ParRPSporeP[P, R]

}


class ParRPSporeP[P, R](current: Pulse[P], transient: Boolean, val lock: TurnLock[ParRPInterTurn], initialIncoming: Set[R])
  extends LevelSporeImpl[P, R](current, transient, initialIncoming) with ReactiveSpore[P, R]  {

  override def set(value: Pulse[P])(implicit turn: Turn[_]): Unit = {
    assert(turn match {
      case pessimistic: ParRP =>
        val wlo: Option[Key[ParRPInterTurn]] = Option(lock).map(_.getOwner)
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