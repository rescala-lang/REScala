package rescala.parrp

import rescala.graph._
import rescala.levelbased.LevelStructTypeImpl
import rescala.locking.{Key, TurnLock}

import scala.language.implicitConversions


class ParRPStructType[P, S <: Struct](current: P, transient: Boolean, val lock: TurnLock[ParRPInterTurn], initialIncoming: Set[Reactive[S]])
  extends LevelStructTypeImpl[P, S](current, transient, initialIncoming) {


  override def set(value: P)(implicit ticket: S#Ticket[S]): Unit = {
    val turn = ticket.turn()
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
    super.set(value)(ticket)
  }
}
