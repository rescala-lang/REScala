package rescala

import rescala.propagation.turns.creation.TurnFactory

object Implicits {
  implicit def default: TurnFactory = pessimistic
  implicit def pessimistic: TurnFactory = TurnFactory.pessimistic
  implicit def synchronized: TurnFactory = TurnFactory.synchronized
  implicit def unmanaged: TurnFactory = TurnFactory.unSynchronized
}
