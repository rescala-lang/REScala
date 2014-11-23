package rescala

import rescala.propagation.turns.creation.TurnFactory
import rescala.propagation.turns.instances.{Pessimistic, Synchronized, UnSynchronized}

object Implicits {
   implicit def default: TurnFactory = pessimistic
   implicit def pessimistic: TurnFactory = Pessimistic
   implicit def synchronized: TurnFactory = Synchronized
   implicit def unmanaged: TurnFactory = UnSynchronized
 }
