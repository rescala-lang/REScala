package rescala

import rescala.propagation.turns.creation.Engine

object Implicits {
  implicit def default: Engine = pessimistic
  implicit def pessimistic: Engine = Engine.pessimistic
  implicit def synchronized: Engine = Engine.synchronized
  implicit def unmanaged: Engine = Engine.unSynchronized
}
