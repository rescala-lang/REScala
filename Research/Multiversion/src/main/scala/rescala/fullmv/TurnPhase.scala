package rescala.fullmv

object TurnPhase {
  type Type = Int
  val dummy: Type = 0

  val Initialized: Type = 1
  val Framing: Type = 2
  val Executing: Type = 3
  val WrapUp: Type = 4
  val Completed: Type = 5
}
