package rescala.core

trait StateAccess[S <: Struct] {
  private[rescala] def staticBefore/*aka regRead/depRead*/[P](reactive: Pulsing[P, S]): P
  private[rescala] def staticAfter[P](reactive: Pulsing[P, S]): P
  private[rescala] def dynamicBefore[P](reactive: Pulsing[P, S]): P
  private[rescala] def dynamicAfter[P](reactive: Pulsing[P, S]): P
}
