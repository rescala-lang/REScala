package rescala.core

trait ComputationStateAccess[S <: Struct] {
  private[rescala] def staticBefore[P](reactive: ReSourciV[P, S]): P
  private[rescala] def staticAfter[P](reactive: ReSourciV[P, S]): P
  private[rescala] def dynamicBefore[P](reactive: ReSourciV[P, S]): P
  private[rescala] def dynamicAfter[P](reactive: ReSourciV[P, S]): P
}





