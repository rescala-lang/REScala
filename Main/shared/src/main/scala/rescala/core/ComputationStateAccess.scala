package rescala.core

trait ComputationStateAccess[S <: Struct] {
  private[core] def staticAfter[P](reactive: ReSourciV[P, S]): P
  private[core] def dynamicBefore[P](reactive: ReSourciV[P, S]): P
  private[core] def dynamicAfter[P](reactive: ReSourciV[P, S]): P
}





