package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.DotStore.{DotFun, DotPair}
import rescala.extra.lattices.delta.interfaces.RGAInterface
import rescala.extra.lattices.delta.interfaces.RGAInterface.{RGACompanion, State}
import rescala.extra.lattices.delta.{CContext, UIJDLattice}

class RGA[E, C: CContext](
    val state: State[E, C],
    protected val antiEntropy: AntiEntropy[State[E, C]]
) extends RGAInterface[E, C, RGA[E, C]] with BasicCRDT[State[E, C], RGA[E, C]] {

  override protected def copy(state: State[E, C]): RGA[E, C] = new RGA(state, antiEntropy)
}

object RGA extends RGACompanion {
  def apply[E, C: CContext](antiEntropy: AntiEntropy[State[E, C]]): RGA[E, C] =
    new RGA(UIJDLattice[State[E, C]].bottom, antiEntropy)
}
