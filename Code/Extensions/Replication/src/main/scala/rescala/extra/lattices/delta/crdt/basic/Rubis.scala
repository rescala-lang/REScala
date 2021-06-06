package rescala.extra.lattices.delta.crdt.basic

import rescala.extra.lattices.delta.interfaces.RubisInterface
import rescala.extra.lattices.delta.interfaces.RubisInterface.{RubisCompanion, State}
import rescala.extra.lattices.delta.{CContext, UIJDLattice}

class Rubis[C: CContext](
    protected[rescala] val state: State[C],
    protected val antiEntropy: AntiEntropy[State[C]]
) extends RubisInterface[C, Rubis[C]] with BasicCRDT[State[C], Rubis[C]] {

  override protected def copy(state: State[C]): Rubis[C] = new Rubis(state, antiEntropy)
}

object Rubis extends RubisCompanion {
  def apply[C: CContext](antiEntropy: AntiEntropy[State[C]]): Rubis[C] =
    new Rubis(UIJDLattice[State[C]].bottom, antiEntropy)
}
