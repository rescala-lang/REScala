package rescala.extra.lattices.delta.crdt.reactive

import kofre.Defs.Id
import kofre.decompose.interfaces.GCounterInterface.State
import kofre.decompose.{Delta, UIJDLattice}
import kofre.decompose.interfaces.PNCounterModule.PNCounter
import kofre.syntax.AllPermissionsCtx

/** [[ReactiveCRDT Reactive]] implementation of [[rescala.extra.lattices.delta.interfaces.PNCounterInterface PNCounterInterface]]
  *
  * Instead of the class constructor, you should use the apply method of the companion object to create new instances.
  */
class ReactivePNCounter(
    val state: PNCounter,
    val replicaID: String,
    val deltaBuffer: List[Delta[PNCounter]]
) extends ReactiveCRDT[PNCounter, ReactivePNCounter] {

  override protected def copy(state: PNCounter, deltaBuffer: List[Delta[PNCounter]]): ReactivePNCounter =
    new ReactivePNCounter(state, replicaID, deltaBuffer)
}

object ReactivePNCounter {

  implicit val rpnCounterContext: AllPermissionsCtx[ReactivePNCounter, PNCounter] = new AllPermissionsCtx[ReactivePNCounter, PNCounter] {
    override def replicaId(c: ReactivePNCounter): Id = c.replicaID
    override def mutate(c: ReactivePNCounter, delta: (State, State)): ReactivePNCounter = c.mutate((_, _) => delta)
    override def query(c: ReactivePNCounter): (State, State) = c.state
  }

  /** Creates a new PNCounter instance
    *
    * @param replicaID Unique id of the replica that this instance is located on
    */
  def apply(replicaID: String): ReactivePNCounter = new ReactivePNCounter(UIJDLattice[PNCounter].bottom, replicaID, List())
}
