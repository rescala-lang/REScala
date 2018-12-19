package rescala.crdts.statecrdts.sequences

import rescala.crdts.statecrdts.StateCRDT
import rescala.crdts.statecrdts.sets.RemovableCRDTSet

trait RemovableCRDTSequence[A] extends CRDTSequence[A] {
  def vertices: RemovableCRDTSet[Vertex[A]]

  def remove(v: Vertex[A])(implicit stateCRDT: StateCRDT[List[A], RemovableCRDTSequence[A]]): RemovableCRDTSequence[A] =
    stateCRDT.fromPayload((vertices.remove(v), edges))
}
