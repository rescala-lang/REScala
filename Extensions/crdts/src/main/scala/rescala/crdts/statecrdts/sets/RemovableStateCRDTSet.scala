package rescala.crdts.statecrdts
package sets

trait RemovableStateCRDTSet[A] extends StateCRDTSet[A] {
  def remove(e: A): RemovableStateCRDTSet[A]
}
