package rescala.crdts.statecrdts
package sets

trait RemovableCRDTSet[A] extends StateCRDTSet[A] {
  def remove(e: A): RemovableCRDTSet[A]
}
