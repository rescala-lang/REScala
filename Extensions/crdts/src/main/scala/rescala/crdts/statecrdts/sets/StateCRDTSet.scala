package rescala.crdts.statecrdts
package sets

trait StateCRDTSet[A] extends StateCRDT {
  override type valueType = Set[A]

  def add(e: A): StateCRDTSet[A]

  def contains(e: A): Boolean
}
