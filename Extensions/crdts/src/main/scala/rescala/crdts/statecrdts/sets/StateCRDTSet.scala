package rescala.crdts.statecrdts
package sets

trait StateCRDTSet[A] {
  def add(e: A): StateCRDTSet[A]

  def value: Set[A]
  def contains(e: A): Boolean
}
