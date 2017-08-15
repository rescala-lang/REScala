package statecrdts
package sets

trait StateCRDTSet extends StateCRDT {
  type Element

  def add(e: Element): selfType

  def remove(e: Element): selfType

  def contains(e: Element): Boolean
}