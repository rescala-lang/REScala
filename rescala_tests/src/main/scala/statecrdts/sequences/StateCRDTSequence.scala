package statecrdts
package sequences

trait StateCRDTSequence extends StateCRDT {
  type Atom

  def contains[A1 >: Atom](v: Vertex[A1]): Boolean

  def before[A1 >: Atom](u: Vertex[A1], v: Vertex[A1]): Boolean
}