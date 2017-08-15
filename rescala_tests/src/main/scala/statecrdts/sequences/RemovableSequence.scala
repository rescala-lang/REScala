package statecrdts
package sequences

trait RemovableSequence extends StateCRDTSequence {
  def remove[A1 >: Atom](v: Vertex[A1]): selfType
}