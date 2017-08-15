package statecrdts
package sequences

trait RemovableSequence[A] extends StateCRDTSequence[A] {
  def remove[A1 >: A](v: Vertex[A1]): selfType
}