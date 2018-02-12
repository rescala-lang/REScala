package rescala.crdts.statecrdts
package sequences

trait RemovableSequence[A] extends StateCRDTSequence[A] {
  type selfType
  def remove[A1 >: A](v: Vertex[A1]): selfType
}
