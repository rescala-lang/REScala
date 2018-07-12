package rescala.crdts.statecrdts.sequences

trait RemovableSequence[A] {
  type selfType
  def remove(v: ValueVertex[A]): selfType
}
