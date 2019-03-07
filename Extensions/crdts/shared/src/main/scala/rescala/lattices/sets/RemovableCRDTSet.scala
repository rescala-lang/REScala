package rescala.lattices.sets

trait RemovableCRDTSet[A] extends StateCRDTSet[A] {
  def remove(e: A): RemovableCRDTSet[A]
}
