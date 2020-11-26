package rescala.extra.lattices.delta

case class Dot(replicaID: String, counter: Int) {
  def next: Dot = Dot(replicaID, counter + 1)
}
