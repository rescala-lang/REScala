package reactive

case class Source[V]() extends Event[V] {
  override def inputs: List[ReSource] = Nil
}
