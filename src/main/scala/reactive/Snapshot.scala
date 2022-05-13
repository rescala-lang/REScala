package reactive

case class Snapshot[B](input: Event[_], fold: Fold[B]) extends Event[B] {
  override def inputs: List[ReSource] = List(input, fold)
}

extension [V] (e: Event[V]) def snapshot[B](other: Fold[B]): Snapshot[B] = Snapshot(e, other)
