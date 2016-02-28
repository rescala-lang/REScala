package rescala.propagation

trait Committable {
  def commit(implicit turn: Turn[_]): Unit
  def release(implicit turn: Turn[_]): Unit
}

