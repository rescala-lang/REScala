package rescala.scheduler

/** Indicates that a class stores buffered changes that can be committed or reverted */
trait Committable[V] {

  /** Commits the buffered changes. */
  def commit(unchange: V => V): Unit

  /** Releases (reverts) the buffered changes. */
  def release(): Unit
}
