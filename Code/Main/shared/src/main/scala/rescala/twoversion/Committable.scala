package rescala.twoversion

/**
  * Indicates that a class stores buffered changes that can be committed or reverted
  */
trait Committable {
  /** Commits the buffered changes. */
  def commit(): Unit

  /** Releases (reverts) the buffered changes. */
  def release(): Unit
}

