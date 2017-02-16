package rescala.twoversion

import rescala.propagation.Turn

/**
  * Indicates that a class stores buffered changes that can be committed or reverted
  */
trait Committable {
  /**
    * Commits the buffered changes.
    *
    * @param turn Turn to use for committing
    */
  def commit(implicit turn: Turn[_]): Unit

  /**
    * Releases (reverts) the buffered changes.
    *
    * @param turn Turn to use for committing
    */
  def release(implicit turn: Turn[_]): Unit
}

