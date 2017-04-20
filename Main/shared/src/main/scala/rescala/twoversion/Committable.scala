package rescala.twoversion

import rescala.graph.Struct

/**
  * Indicates that a class stores buffered changes that can be committed or reverted
  */
trait Committable[S <: Struct] {
  /**
    * Commits the buffered changes.
    *
    * @param turn Turn to use for committing
    */
  def commit(implicit turn: TwoVersionPropagation[S]): Unit

  /**
    * Releases (reverts) the buffered changes.
    *
    * @param turn Turn to use for committing
    */
  def release(implicit turn: TwoVersionPropagation[S]): Unit
}

