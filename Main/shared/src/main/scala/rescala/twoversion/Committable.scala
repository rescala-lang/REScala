package rescala.twoversion

import rescala.core.Struct

/**
  * Indicates that a class stores buffered changes that can be committed or reverted
  */
trait Committable[S <: Struct] {
  /** Commits the buffered changes. */
  def commit(): Unit

  /** Releases (reverts) the buffered changes. */
  def release(): Unit
}

