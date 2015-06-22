package rescala.graph

import rescala.synchronization.Key

trait ITurnLock {
  def getOwner: Key

  /** returns true if key owns the write lock */
  def isOwner(key: Key): Boolean

  /**
   * locks this if it is free, returns the current owner (which is key, if locking succeeded)
   * does not check for shared access.
   */
  def tryLock(key: Key): Key

  def share(key: Key): Unit
  def acquired(key: Key): Key

  /** transfers the lock from the turn to the target. */
  def transfer(target: Key, oldOwner: Key, ignoreShared: Boolean = false): Unit
}

object NoLock extends ITurnLock {
  override def getOwner: Key = null
  /**
   * locks this if it is free, returns the current owner (which is key, if locking succeeded)
   * does not check for shared access.
   */
  override def tryLock(key: Key): Key = key
  override def share(key: Key): Unit = ()
  /** transfers the lock from the turn to the target. */
  override def transfer(target: Key, oldOwner: Key, ignoreShared: Boolean): Unit = ()
  /** returns true if key owns the write lock */
  override def isOwner(key: Key): Boolean = true
  override def acquired(key: Key): Key = key
}
