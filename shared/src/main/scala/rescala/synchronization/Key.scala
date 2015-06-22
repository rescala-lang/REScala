package rescala.synchronization

import rescala.graph.ITurnLock
import rescala.turns.Turn

trait Key {
  def turn: Turn
  def continue(): Unit
  def await(): Unit
  def lockKeychain[R](f: => R): R
  def addLock(lock: ITurnLock): Unit
  def grabLocks(): List[ITurnLock]
  def releaseAll(): Unit
}
