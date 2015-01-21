package rescala.synchronization

import rescala.graph.Globals

import scala.collection.immutable.Queue

class Keychain(init: Key) {


  val id = Globals.nextID()
  override def toString = s"Keychain($id)"

  /** synchronized on this */
  var keys: Queue[Key] = Queue(init)

  def append(other: Keychain): Unit = {
    assert(this ne other, s"tried to append $this to itself")
    Keychains.locked(this, other) {
      other.keys.foreach(_.keychain = this)
      keys = keys.enqueue(other.keys)
    }
  }

  def isHead(key: Key): Boolean = keys.nonEmpty && (keys.head eq key)

  def releaseHead() = {
    val (h, r) = keys.dequeue
    keys = r
    if (keys.isEmpty) h.transferAll(null)
    else {
      val target = keys.head
      target.synchronized {
        h.transferAll(target)
        target.notify()
      }
    }
  }
}
