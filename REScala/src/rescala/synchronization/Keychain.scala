package rescala.synchronization

import rescala.graph.Globals

class Keychain(init: Key) {

  val id = Globals.nextID()
  override def toString = s"Keychain($id)"

  /** synchronized on this */
  var keys: List[Key] = init :: Nil

  def append(other: Keychain): Unit = {
    assert(this ne other, s"tried to append $this to itself")
    Keychains.locked(this, other) {
        other.keys.foreach(_.keychain = this)
        keys = keys ::: other.keys
    }
  }
}
