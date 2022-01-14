
package encrdt.crdts.interfaces

trait Crdt[T] {
  def merge(state: T): Unit
  def state: T
}
