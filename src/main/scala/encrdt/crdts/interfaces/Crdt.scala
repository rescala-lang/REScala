package de.ckuessner
package encrdt.crdts.interfaces

trait Crdt[T] {
  def merge(state: T): Unit

}
