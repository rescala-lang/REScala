package de.ckuessner
package encrdt.experiments

trait Crdt {
  type StateT

  // Will probably change later on
  type ReplicaId = Int

  def replicaId: ReplicaId

  def state: StateT
  def merge(remote: StateT)
}
