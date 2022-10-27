package com.github.ckuessner.encrdt.sync

import java.net.URI

trait ConnectionManager[S] {
  val localReplicaId: String

  def stateChanged(newState: S): Unit

  def connectToReplica(remoteReplicaId: String, uri: URI): Unit

  def stop(): Unit

  def uri: URI

  def remoteAddresses: Set[String]
}
