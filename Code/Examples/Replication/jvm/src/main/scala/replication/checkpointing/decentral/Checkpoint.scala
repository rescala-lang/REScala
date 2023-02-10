package replication.checkpointing.decentral
import kofre.base.Uid

case class Checkpoint(replicaID: Uid, counter: Int)
