package replication.checkpointing.decentral
import rdts.base.Uid

case class Checkpoint(replicaID: Uid, counter: Int)
