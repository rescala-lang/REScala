package replication.checkpointing.decentral
import kofre.base.Id

case class Checkpoint(replicaID: Id, counter: Int)
