package replication.checkpointing

import de.rmgk.options.{Argument, Single, Subcommand}
import replication.checkpointing.central.CentralOptions
import replication.checkpointing.decentral.DecentralOptions

case class CheckpointingOptions(
    decentral: Subcommand[DecentralOptions] = Subcommand(DecentralOptions()),
    central: Subcommand[CentralOptions] = Subcommand(CentralOptions()),
)
