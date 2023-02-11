package replication.checkpointing.central

import de.rmgk.options.*

case class CentralPeerOptions(
    id: Argument[String, Single, Style.Named] = Argument(),
    listenPort: Argument[Int, Single, Style.Named] = Argument(),
    connectTo: Argument[String, List, Style.Named] = Argument(_.valueName("<ip:port>"), default = Some(Nil)),
    initSize: Argument[Int, Single, Style.Named] = Argument(),
)

case class CentralCheckpointerOptions(
    listenPort: Argument[Int, Single, Style.Named] = Argument(),
)

case class CentralOptions(
    peer: Subcommand[CentralPeerOptions] = Subcommand(CentralPeerOptions()),
    checkpointer: Subcommand[CentralCheckpointerOptions] = Subcommand(CentralCheckpointerOptions())
)
