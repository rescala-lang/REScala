package replication.checkpointing.decentral

import de.rmgk.options.{Argument, Single, Style}

case class DecentralOptions(
    id: Argument[String, Single, Style.Named] = Argument(),
    listenPort: Argument[Int, Single, Style.Named] = Argument(),
    connectTo: Argument[String, List, Style.Named] = Argument(_.valueName("<ip:port>"), default = Some(Nil)),
    initSize: Argument[Int, Single, Style.Named] = Argument(),
)
