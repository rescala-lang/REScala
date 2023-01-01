package replication

import de.rmgk.options.*
import replication.calendar.{CalendarOptions, Peer}
import replication.central.{CentralOptions, Checkpointer}
import replication.decentral.{DecentralOptions, Replica}

case class CliArgs(
    calendar: Subcommand[CalendarOptions] = Subcommand(CalendarOptions()),
    decentral: Subcommand[DecentralOptions] = Subcommand(DecentralOptions()),
    central: Subcommand[CentralOptions] = Subcommand(CentralOptions()),
    dtn: Subcommand[Unit] = Subcommand.empty()
)

object cli {
  def main(args: Array[String]): Unit = {

    val instance = CliArgs()

    val parser = makeParser(
      instance,
      { b =>
        scopt.OParser.sequence(
          b.programName("repl-cli"),
          b.help("help").text("prints this usage text")
        )
      }
    )
    scopt.OParser.parse(parser, args, instance)

    val ipAndPort = """([\d.]*):(\d*)""".r

    instance.calendar.value match
      case None =>
      case Some(calArgs) =>
        val ipsAndPorts = calArgs.connectTo.value.collect {
          case ipAndPort(ip, port) => (ip, port.toInt)
        }

        new Peer(calArgs.id.value, calArgs.listenPort.value, ipsAndPorts).run()

    instance.decentral.value match
      case None =>
      case Some(decArgs) =>
        val ipsAndPorts = decArgs.connectTo.value.collect {
          case ipAndPort(ip, port) => (ip, port.toInt)
        }

        new Replica(decArgs.listenPort.value, ipsAndPorts, decArgs.id.value, decArgs.initSize.value).run()

    instance.central.value match
      case None =>
      case Some(centralArgs) =>
        centralArgs.peer.value match
          case None =>
          case Some(centralPeerArgs) =>
            val ipsAndPorts = centralPeerArgs.connectTo.value.collect {
              case ipAndPort(ip, port) => (ip, port.toInt)
            }

            new replication.central.Peer(centralPeerArgs.id.value, centralPeerArgs.listenPort.value, ipsAndPorts).run()
        centralArgs.checkpointer.value match
          case None =>
          case Some(checkpointerArgs) =>
            new Checkpointer(checkpointerArgs.listenPort.value).run()

    if instance.dtn.value.isDefined then dtn.run()

  }
}
