package replication

import de.rmgk.options.*
import replication.calendar.{CalendarOptions, Peer}
import replication.checkpointing.CheckpointingOptions
import replication.checkpointing.central.{CentralOptions, Checkpointer}
import replication.checkpointing.decentral.{DecentralOptions, Replica}
import kofre.base.Uid
import replication.fbdc.FbdcCli

import java.nio.file.Path

case class CliArgs(
    calendar: Subcommand[CalendarOptions] = Subcommand(CalendarOptions()),
    dtn: Subcommand[Unit] = Subcommand.empty(),
    checkpointing: Subcommand[CheckpointingOptions] = Subcommand(CheckpointingOptions()),
    conn: Subcommand[fbdc.CliConnections] = Subcommand(fbdc.CliConnections()),
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

        new Peer(Uid.predefined(calArgs.id.value), calArgs.listenPort.value, ipsAndPorts).run()

    instance.checkpointing.value.flatMap(_.decentral.value) match
      case None =>
      case Some(decArgs) =>
        val ipsAndPorts = decArgs.connectTo.value.collect {
          case ipAndPort(ip, port) => (ip, port.toInt)
        }

        new Replica(
          decArgs.listenPort.value,
          ipsAndPorts,
          Uid.predefined(decArgs.id.value),
          decArgs.initSize.value
        ).run()

    instance.checkpointing.value.flatMap(_.central.value) match
      case None =>
      case Some(centralArgs) =>
        centralArgs.peer.value match
          case None =>
          case Some(centralPeerArgs) =>
            val ipsAndPorts = centralPeerArgs.connectTo.value.collect {
              case ipAndPort(ip, port) => (ip, port.toInt)
            }

            new replication.checkpointing.central.Peer(
              Uid.predefined(centralPeerArgs.id.value),
              centralPeerArgs.listenPort.value,
              ipsAndPorts
            ).run()
        centralArgs.checkpointer.value match
          case None =>
          case Some(checkpointerArgs) =>
            new Checkpointer(checkpointerArgs.listenPort.value).run()

    if instance.dtn.value.isDefined then dtn.run()

    instance.conn.value match
      case None =>
      case Some(connections) =>
        val serv = new FbdcCli(connections)
        serv.start()
        ()

  }
}
