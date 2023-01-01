package calendar

import cats.implicits._
import com.monovore.decline.{Command, CommandApp, Opts}

object ConsoleReplication extends CommandApp(
      name = "cr",
      header = "test CRDTs on the commandline",
      main = Commandline.command.options
    )

object Commandline {

  val idArg: Opts[String]            = Opts.argument[String](metavar = "id")
  val listenPortArg: Opts[Int]       = Opts.argument[Int](metavar = "listenPort")
  val connectArg: Opts[List[String]] = Opts.arguments[String](metavar = "connectTo").orEmpty

  val peerCommand: Opts[Unit] = Opts.subcommand(
    name = "peer",
    help = "Start a new peer"
  ) {
    val ipAndPort = """([\d.]*):(\d*)""".r

    (idArg, listenPortArg, connectArg).mapN {
      case (id, listenPort, connections) =>
        val ipsAndPorts = connections.collect {
          case ipAndPort(ip, port) => (ip, port.toInt)
        }

        new Peer(id, listenPort, ipsAndPorts).run()
    }
  }

  val command: Command[Unit] = Command(name = "conrep", header = "test CRTDs on the console") {
    peerCommand
  }

}
