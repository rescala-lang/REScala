import cats.data.NonEmptyList
import cats.implicits._
import com.monovore.decline.{Command, CommandApp, Opts}

object ConsoleReplication extends CommandApp(
      name = "cr",
      header = "test CRDTs on the commandline",
      main = Commandline.command.options
    )

object Commandline {

  val idArg: Opts[String]                    = Opts.argument[String](metavar = "id")
  val listenPortArg: Opts[Int]               = Opts.argument[Int](metavar = "listenPort")
  val connectArg: Opts[NonEmptyList[String]] = Opts.arguments[String](metavar = "connectTo")

  val peerCommand: Opts[Unit] = Opts.subcommand(
    name = "peer",
    help = "Start peer"
  ) {
    val ipAndPort = """([\d.]*):(\d*)""".r

    (idArg, listenPortArg, connectArg).mapN {
      case (id, listenPort, connections) =>
        val ipsAndPorts = connections.map {
          case ipAndPort(ip, port) => (ip, port.toInt)
        }.toList

        new Peer(id, listenPort, ipsAndPorts).run()
    }
  }

  val listenPeerCommand: Opts[Unit] = Opts.subcommand(
    name = "listenpeer",
    help = "Start peer that only listens"
  ) {
    (idArg, listenPortArg).mapN {
      case (id, listenPort) => new Peer(id, listenPort, List()).run()
    }
  }

  val checkpointerCommand: Opts[Unit] = Opts.subcommand(
    name = "checkpointer",
    help = "Start checkpointer"
  ) {
    listenPortArg.map { new Checkpointer(_).run() }
  }

  val command: Command[Unit] = Command(name = "conrep", header = "test CRTDs on the console") {
    peerCommand orElse listenPeerCommand orElse checkpointerCommand
  }

}
