package decentral

import cats.data.NonEmptyList
import cats.implicits._
import com.monovore.decline.{Command, CommandApp, Opts}

object ConsoleReplication extends CommandApp(
      name = "cr",
      header = "test CRDTs on the commandline",
      main = Commandline.command.options
    )

object Commandline {

  val idArg: Opts[String]      = Opts.argument[String](metavar = "id")
  val listenPortArg: Opts[Int] = Opts.argument[Int](metavar = "listenPort")
  val connectArg: Opts[List[String]] = Opts.arguments[String](metavar = "connectTo").orNone.map {
    case None      => List.empty[String]
    case Some(nel) => nel.toList
  }
  val initSizeArg: Opts[Int] = Opts.argument[Int](metavar = "initSize")

  val replicaCommand: Opts[Unit] = Opts.subcommand(
    name = "replica",
    help = "Start a new replica"
  ) {
    val ipAndPort = """([\d.]*):(\d*)""".r

    (idArg, listenPortArg, connectArg, initSizeArg).mapN {
      case (id, listenPort, connectTo, initSize) =>
        val ipsAndPorts = connectTo.map {
          case ipAndPort(ip, port) => (ip, port.toInt)
        }.toList

        new Replica(listenPort, ipsAndPorts, id, initSize).run()
    }
  }

  val command: Command[Unit] = Command(name = "conrep", header = "test CRTDs on the console") {
    replicaCommand
  }

}
