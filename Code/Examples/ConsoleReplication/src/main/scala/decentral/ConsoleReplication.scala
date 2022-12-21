package decentral

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
    (idArg, listenPortArg, connectArg, initSizeArg).mapN {
      case (id, listenPort, connectTo, initSize) =>
        val hostsAndPorts = connectTo.collect { hostAndIp =>
          val lastColon = hostAndIp.lastIndexOf(':')
          (hostAndIp.substring(0, lastColon), hostAndIp.substring(lastColon + 1).toInt)
        }

        new Replica(listenPort, hostsAndPorts, id, initSize).run()
    }
  }

  val command: Command[Unit] = Command(name = "conrep", header = "test CRTDs on the console") {
    replicaCommand
  }

}
