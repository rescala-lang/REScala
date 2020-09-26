package ersir

import java.nio.file.{Path, Paths}

import cats.implicits._
import com.monovore.decline.{Opts, Command}

object Ersir {

  val port: Opts[Int] = Opts.option[Int]("port", help = "Weberver listening port.")
    .withDefault(9110)
  val interface: Opts[String] =
    Opts.option[String]("interface", metavar = "interface", help = "Interface to bind the server on.")
      .withDefault("localhost")
  val optBasedir: Opts[Path] = Opts.option[Path](
    "basedir",
    short = "b",
    metavar = "directory",
    help = "Base directory to store settings and data."
  )
    .withDefault(
      Paths.get("./")
    )

  val command: Command[Services] = Command(name = "ersir", header = "Start server!") {
    (optBasedir, interface, port).mapN(new Services(_, _, _)).map { services =>
      scribe.info(s"loading ressources from ${services.basepath}")
      scribe.info(s"starting server, fetching posts …")
      services.startServer()
      scribe.info(s"initialization done, connect on http://${services.interface}:${services.port}/")
      services
    }
  }

  def main(args: Array[String]): Unit = run(args.toSeq: _*)

  def run(args: String*): Services = {
    command.parse(args) match {
      case Left(help) =>
        println(help)
        sys.exit(0)
      case Right(service) => service
    }
  }
}
