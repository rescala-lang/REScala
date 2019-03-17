package ersir

import java.nio.file.{Path, Paths}

import cats.implicits._
import com.monovore.decline.{Opts, Command}
import ersir.shared.Log

object Ersir {

  val port      : Opts[Int]     = Opts.option[Int]("port", help = "Weberver listening port.")
                                  .withDefault(9110)
  val interface : Opts[String]  = Opts.option[String]("interface",
                                                      metavar = "interface",
                                                      help = "Interface to bind the server on.")
                                  .withDefault("0")
  val server    : Opts[Boolean] = Opts.flag("noserver", "Do not start the server.")
                                  .orTrue
  val optBasedir: Opts[Path]    = Opts.option[Path]("basedir", short = "b", metavar = "directory",
                                                    help = "Base directory to store settings and data.")
                                  .withDefault(
                                    Paths.get("./"))

  val command: Command[Services] = Command(name = "ersir", header = "Start server!") {
    val services = (optBasedir, interface, port).mapN(new Services(_, _, _))

    (services, server).mapN {
      (services, server) =>

        if (server) {
          services.startServer()
        }
        Log.Main.info(s"initialization done, connect on http://$interface:$port/")
        services
    }
  }


  def main(args: Array[String]): Unit = run(args: _*)

  def run(args: String*): Services = {
    command.parse(args) match {
      case Left(help)     =>
        println(help)
        sys.exit(0)
      case Right(service) => service
    }
  }
}
