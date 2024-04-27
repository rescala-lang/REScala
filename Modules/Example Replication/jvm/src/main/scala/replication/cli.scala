package replication

import de.rmgk.options.*
import rdts.base.Uid
import replication.fbdc.{CliConnections, FbdcCli}

import java.nio.file.{Files, Path}

given [T](using avp: ArgumentValueParser[T]): ArgumentValueParser[List[T]] with
  override def apply(args: List[String]): (Option[List[T]], List[String]) =
    def rec(remaining: List[String], acc: List[T]): (Option[List[T]], List[String]) =
      if remaining.isEmpty
      then (Some(acc), Nil)
      else
        val (value, rest) = avp.apply(remaining)
        value match
          case None    => (Some(acc.reverse), remaining)
          case Some(v) => rec(rest, v :: acc)
    rec(args, Nil)

  override def valueDescription: String = s"${avp.valueDescription}*"

object cli {
  def main(args: Array[String]): Unit =

    val id         = named[String]("--id", "")
    val listenPort = named[Int]("--listenPort", "")

    val ipAndPort = """([\d.]*):(\d*)""".r

    given argValParser: ArgumentValueParser[(String, Int)] with
      override def apply(args: List[String]): (Option[(String, Int)], List[String]) =
        args match {
          case ipAndPort(ip, port) :: rest => (Some((ip, Integer.parseInt(port))), rest)
          case _                           => (None, args)
        }

      override def valueDescription: String = "<ip:port>"
    end argValParser

    val argparse = argumentParser {
      inline def ipsAndPorts = named[List[(String, Int)]]("--connectTo", "<ip:port>", Nil)

      subcommand("calendar", ""):
        new replication.calendar.Peer(Uid.predefined(id.value), listenPort.value, ipsAndPorts.value).run()
      .value

      subcommand("dtn", ""):
        dtn.run()
      .value

      subcommand("conn", ""):
        val settings = CliConnections(
          named[Option[Int]]("--tcp-listen-port", "tcp listen port", None).value,
          named[List[(String, Int)]]("--tcp-connect", "connections", Nil).value,
          named[Option[Int]]("--webserver-listen-port", "webserver listen port", None).value,
          named[Option[Path]]("--webserver-static-path", "webserver static path", None).value,
          named[Option[Path]]("--northwind-path", "northwind sqlite database path", None).value,
        )
        settings.`webserver-static-path` match
          case None =>
          case Some(path) =>
            assert(Files.exists(path), s"path $path must exist if specified")
        val serv = new FbdcCli(settings)
        serv.start()
      .value
    }

    println(argparse)
    argparse.parse(args.toList).printHelp()
}
