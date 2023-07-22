package replication

import de.rmgk.options.*
import kofre.base.Uid
import replication.checkpointing.central.Checkpointer
import replication.fbdc.{CliConnections, FbdcCli}

import java.nio.file.Path
import scala.util.boundary

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

    given ArgumentValueParser[(String, Int)] with
      override def apply(args: List[String]): (Option[(String, Int)], List[String]) =
        args match {
          case ipAndPort(ip, port) :: rest => (Some((ip, Integer.parseInt(port))), rest)
          case _ => (None, args)
        }

      override def valueDescription: String = "<ip:port>"

    parseArguments(args.toList) {


      val ipsAndPorts = named[List[(String, Int)]]("--connectTo", "<ip:port>", Nil).value


      boundary {

        subcommand("calendar", ""):
          new replication.calendar.Peer(Uid.predefined(id.value), listenPort.value, ipsAndPorts).run()
          boundary.break()

        subcommand("checkpointing", ""):
          subcommand("decentral", ""):
            new replication.checkpointing.decentral.Replica(
              listenPort.value,
              ipsAndPorts,
              Uid.predefined(id.value),
              named[Int]("--initSize", "").value
            ).run()
            boundary.break()

          subcommand("central", ""):
            subcommand("peer", ""):
              new replication.checkpointing.central.Peer(
                Uid.predefined(id.value),
                listenPort.value,
                ipsAndPorts
              ).run()
              boundary.break()
            subcommand("checkpointer", ""):
              new Checkpointer(listenPort.value).run()
              boundary.break()

        subcommand("dtn", ""):
          dtn.run()
          boundary.break()

        subcommand("conn", ""):
          val serv = new FbdcCli(CliConnections(
            named[Option[Int]]("--tcp-listen-port", "tcp listen port", None).value,
            named[List[(String, Int)]]("--tcp-connect", "connections", Nil).value,
            named[Option[Int]]("--webserver-listen-port", "webserver listen port", None).value,
            named[Option[Path]]("--webserver-static-path", "webserver static path", None).value,
            named[Option[Path]]("--northwind-path", "northwind sqlite database path", None).value,
          ))
          serv.start()
          boundary.break()

        throw ParseException("no subcommand matched")
      }
    }.printHelp()
}
