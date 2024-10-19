package probench

import channels.TCP
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import de.rmgk.options.*
import probench.data.{ClientNodeState, KVOperation, Request}
import rdts.base.Uid
import rdts.datatypes.experiments.protocols.Membership
import rdts.datatypes.experiments.protocols.simplified.Paxos

import java.net.Socket
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext

object cli {

  private val executor: ExecutorService = Executors.newCachedThreadPool()
  private val ec: ExecutionContext      = ExecutionContext.fromExecutor(executor)

  def main(args: Array[String]): Unit = {
    val name       = named[String]("--name", "")
    val clientPort = named[Int]("--listen-client-port", "")
    val peerPort   = named[Int]("--listen-peer-port", "")

    val ipAndPort = """(.+):(\d*)""".r

    given ipAndPortParser: ArgumentValueParser[(String, Int)] with
      override def apply(args: List[String]): (Option[(String, Int)], List[String]) =
        args match {
          case ipAndPort(ip, port) :: rest => (Some((ip, Integer.parseInt(port))), rest)
          case _                           => (None, args)
        }

      override def valueDescription: String = "[<ip:port>]"
    end ipAndPortParser

    given uidParser: ArgumentValueParser[Uid] with
      override def apply(args: List[String]): (Option[Uid], List[String]) =
        args match {
          case string :: rest => (Some(Uid.predefined(string)), rest)
          case _              => (None, args)
        }

      override def valueDescription: String = "[uid]"
    end uidParser

    given JsonValueCodec[ClientNodeState] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    given JsonValueCodec[Membership[Request, Paxos, Paxos]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    val argparse = argumentParser {
      inline def cluster           = named[List[(String, Int)]]("--cluster", "[<ip:port>]")
      inline def initialClusterIds = named[List[Uid]]("--initial-cluster-ids", "[name]")

      inline def clientNode = named[(String, Int)]("--node", "<ip:port>")

      subcommand("node", "starts a cluster node") {
        val node = Node(name.value, initialClusterIds.value.toSet)

        node.addClientConnection(TCP.listen(TCP.defaultSocket("localhost", clientPort.value), ec))
        node.addClusterConnection(TCP.listen(TCP.defaultSocket("localhost", peerPort.value), ec))

        cluster.value.foreach { (ip, port) =>
          node.addClusterConnection(TCP.connect(() => Socket(ip, port), ec))
        }
      }.value

      subcommand("client", "starts a client to interact with a node") {
        val client = Client()

        val (ip, port) = clientNode.value

        client.addLatentConnection(TCP.connect(() => Socket(ip, port), ec))

        client.startCLI()
      }.value

      subcommand("benchmark", "") {}.value
    }

    argparse.parse(args.toList).printHelp()
  }

}
