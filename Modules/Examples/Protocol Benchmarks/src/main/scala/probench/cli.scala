package probench

import channels.{Abort, NioTCP, TCP, UDP}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import de.rmgk.options.*
import probench.clients.{ClientCLI, ProBenchClient}
import probench.data.{ClientNodeState, KVOperation, Request}
import rdts.base.Uid
import rdts.datatypes.experiments.protocols.Membership
import rdts.datatypes.experiments.protocols.simplified.Paxos

import java.net.{DatagramSocket, InetSocketAddress}
import java.util.Timer
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext

object cli {

  private val executor: ExecutorService = Executors.newCachedThreadPool()
  private val ec: ExecutionContext      = ExecutionContext.fromExecutor(executor)

  def main(args: Array[String]): Unit = {

    val clientPort = named[Int]("--listen-client-port", "")
    val peerPort   = named[Int]("--listen-peer-port", "")

    val ipAndPort = """(.+):(\d+)""".r

    given ipAndPortParser: ArgumentValueParser[(String, Int)] with
      override def apply(args: List[String]): (Option[(String, Int)], List[String]) =
        args match {
          case ipAndPort(ip, port) :: rest => (Some((ip, Integer.parseInt(port))), rest)
          case _                           => (None, args)
        }

      override def valueDescription: String = "<ip:port>"
    end ipAndPortParser

    given uidParser: ArgumentValueParser[Uid] with
      override def apply(args: List[String]): (Option[Uid], List[String]) =
        args match {
          case string :: rest => (Some(Uid.predefined(string)), rest)
          case _              => (None, args)
        }

      override def valueDescription: String = "<uid>"
    end uidParser

    given JsonValueCodec[ClientNodeState] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    given JsonValueCodec[Membership[Request, Paxos, Paxos]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    def socketPath(host: String, port: Int) = {
//      val p = Path.of(s"target/sockets/$name")
//      Files.createDirectories(p.getParent)
//      p.toFile.deleteOnExit()
//      UnixDomainSocketAddress.of(p)

      InetSocketAddress(host, port)

    }

    val argparse = argumentParser {
      inline def cluster           = named[List[(String, Int)]]("--cluster", "")
      inline def initialClusterIds = named[List[Uid]]("--initial-cluster-ids", "")
      inline def clientNode        = named[(String, Int)]("--node", "<ip:port>")
      inline def name              = named[Uid]("--name", "", Uid.gen())

      subcommand("node", "starts a cluster node") {
        val node = Node(name.value, initialClusterIds.value.toSet)

        node.addClientConnection(TCP.listen(TCP.defaultServerSocket(socketPath("localhost", clientPort.value)), ec))
        node.addClusterConnection(TCP.listen(TCP.defaultServerSocket(socketPath("localhost", peerPort.value)), ec))

        Timer().schedule(() => node.clusterDataManager.pingAll(), 1000, 1000)

        cluster.value.foreach { (ip, port) =>
          node.addClusterConnection(TCP.connect(TCP.defaultSocket(socketPath(ip, port)), ec))
        }
      }.value

      subcommand("nio-node", "starts a cluster node") {
        val node = Node(name.value, initialClusterIds.value.toSet)

        val nioTCP = NioTCP()
        ec.execute(() => nioTCP.loopSelection(Abort()))

        node.addClientConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
          "localhost",
          clientPort.value
        ))))
        node.addClusterConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
          "localhost",
          peerPort.value
        ))))

        Timer().schedule(() => node.clusterDataManager.pingAll(), 1000, 1000)

        cluster.value.foreach { (ip, port) =>
          node.addClusterConnection(nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(ip, port))))
        }
      }.value

      subcommand("udp-node", "starts a cluster node") {
        val node = Node(name.value, initialClusterIds.value.toSet)

        node.addClientConnection(UDP.listen(() => new DatagramSocket(clientPort.value), ec))
        node.addClusterConnection(UDP.listen(() => new DatagramSocket(peerPort.value), ec))

        Timer().schedule(() => node.clusterDataManager.pingAll(), 1000, 1000)

        cluster.value.foreach { (ip, port) =>
          node.addClusterConnection(UDP.connect(InetSocketAddress(ip, port), () => new DatagramSocket(), ec))
        }
      }.value

      subcommand("client", "starts a client to interact with a node") {
        val client = ProBenchClient(name.value)

        val (ip, port) = clientNode.value
        client.addLatentConnection(TCP.connect(TCP.defaultSocket(socketPath(ip, port)), ec))

        ClientCLI(name.value, client).startCLI()
      }.value

      subcommand("nio-client", "starts a client to interact with a node") {
        val client = ProBenchClient(name.value)

        val (ip, port) = clientNode.value

        val nioTCP = NioTCP()
        ec.execute(() => nioTCP.loopSelection(Abort()))

        client.addLatentConnection(nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(ip, port))))

        ClientCLI(name.value, client).startCLI()
      }.value

      subcommand("udp-client", "starts a client to interact with a node") {
        val client = ProBenchClient(name.value)

        val (ip, port) = clientNode.value

        client.addLatentConnection(UDP.connect(InetSocketAddress(ip, port), () => new DatagramSocket(), ec))

        ClientCLI(name.value, client).startCLI()
      }.value

      subcommand("benchmark", "") {}.value
    }

    argparse.parse(args.toList).printHelp()
  }

}
