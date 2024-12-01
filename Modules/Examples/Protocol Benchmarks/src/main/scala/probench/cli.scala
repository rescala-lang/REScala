package probench

import channels.{Abort, NioTCP, TCP, UDP}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import de.rmgk.options.*
import de.rmgk.options.Result.{Err, Ok}
import probench.clients.{ClientCLI, EtcdClient, ProBenchClient}
import probench.data.{ClientNodeState, KVOperation, Request}
import rdts.base.Uid
import rdts.datatypes.experiments.protocols.Membership
import rdts.datatypes.experiments.protocols.simplified.{GeneralizedPaxos, Paxos}

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
      override def parse(args: List[String]): Result[(String, Int)] =
        args match {
          case ipAndPort(ip, port) :: rest => Ok((ip, Integer.parseInt(port)), rest)
          case _                           => Err("not a valid ip:port")
        }

      def descriptor: de.rmgk.options.Descriptor = Descriptor("ip:port", "ip:port pair")
    end ipAndPortParser

    given uidParser: ArgumentValueParser[Uid] with
      override def parse(args: List[String]): Result[Uid] =
        args match {
          case string :: rest => Result.Ok(Uid.predefined(string), rest)
          case _              => Result.Err("not a valid uid", descriptor)
        }

      override def descriptor: Descriptor = Descriptor("uid", "uid")

    end uidParser

    given JsonValueCodec[ClientNodeState] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    given paxosMembership: JsonValueCodec[Membership[Request, Paxos, Paxos]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    given genPaxosMembership: JsonValueCodec[Membership[Request, GeneralizedPaxos, GeneralizedPaxos]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    def socketPath(host: String, port: Int) = {
//      val p = Path.of(s"target/sockets/$name")
//      Files.createDirectories(p.getParent)
//      p.toFile.deleteOnExit()
//      UnixDomainSocketAddress.of(p)

      InetSocketAddress(host, port)

    }

    val argparse = composedParser {
      inline def cluster           = named[List[(String, Int)]]("--cluster", "")
      inline def initialClusterIds = named[List[Uid]]("--initial-cluster-ids", "")
      inline def clientNode        = named[(String, Int)]("--node", "<ip:port>")
      inline def name              = named[Uid]("--name", "", Uid.gen())
      inline def endpoints         = named[List[String]]("--endpoints", "")

      alternatives(
        subcommand("node", "starts a cluster node") {
          val node = Node(name.value, initialClusterIds.value.toSet)

          node.addClientConnection(TCP.listen(TCP.defaultServerSocket(socketPath("localhost", clientPort.value)), ec))
          node.addClusterConnection(TCP.listen(TCP.defaultServerSocket(socketPath("localhost", peerPort.value)), ec))

          Timer().schedule(() => node.clusterDataManager.pingAll(), 1000, 1000)

          cluster.value.foreach { (ip, port) =>
            node.addClusterConnection(TCP.connect(TCP.defaultSocket(socketPath(ip, port)), ec))
          }
        },
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
        },
        subcommand("udp-node", "starts a cluster node") {
          val node = Node(name.value, initialClusterIds.value.toSet)

          node.addClientConnection(UDP.listen(() => new DatagramSocket(clientPort.value), ec))
          node.addClusterConnection(UDP.listen(() => new DatagramSocket(peerPort.value), ec))

          Timer().schedule(() => node.clusterDataManager.pingAll(), 1000, 1000)

          cluster.value.foreach { (ip, port) =>
            node.addClusterConnection(UDP.connect(InetSocketAddress(ip, port), () => new DatagramSocket(), ec))
          }
        },
        subcommand("client", "starts a client to interact with a node") {
          val client = ProBenchClient(name.value)

          val (ip, port) = clientNode.value
          client.addLatentConnection(TCP.connect(TCP.defaultSocket(socketPath(ip, port)), ec))

          ClientCLI(name.value, client).startCLI()
        },
        subcommand("nio-client", "starts a client to interact with a node") {
          val client = ProBenchClient(name.value)

          val (ip, port) = clientNode.value

          val nioTCP = NioTCP()
          ec.execute(() => nioTCP.loopSelection(Abort()))

          client.addLatentConnection(nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(ip, port))))

          ClientCLI(name.value, client).startCLI()
        },
        subcommand("udp-client", "starts a client to interact with a node") {
          val client = ProBenchClient(name.value)

          val (ip, port) = clientNode.value

          client.addLatentConnection(UDP.connect(InetSocketAddress(ip, port), () => new DatagramSocket(), ec))

          ClientCLI(name.value, client).startCLI()
        },
        subcommand("etcd-client", "starts a client to interact with an etcd cluster") {
          val client = EtcdClient(name.value, endpoints.value)

          ClientCLI(name.value, client).startCLI()
        },
      )
    }

    parse(args.toList)(argparse)
    ()
  }

}
