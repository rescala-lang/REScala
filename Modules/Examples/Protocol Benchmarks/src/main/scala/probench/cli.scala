package probench

import channels.{Abort, NioTCP, UDP}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import de.rmgk.options.*
import de.rmgk.options.Result.{Err, Ok}
import probench.clients.{BenchmarkMode, ClientCLI, EtcdClient, ProBenchClient}
import probench.data.{ClientState, ClusterState, KVOperation, KVState, ConnInformation}
import rdts.base.Uid
import rdts.datatypes.experiments.protocols.MultiPaxos
import replication.{FileConnection, ProtocolMessage}

import java.net.{DatagramSocket, InetSocketAddress}
import java.nio.file.{Files, Path}
import java.util.Timer
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext

object Codecs {
  // codecs
  given JsonValueCodec[ClusterState] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given clusterCodec: JsonValueCodec[ProtocolMessage[ClusterState]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given JsonValueCodec[ClientState] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given clientCodec: JsonValueCodec[ProtocolMessage[ClientState]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given JsonValueCodec[ConnInformation] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

}

import probench.Codecs.given

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

    given benchmarkModeParser: ArgumentValueParser[BenchmarkMode] with
      override def parse(args: List[String]): Result[BenchmarkMode] =
        args match {
          case "read" :: rest  => Result.Ok(BenchmarkMode.Read, rest)
          case "write" :: rest => Result.Ok(BenchmarkMode.Write, rest)
          case "mixed" :: rest => Result.Ok(BenchmarkMode.Mixed, rest)
          case _               => Result.Err("not a valid benchmark mode", descriptor)
        }

      override def descriptor: Descriptor = Descriptor("mode", "read|write|mixed")

    end benchmarkModeParser

    def socketPath(host: String, port: Int) = {
//      val p = Path.of(s"target/sockets/$name")
//      Files.createDirectories(p.getParent)
//      p.toFile.deleteOnExit()
//      UnixDomainSocketAddress.of(p)

      InetSocketAddress(host, port)
    }

    val cluster           = named[List[(String, Int)]]("--cluster", "")
    val initialClusterIds = named[List[Uid]]("--initial-cluster-ids", "")
    val clientNode        = named[(String, Int)]("--node", "<ip:port>")
    val name              = named[Uid]("--name", "", Uid.gen())
    val endpoints         = named[List[String]]("--endpoints", "")

    val warmup      = named[Int]("--warmup", "warmup period for the benchmark in seconds")
    val measurement = named[Int]("--measurement", "measurement period for the benchmark in seconds")
    val times       = named[Int]("--times", "number of operations for the benchmark")
    val mode        = named[BenchmarkMode]("--mode", "mode for the benchmark")

    val argparse = composedParser {

      alternatives(
        subcommand("easy-setup", "for lazy tests") {
          val ids                            = Set("Node1", "Node2", "Node3").map(Uid.predefined)
          val nodes @ primary :: secondaries = ids.map { id => KeyValueReplica(id, ids) }.toList: @unchecked
          val connection                     = channels.SynchronousLocalConnection[ProtocolMessage[ClusterState]]()
          primary.cluster.dataManager.addLatentConnection(connection.server)
          secondaries.foreach { node =>
            node.cluster.dataManager.addLatentConnection(connection.client(node.uid.toString))
          }

          val persist = flag("--persistence", "enable persistence").value

          if persist then {
            val persistencePath = Path.of("target/clusterdb/")
            Files.createDirectories(persistencePath)

            nodes.foreach { node =>
              node.cluster.dataManager.addLatentConnection(
                FileConnection[ClusterState](persistencePath.resolve(node.uid.toString + ".jsonl"))
              )
            }
          }

          val clientConnection = channels.SynchronousLocalConnection[ProtocolMessage[ClientState]]()

          primary.client.dataManager.addLatentConnection(clientConnection.server)

          val clientUid = Uid.gen()
          val client    = ProBenchClient(clientUid)
          client.addLatentConnection(clientConnection.client(clientUid.toString))

          ClientCLI(clientUid, client).startCLI()

        },
        subcommand("node", "starts a cluster node") {
          val node = KeyValueReplica(name.value, initialClusterIds.value.toSet)

          val nioTCP = NioTCP()
          ec.execute(() => nioTCP.loopSelection(Abort()))

          node.client.dataManager.addLatentConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
            "0",
            clientPort.value
          ))))

          val peerPortVal = peerPort.value

          node.cluster.dataManager.addLatentConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
            "0",
            peerPortVal
          ))))
          node.connInf.dataManager.addLatentConnection(nioTCP.listen(nioTCP.defaultServerSocketChannel(socketPath(
            "0",
            peerPortVal + 1
          ))))

//          Timer().schedule(() => node.cluster.dataManager.pingAll(), 1000, 1000)
          Timer().schedule(() => {
            node.connInf.sendHeartbeat()
            node.connInf.checkLiveness()
          }, 1000, 1000)

          cluster.value.foreach { (host, port) =>
            println(s"Connecting to $host:${port + 1}")
            node.connInf.dataManager.addRetryingLatentConnection(
              () => nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(host, port + 1))),
              1000,
              10
            )
            println(s"Connecting to $host:$port")
            node.cluster.dataManager.addRetryingLatentConnection(
              () => nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(host, port))),
              1000,
              10
            )
            println(s"Connecting to $host:${port - 1}")
            node.client.dataManager.addRetryingLatentConnection(
              () => nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(host, port - 1))),
              1000,
              10
            )
          }
        },
        subcommand("udp-node", "starts a cluster node") {
          val node = KeyValueReplica(name.value, initialClusterIds.value.toSet)

          node.client.dataManager.addLatentConnection(UDP.listen(() => new DatagramSocket(clientPort.value), ec))
          node.cluster.dataManager.addLatentConnection(UDP.listen(() => new DatagramSocket(peerPort.value), ec))
          node.connInf.dataManager.addLatentConnection(UDP.listen(() => new DatagramSocket(peerPort.value + 1), ec))

          Timer().schedule(() => node.cluster.dataManager.pingAll(), 1000, 1000)

          cluster.value.foreach { (ip, port) =>
            node.cluster.dataManager.addLatentConnection(UDP.connect(
              InetSocketAddress(ip, port),
              () => new DatagramSocket(),
              ec
            ))
            node.connInf.dataManager.addLatentConnection(UDP.connect(
              InetSocketAddress(ip, port + 1),
              () => new DatagramSocket(),
              ec
            ))
          }
        },
        subcommand("client", "starts a client to interact with a node") {
          val client = ProBenchClient(name.value)

          val (ip, port) = clientNode.value

          val nioTCP = NioTCP()
          val abort  = Abort()
          ec.execute(() => nioTCP.loopSelection(abort))

          client.addRetryingLatentConnection(
            () => nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(ip, port))),
            1000,
            10
          )

          ClientCLI(name.value, client).startCLI()

          abort.closeRequest = true
          executor.shutdownNow()
        },
        subcommand("udp-client", "starts a client to interact with a node") {
          val client = ProBenchClient(name.value)

          val (ip, port) = clientNode.value

          client.addLatentConnection(UDP.connect(InetSocketAddress(ip, port), () => new DatagramSocket(), ec))

          ClientCLI(name.value, client).startCLI()
          executor.shutdownNow()
        },
        subcommand("benchmark-client", "starts a benchmark client") {
          val client = ProBenchClient(name.value)

          val (ip, port) = clientNode.value

          val nioTCP = NioTCP()
          val abort  = Abort()
          ec.execute(() => nioTCP.loopSelection(abort))

          client.addRetryingLatentConnection(
            () => nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(ip, port))),
            1000,
            10
          )

          client.benchmark(mode = mode.value, times = times.value)

          abort.closeRequest = true
          executor.shutdownNow()
        },
        subcommand("benchmark-client-timed", "starts a benchmark client") {
          val client = ProBenchClient(name.value)

          val (ip, port) = clientNode.value

          val nioTCP = NioTCP()
          val abort  = Abort()
          ec.execute(() => nioTCP.loopSelection(abort))

          client.addRetryingLatentConnection(
            () => nioTCP.connect(nioTCP.defaultSocketChannel(socketPath(ip, port))),
            1000,
            10
          )

          client.benchmarkTimed(warmup.value, measurement.value, mode.value)

          abort.closeRequest = true
          executor.shutdownNow()
        },
        subcommand("etcd-client-timed", "starts a benchmark client") {
          val client = EtcdClient(name.value, endpoints.value)

          client.benchmarkTimed(warmup.value, measurement.value, mode.value)

          executor.shutdownNow()
        },
        subcommand("etcd-client", "starts a client to interact with an etcd cluster") {
          val client = EtcdClient(name.value, endpoints.value)

          ClientCLI(name.value, client).startCLI()

          executor.shutdownNow()
        },
      )
    }

    parse(args.toList)(argparse)
    ()
  }

}
