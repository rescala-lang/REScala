package probench

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import de.rmgk.options.*
import probench.clients.{ClientCLI, ProBenchClient}
import probench.data.{ClientNodeState, ClusterData, KVOperation}
import rdts.base.Uid
import rdts.datatypes.experiments.protocols.Membership
import rdts.datatypes.experiments.protocols.simplified.Paxos
import replication.ProtocolMessage

class ClusterConsensus extends munit.FunSuite {
  test("simple consensus") {
    given JsonValueCodec[ClientNodeState] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    type MembershipType = Membership[ClusterData, Paxos, Paxos]

    given paxosMembership: JsonValueCodec[MembershipType] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    given JsonValueCodec[ProtocolMessage[MembershipType]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    val ids                            = Set("Node1", "Node2", "Node3").map(Uid.predefined)
    val nodes @ primary :: secondaries = ids.map { id => KeyValueReplica(id, ids) }.toList: @unchecked
    val connection                     = channels.SynchronousLocalConnection[ProtocolMessage[MembershipType]]()
    primary.addClusterConnection(connection.server)
    secondaries.foreach { node => node.addClusterConnection(connection.client(node.uid.toString)) }

    val clientConnection = channels.SynchronousLocalConnection[ProtocolMessage[ClientNodeState]]()

    primary.addClientConnection(clientConnection.server)

    val clientUid = Uid.gen()
    val client    = ProBenchClient(clientUid, blocking = false)
    client.addLatentConnection(clientConnection.client(clientUid.toString))

    client.read("test")
    Thread.sleep(200)

    assertEquals(nodes(0).currentState, nodes(1).currentState)
    assertEquals(nodes(1).currentState, nodes(2).currentState)
    assertEquals(nodes(2).currentState, nodes(0).currentState)

    def noUpkeep(keyValueReplica: KeyValueReplica): Unit = {
      val current = keyValueReplica.currentState
      assertEquals(
        current `merge` current.upkeep()(using keyValueReplica.localUid),
        current,
        s"${keyValueReplica.uid} upkeep"
      )
    }

    nodes.foreach(noUpkeep)

  }
}
