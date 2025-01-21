package probench

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import probench.clients.ProBenchClient
import probench.data.{ClientNodeState, ClusterData, KVOperation}
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.experiments.protocols.{MultiPaxos, Participants}
import replication.ProtocolMessage

class ClusterConsensus extends munit.FunSuite {
  test("simple consensus") {
    given JsonValueCodec[ClientNodeState] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    type ConsensusType = MultiPaxos[ClusterData]

    given paxosMembership: JsonValueCodec[ConsensusType] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    given JsonValueCodec[ProtocolMessage[ConsensusType]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    val ids                            = Set("Node1", "Node2", "Node3").map(Uid.predefined)
    given Participants(ids)
    val nodes @ primary :: secondaries = ids.map { id => KeyValueReplica(id, ids) }.toList: @unchecked
    val connection                     = channels.SynchronousLocalConnection[ProtocolMessage[ConsensusType]]()
    primary.addClusterConnection(connection.server)
    secondaries.foreach { node => node.addClusterConnection(connection.client(node.uid.toString)) }

    val clientConnection = channels.SynchronousLocalConnection[ProtocolMessage[ClientNodeState]]()

    primary.addClientConnection(clientConnection.server)

    val clientUid = Uid.gen()
    val client    = ProBenchClient(clientUid, blocking = true)
    client.addLatentConnection(clientConnection.client(clientUid.toString))

    client.read("test")

    assertEquals(nodes(0).currentState, nodes(1).currentState)
    assertEquals(nodes(1).currentState, nodes(2).currentState)
    assertEquals(nodes(2).currentState, nodes(0).currentState)

    def investigateUpkeep(state: ConsensusType)(using LocalUid) = {
      val delta  = state.upkeep
      val merged = (state `merge` delta)
      assert(state != merged)
      assert(delta `inflates` state, delta)
    }

    while {

      Thread.sleep(100)

      nodes.filter(_.needsUpkeep()).exists { n =>
        println(s"forcing upkeep on $n")
        investigateUpkeep(n.currentState)(using n.localUid)
        n.forceUpkeep()
        true
      }

    } do ()

    nodes.foreach(node => assert(!node.needsUpkeep(), node.uid))

    Thread.sleep(200)

    def noUpkeep(keyValueReplica: KeyValueReplica): Unit = {
      val current = keyValueReplica.currentState
      assertEquals(
        current `merge` current.upkeep(using keyValueReplica.localUid),
        current,
        s"${keyValueReplica.uid} upkeep"
      )
    }

    nodes.foreach(noUpkeep)

    assertEquals(nodes(0).currentState, nodes(1).currentState)
    assertEquals(nodes(1).currentState, nodes(2).currentState)
    assertEquals(nodes(2).currentState, nodes(0).currentState)

    println(s"================ at the end of the tests")

  }
}
