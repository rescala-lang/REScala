package probench

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import probench.clients.ProBenchClient
import probench.data.{ClientState, ClusterState, KVOperation}
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.experiments.protocols.{MultiPaxos, Participants}
import replication.ProtocolMessage

class ClusterConsensus extends munit.FunSuite {
  test("simple consensus") {

    given JsonValueCodec[ClusterState] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    given clusterCodec: JsonValueCodec[ProtocolMessage[ClusterState]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    given JsonValueCodec[ClientState] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    given clientCodec: JsonValueCodec[ProtocolMessage[ClientState]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    val ids = Set("Node1", "Node2", "Node3").map(Uid.predefined)
    given Participants(ids)
    val nodes @ primary :: secondaries = ids.map { id => KeyValueReplica(id, ids) }.toList: @unchecked
    val connection                     = channels.SynchronousLocalConnection[ProtocolMessage[ClusterState]]()
    primary.clusterDataManager.addLatentConnection(connection.server)
    secondaries.foreach { node => node.clusterDataManager.addLatentConnection(connection.client(node.uid.toString)) }

    val clientConnection = channels.SynchronousLocalConnection[ProtocolMessage[ClientState]]()

    primary.clientDataManager.addLatentConnection(clientConnection.server)

    val clientUid = Uid.gen()
    val client    = ProBenchClient(clientUid, blocking = true)
    client.addLatentConnection(clientConnection.client(clientUid.toString))

    client.read("test")

    assertEquals(nodes(0).clusterState, nodes(1).clusterState)
    assertEquals(nodes(1).clusterState, nodes(2).clusterState)
    assertEquals(nodes(2).clusterState, nodes(0).clusterState)

    def investigateUpkeep(state: ClusterState)(using LocalUid) = {
      val delta  = state.upkeep
      val merged = (state `merge` delta)
      assert(state != merged)
      assert(delta `inflates` state, delta)
    }

    while {

      Thread.sleep(100)

      nodes.filter(_.needsUpkeep()).exists { n =>
        println(s"forcing upkeep on $n")
        investigateUpkeep(n.clusterState)(using n.localUid)
        n.forceUpkeep()
        true
      }

    } do ()

    nodes.foreach(node => assert(!node.needsUpkeep(), node.uid))

    Thread.sleep(2000)

    def noUpkeep(keyValueReplica: KeyValueReplica): Unit = {
      val current = keyValueReplica.clusterState
      assertEquals(
        current `merge` current.upkeep(using keyValueReplica.localUid),
        current,
        s"${keyValueReplica.uid} upkeep"
      )
    }

    nodes.foreach(noUpkeep)

    assertEquals(nodes(0).clusterState, nodes(1).clusterState)
    assertEquals(nodes(1).clusterState, nodes(2).clusterState)
    assertEquals(nodes(2).clusterState, nodes(0).clusterState)

    println(s"================ at the end of the tests")

  }
}
