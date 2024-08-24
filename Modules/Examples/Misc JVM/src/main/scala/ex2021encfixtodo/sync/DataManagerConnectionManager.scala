package ex2021encfixtodo.sync

import channels.TCP
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.{Aead, CleartextKeysetHandle, JsonKeysetReader, JsonKeysetWriter, KeyTemplates, KeysetHandle}
import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.dotted.{Dotted, HasDots, Obrem}
import replication.{DataManager, ProtocolDots}

import java.net.{Socket, URI}
import java.nio.file.{Files, Path}
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Random, Try}

class AeadTranslation(aead: com.google.crypto.tink.Aead) extends replication.Aead {
  override def encrypt(data: Array[Byte], associated: Array[Byte]): Array[Byte] = aead.encrypt(data, associated)

  override def decrypt(data: Array[Byte], associated: Array[Byte]): Try[Array[Byte]] =
    Try(aead.decrypt(data, associated))
}

class DataManagerConnectionManager[State: JsonValueCodec: Lattice: Bottom: HasDots](
    replicaId: LocalUid,
    receiveCallback: Obrem[State] => Unit
) extends ConnectionManager[Obrem[State]] {

  AeadConfig.register()
  private val keysetFilePath: Path = Path.of("demokey.json")
  if !Files.exists(keysetFilePath) then {
    val keyset: KeysetHandle = KeysetHandle.generateNew(KeyTemplates.get("XCHACHA20_POLY1305"))
    CleartextKeysetHandle.write(keyset, JsonKeysetWriter.withOutputStream(Files.newOutputStream(keysetFilePath)))
  }

  private val keyset =
    CleartextKeysetHandle.read(JsonKeysetReader.withInputStream(Files.newInputStream(keysetFilePath)))
  private val aead: Aead = keyset.getPrimitive(classOf[Aead])

  println(keyset.getKeysetInfo)

  val dataManager: DataManager[State] =
    DataManager[State](
      replicaId: LocalUid,
      _ => (),
      pd => receiveCallback(Dotted(pd.data, pd.context).toObrem),
      crypto = Some(AeadTranslation(aead))
    )

  val port = Random.nextInt(10000) + 50000

  val executor: ExecutorService = Executors.newCachedThreadPool((r: Runnable) =>
    Executors.defaultThreadFactory().newThread(r).tap(_.setDaemon(true))
  )
  val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  dataManager.addLatentConnection(TCP.listen(TCP.defaultSocket("127.0.0.1", port), ec))

  override val localReplicaId: String = replicaId.toString

  override def stateChanged(newState: Obrem[State]): Unit = {
    dataManager.applyLocalDelta(ProtocolDots(newState.data, newState.context))
  }

  override def connectToReplica(remoteReplicaId: String, uri: URI): Unit = {
    dataManager.addLatentConnection(TCP.connect(() => new Socket(uri.getHost, uri.getPort), ec))
  }

  override def stop(): Unit = {
    dataManager.globalAbort.closeRequest = true
    executor.shutdownNow()
    ()
  }

  override def uri: URI = URI.create(s"tcp://127.0.0.1:$port")

  override def remoteAddresses: Set[String] = Set.empty
}
