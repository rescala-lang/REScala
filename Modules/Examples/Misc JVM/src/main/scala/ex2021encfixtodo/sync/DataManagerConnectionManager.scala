package ex2021encfixtodo.sync

import channels.NioTCP
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.{Aead, CleartextKeysetHandle, JsonKeysetReader, JsonKeysetWriter, KeyTemplates, KeysetHandle, LegacyKeysetSerialization}
import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.dotted.{Dotted, HasDots, Obrem}
import replication.DeltaDissemination
import replication.JsoniterCodecs.given

import java.net.{InetSocketAddress, Socket, URI}
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

class DataManagerConnectionManager[State: {JsonValueCodec, Lattice, Bottom, HasDots}](
    replicaId: LocalUid,
    receiveCallback: Obrem[State] => Unit
) extends ConnectionManager[Obrem[State]] {

  given JsonValueCodec[Obrem[State]] = JsonCodecMaker.make

  AeadConfig.register()
  private val keysetFilePath: Path = Path.of("demokey.json")
  if !Files.exists(keysetFilePath) then {
    val keyset: KeysetHandle = KeysetHandle.generateNew(KeyTemplates.get("XCHACHA20_POLY1305"))
    CleartextKeysetHandle.write(keyset, JsonKeysetWriter.withOutputStream(Files.newOutputStream(keysetFilePath)))
  }

  private val keyset =
    CleartextKeysetHandle.read(JsonKeysetReader.withInputStream(Files.newInputStream(keysetFilePath)))
  private val aead: Aead = keyset.getPrimitive(classOf[Aead])

  println(LegacyKeysetSerialization.getKeysetInfo(keyset))

  val dataManager: DeltaDissemination[Obrem[State]] =
    DeltaDissemination[Obrem[State]](
      replicaId: LocalUid,
      receiveCallback,
      crypto = Some(AeadTranslation(aead))
    )

  val port = Random.nextInt(10000) + 50000

  val executor: ExecutorService = Executors.newCachedThreadPool((r: Runnable) =>
    Executors.defaultThreadFactory().newThread(r).tap(_.setDaemon(true))
  )
  val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  val niotcp = new NioTCP {}

  dataManager.addLatentConnection(niotcp.listen(
    niotcp.defaultServerSocketChannel(new InetSocketAddress("127.0.0.1", port))
  ))

  override val localReplicaId: String = replicaId.toString

  override def stateChanged(newState: Obrem[State]): Unit = {
    dataManager.applyDelta(newState)
  }

  override def connectToReplica(remoteReplicaId: String, uri: URI): Unit = {
    dataManager.addLatentConnection(niotcp.connect(niotcp.defaultSocketChannel(new InetSocketAddress(
      uri.getHost,
      uri.getPort
    ))))
  }

  override def stop(): Unit = {
    dataManager.globalAbort.closeRequest = true
    executor.shutdownNow()
    ()
  }

  override def uri: URI = URI.create(s"tcp://127.0.0.1:$port")

  override def remoteAddresses: Set[String] = Set.empty
}
