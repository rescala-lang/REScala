package intermediaries_demo

import com.google.crypto.tink.*
import com.google.crypto.tink.aead.AeadConfig
import encrdtlib.sync.client_server.TrustedReplicaWebSocketClient
import todolist.SyncedTodoListCrdt.{StateType, stateCodec}
import todolist.{ConnectionManagerFactory, TodoListApp}

import java.nio.file.{Files, Path, Paths}
import scala.annotation.nowarn

object TrustedReplicaDemoApp extends TodoListApp {
  AeadConfig.register()
  private val keysetFilePath: Path = Paths.get(".", "demokey.json")
  if (!Files.exists(keysetFilePath)) {
    val keyset: KeysetHandle = KeysetHandle.generateNew(KeyTemplates.get("XCHACHA20_POLY1305"))
    CleartextKeysetHandle.write(keyset, JsonKeysetWriter.withFile(keysetFilePath.toFile): @nowarn)
  }

  private val keyset     = CleartextKeysetHandle.read(JsonKeysetReader.withFile(keysetFilePath.toFile): @nowarn)
  private val aead: Aead = keyset.getPrimitive(classOf[Aead])

  Console.println(keyset.getKeysetInfo)

  ConnectionManagerFactory.impl = (replicaId, query, received) =>
    new TrustedReplicaWebSocketClient[StateType](replicaId, aead) {
      override protected def merge(state: StateType): Unit = received(state)

      override protected def localState(): StateType = query()
    }
}
