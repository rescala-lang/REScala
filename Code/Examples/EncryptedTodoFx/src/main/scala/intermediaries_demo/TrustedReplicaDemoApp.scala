package intermediaries_demo

import todolist.SyncedTodoListCrdt.{StateType, stateCodec}
import todolist.{ConnectionManagerFactory, TodoListApp}

import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink._
import encrdtlib.sync.client_server.TrustedReplicaWebSocketClient

import java.nio.file.{Files, Path, Paths}

object TrustedReplicaDemoApp extends TodoListApp {
  AeadConfig.register()
  private val keysetFilePath: Path = Paths.get(".", "demokey.json")
  if (!Files.exists(keysetFilePath)) {
    val keyset: KeysetHandle = KeysetHandle.generateNew(KeyTemplates.get("XCHACHA20_POLY1305"))
    CleartextKeysetHandle.write(keyset, JsonKeysetWriter.withFile(keysetFilePath.toFile))
  }

  private val keyset     = CleartextKeysetHandle.read(JsonKeysetReader.withFile(keysetFilePath.toFile))
  private val aead: Aead = keyset.getPrimitive(classOf[Aead])

  Console.println(keyset.getKeysetInfo)

  ConnectionManagerFactory.impl = (replicaId, query, received) =>
    new TrustedReplicaWebSocketClient[StateType](replicaId, aead) {
      override protected def merge(state: StateType): Unit = received(state)

      override protected def localState(): StateType = query()
    }
}
