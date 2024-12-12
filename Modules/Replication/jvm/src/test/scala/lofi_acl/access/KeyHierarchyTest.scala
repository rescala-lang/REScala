package lofi_acl.access

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import lofi_acl.ardt.datatypes.LWW
import lofi_acl.crypto.{Ed25519Util, KeyDerivationKey, PublicIdentity}
import munit.FunSuite
import rdts.base.Bottom
import rdts.datatypes.LastWriterWins
import rdts.time.Dot

class KeyHierarchyTest extends FunSuite {
  private given lwwSurgeon: DeltaSurgeon[LastWriterWins[Option[String]]] = {
    import DeltaSurgeon.given
    given stringCodec: JsonValueCodec[String] = JsonCodecMaker.make
    given stringBottom: Bottom[String]        = Bottom.provide("")
    given DeltaSurgeon[String]                = DeltaSurgeon.ofTerminalValue
    LWW.deltaSurgeon[Option[String]]
  }

  private val replicaId = PublicIdentity.fromPublicKey(Ed25519Util.generateNewKeyPair.getPublic)

  test("FullKeyHierarchy with LastWriterWins[String]") {
    val key = FullKeyHierarchy(KeyDerivationKey())

    Seq(
      LastWriterWins.empty[Option[String]],
      LastWriterWins.now(Some("Test")),
      LastWriterWins.now(None),
    ).foreach { delta =>
      testEncryptDecrypt(key, delta)
    }
  }

  def testEncryptDecrypt(keyHierarchy: KeyHierarchy, delta: LastWriterWins[Option[String]]): Unit = {
    val dot       = Dot(replicaId.toUid, 42)
    val isolated  = lwwSurgeon.isolate(delta)
    val encrypted = keyHierarchy.encryptDelta(dot, isolated)
    val decrypted = keyHierarchy.decryptDelta(dot, encrypted)
    assertEquals(lwwSurgeon.recombine(decrypted), delta)
  }
}
