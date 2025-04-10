package lofi_acl.access

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.{Ed25519Util, PublicIdentity}
import lofi_acl.ardt.datatypes.LWW
import munit.FunSuite
import rdts.base.{Bottom, Uid}
import rdts.datatypes.LastWriterWins
import rdts.time.Dot

case class CompoundTest(a: LastWriterWins[Option[String]], b: String)

class KeyHierarchyTest extends FunSuite {
  given stringCodec: JsonValueCodec[String] = JsonCodecMaker.make
  given stringBottom: Bottom[String]        = Bottom.provide("")
  given DeltaSurgeon[String]                = DeltaSurgeon.ofTerminalValue

  private given lwwSurgeon: DeltaSurgeon[LastWriterWins[Option[String]]] = {
    import DeltaSurgeon.given
    LWW.deltaSurgeon[Option[String]]
  }

  private given DeltaSurgeon[CompoundTest] = {
    given compoundTestBottom: Bottom[CompoundTest] = Bottom.derived
    DeltaSurgeon.derived
  }

  private val replicaId        = PublicIdentity.fromPublicKey(Ed25519Util.generateNewKeyPair.getPublic)
  private val fullKeyHierarchy = FullKeyHierarchy(KeyDerivationKey())

  test("FullKeyHierarchy with LastWriterWins[String]") {
    Seq(
      LastWriterWins.empty[Option[String]],
      LastWriterWins.now(Some("Test")),
      LastWriterWins.now(None),
    ).foreach { delta =>
      testEncryptDecrypt(fullKeyHierarchy, delta)
    }
  }

  def testEncryptDecrypt(keyHierarchy: KeyHierarchy, delta: LastWriterWins[Option[String]]): Unit = {
    val dot       = Dot(Uid(replicaId.id), 42)
    val isolated  = lwwSurgeon.isolate(delta)
    val encrypted = keyHierarchy.encryptDelta(dot, isolated)
    val decrypted = keyHierarchy.decryptDelta(dot, encrypted)
    assertEquals(lwwSurgeon.recombine(decrypted), delta)
  }
}
