package de.ckuessner
package encrdt.encrypted

import encrdt.lattices.CounterLattice

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.{Aead, KeyTemplates, KeysetHandle}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class EncryptedCrdtSpec extends AnyFlatSpec {
  AeadConfig.register()
  val keyset: KeysetHandle = KeysetHandle.generateNew(KeyTemplates.get("XCHACHA20_POLY1305"))
  val aead: Aead = keyset.getPrimitive(classOf[Aead])

  implicit val codec: JsonValueCodec[CounterLattice] = JsonCodecMaker.make

  "EncryptedCrdt" should "unseal for simple example with CounterCrdt" in {
    val counterCrdtState = CounterLattice(Map("123" -> 12, "42" -> 21), Map("4711" -> 1, "42" -> 12))

    val encCrdt = EncryptedCrdt.from(counterCrdtState, "encreplica", aead).get
    assertResult(counterCrdtState) {
      encCrdt.unseal.get
    }
  }

  it should "merge for CounterCrdt and unseal to correct Crdt" in {
    val encCrdt = new EncryptedCrdt[CounterLattice](aead, "encreplica")
    var crdtState = CounterLattice()

    val r = new Random()
    for (_ <- 1 to 10) {
      val replicaId = r.nextInt().abs.toString
      crdtState = crdtState.updated(replicaId, r.nextInt())
      encCrdt.update(crdtState)
      assertResult(crdtState) {
        encCrdt.unseal.get
      }
    }
  }
}
