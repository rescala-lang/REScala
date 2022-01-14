package de.ckuessner
package encrdt.benchmarks

import com.github.javafaker.Faker
import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.{Aead, KeyTemplates, KeysetHandle}
import org.conscrypt.Conscrypt

import java.security.Security
import java.util.{Random, UUID}

object Helper {

  def uuidKeyValuePairs(size: Int): Array[(String, String)] = {
    val faker = new Faker(new Random(42))
    val arr = new Array[(String, String)](size)
    for (i <- arr.indices) {
      arr(i) = UUID.randomUUID().toString -> UUID.randomUUID().toString
    }
    arr
  }

  def dummyKeyValuePairs(size: Int): Array[(String, String)] = {
    val faker = new Faker(new Random(42))
    val arr = new Array[(String, String)](size)
    for (i <- arr.indices) {
      arr(i) = faker.name().fullName() -> faker.address().fullAddress()
    }
    arr
  }

  def setupAead(keyTemplateString: String): Aead = {
    Conscrypt.checkAvailability()
    Security.addProvider(Conscrypt.newProvider)
    AeadConfig.register()
    val keyset: KeysetHandle = KeysetHandle.generateNew(KeyTemplates.get(keyTemplateString))
    keyset.getPrimitive(classOf[Aead])
  }
}
