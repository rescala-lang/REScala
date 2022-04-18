package benchmarks.encrdt

import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.{Aead, KeyTemplates, KeysetHandle}
import org.conscrypt.Conscrypt

import java.security.Security
import java.util.UUID

object Helper {
  def uuidKeyValuePairs(size: Int): Array[(String, String)] = {
    val arr = new Array[(String, String)](size)
    for (i <- arr.indices) {
      arr(i) = UUID.randomUUID().toString -> UUID.randomUUID().toString
    }
    arr
  }

  def dummyKeyValuePairs(size: Int): Array[(String, String)] = {
    val arr = new Array[(String, String)](size)
    for (i <- arr.indices) {
      arr(i) = scala.util.Random.nextString(10) -> scala.util.Random.nextString(10)
    }
    arr
  }

  def setupAead(keyTemplateString: String): Aead = {
    if (Conscrypt.isAvailable) {
      Conscrypt.checkAvailability()
      Security.addProvider(Conscrypt.newProvider)
    } else
      System.err.println("Conscrypt could not be loaded, continuing anyway")
    AeadConfig.register()
    val keyset: KeysetHandle = KeysetHandle.generateNew(KeyTemplates.get(keyTemplateString))
    keyset.getPrimitive(classOf[Aead])
  }
}
