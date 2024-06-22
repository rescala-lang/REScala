package benchmarks.encrdt

import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.{Aead, KeyTemplates, KeysetHandle}
import org.conscrypt.Conscrypt

import java.security.Security

object Helper {

  def dummyKeyValuePairs(size: Int): Array[(String, String)] = {
    val arr = new Array[(String, String)](size)
    for i <- arr.indices do {
      arr(i) = scala.util.Random.nextString(10) -> scala.util.Random.nextString(10)
    }
    arr
  }

  def setupAead(keyTemplateString: String): Aead = {
    if Conscrypt.isAvailable then {
      Conscrypt.checkAvailability()
      Security.addProvider(Conscrypt.newProvider)
    } else
      System.err.println("Conscrypt could not be loaded, continuing anyway")
    AeadConfig.register()
    val keyset: KeysetHandle = KeysetHandle.generateNew(KeyTemplates.get(keyTemplateString))
    keyset.getPrimitive(classOf[Aead])
  }
}
