package com.github.ckuessner.aead

import com.github.ckuessner.aead.AeadHelper
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AsyncFlatSpec
import typings.libsodiumWrappers.mod as sodium

import scala.concurrent.ExecutionContext
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.{Failure, Success}

class AeadHelperTest extends AsyncFlatSpec with BeforeAndAfter {
  implicit override def executionContext: ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  private var key: Uint8Array               = null
  private var otherKey: Uint8Array          = null
  private val testMessage                   = "Test Message"
  private val associatedData                = "Associated Message"
  private var expectedCiphertextLength: Int = -1

  def uint8ArrayEquals(a: Uint8Array, b: Uint8Array): Boolean =
    a.toArray[Short].sameElements(b.toArray[Short])

  before {
    AeadHelper.ready().andThen { _ =>
      key = AeadHelper.generateRawKey
      otherKey = AeadHelper.generateRawKey
      expectedCiphertextLength = testMessage.getBytes.length
        + sodium.cryptoAeadXchacha20poly1305IetfABYTES.intValue()
        + sodium.cryptoAeadXchacha20poly1305IetfNPUBBYTES.intValue()
    }
  }

  "initialization" should "not fail" in {
    AeadHelper.ready().map(_ => succeed)(executionContext)
  }

  behavior of "generateKey"

  it should "not fail" in {
    AeadHelper
      .ready()
      .map(_ => {
        assert(key.length == sodium.cryptoAeadXchacha20poly1305IetfKEYBYTES)
      })
  }

  it should "generate different keys" in {
    AeadHelper
      .ready()
      .map(_ => {
        assert(!uint8ArrayEquals(key, otherKey))
      })
  }

  behavior of "encrypt"

  it should "produce ciphertext of correct length for strings with associated data" in {
    AeadHelper
      .ready()
      .map(_ => {
        AeadHelper.encrypt(testMessage, associatedData, key)
      })
      .map {
        case Success(cipherText) => assert(cipherText.length == expectedCiphertextLength)
        case Failure(exception)  => fail(exception)
      }
  }

  it should "produce ciphertext of correct length for strings with empty associated data" in {
    AeadHelper
      .ready()
      .map(_ => {
        AeadHelper.encrypt(testMessage, associatedData, key)
      })
      .map {
        case Success(cipherText) => assert(cipherText.length == expectedCiphertextLength)
        case Failure(exception)  => fail(exception)
      }
  }

  it should "use different nonces each time" in {
    AeadHelper
      .ready()
      .map(_ => {
        (
          AeadHelper.encrypt(testMessage, associatedData, key),
          AeadHelper.encrypt(testMessage, associatedData, key)
        )
      })
      .map {
        case (Success(l), Success(r)) => (l, r)
        case _                        => fail()
      }
      .map { case (ctLeft, ctRight) =>
        assert(!uint8ArrayEquals(ctLeft, ctRight))
      }
  }

  behavior of "decrypt"

  it should "produce original string given same associated data and correct key" in {
    AeadHelper
      .ready()
      .map(_ => {
        val encryptedMessage = AeadHelper.encrypt(testMessage, associatedData, key).get
        AeadHelper.decrypt(encryptedMessage, associatedData, key)
      })
      .map {
        case Success(decrypted) => assert(decrypted.equals(testMessage))
        case _                  => fail()
      }
  }

  it should "fail with wrong key" in {
    AeadHelper
      .ready()
      .map(_ => {
        val encryptedMessage = AeadHelper.encrypt(testMessage, associatedData, key)
        AeadHelper.decrypt(encryptedMessage.get, associatedData, otherKey)
      })
      .map {
        case Failure(_) => succeed
        case Success(_) => fail()
      }
  }

  it should "fail with wrong associated data" in {
    AeadHelper
      .ready()
      .map(_ => {
        val encryptedMessage = AeadHelper.encrypt(testMessage, associatedData, key)
        AeadHelper.decrypt(encryptedMessage.get, "Not the associated data", otherKey)
      })
      .map {
        case Failure(_) => succeed
        case Success(_) => fail()
      }
  }
}
