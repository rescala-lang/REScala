package com.github.ckuessner.aead

import com.github.ckuessner.aead.AeadHelper
import munit.AnyFixture

import scala.concurrent.ExecutionContext
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.{Failure, Success}

class AeadHelperTest extends munit.FunSuite {
  given executionContext: ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  private var key: Uint8Array               = null
  private var otherKey: Uint8Array          = null
  private val testMessage                   = "Test Message"
  private val associatedData                = "Associated Message"
  private var expectedCiphertextLength: Int = -1

  def uint8ArrayEquals(a: Uint8Array, b: Uint8Array): Boolean =
    a.toArray[Short].sameElements(b.toArray[Short])

  override def munitFixtures: Seq[AnyFixture[?]] = List(
    new Fixture[Unit]("ready") {
      override def apply(): Unit = ()
      override def beforeAll(): Unit =
        AeadHelper.ready().andThen { _ =>
          key = AeadHelper.generateRawKey
          otherKey = AeadHelper.generateRawKey
          expectedCiphertextLength =
            testMessage.getBytes.length
            + sodium.crypto_aead_xchacha20poly1305_ietf_ABYTES.intValue()
            + sodium.crypto_aead_xchacha20poly1305_ietf_NPUBBYTES.intValue()
        }
    }
  )

  test("initialization should not fail") {
    AeadHelper.ready().map(_ => ())(executionContext)
  }

  test("generateKey should not fail") {
    AeadHelper
      .ready()
      .map(_ => {
        assert(key.length == sodium.crypto_aead_xchacha20poly1305_ietf_KEYBYTES)
      })
  }

  test("generateKey should generate different keys") {
    AeadHelper
      .ready()
      .map(_ => {
        assert(!uint8ArrayEquals(key, otherKey))
      })
  }

  test("encrypt should produce ciphertext of correct length for strings with associated data") {
    AeadHelper
      .ready()
      .map(_ => {
        AeadHelper.encrypt(testMessage, associatedData, key)
      })
      .map {
        case Success(cipherText) => assert(cipherText.length == expectedCiphertextLength)
        case Failure(exception)  => fail("fail", exception)
      }
  }

  test("encrypt should produce ciphertext of correct length for strings with empty associated data") {
    AeadHelper
      .ready()
      .map(_ => {
        AeadHelper.encrypt(testMessage, associatedData, key)
      })
      .map {
        case Success(cipherText) => assert(cipherText.length == expectedCiphertextLength)
        case Failure(exception)  => fail("fail", exception)
      }
  }

  test("encrypt should use different nonces each time") {
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
        case _                        => fail("fail")
      }
      .map { case (ctLeft, ctRight) =>
        assert(!uint8ArrayEquals(ctLeft, ctRight))
      }
  }

  test("decrypt should produce original string given same associated data and correct key") {
    AeadHelper
      .ready()
      .map(_ => {
        val encryptedMessage = AeadHelper.encrypt(testMessage, associatedData, key).get
        AeadHelper.decrypt(encryptedMessage, associatedData, key)
      })
      .map {
        case Success(decrypted) => assert(decrypted.equals(testMessage))
        case _                  => fail("fail")
      }
  }

  test("decrypt should fail with wrong key") {
    AeadHelper
      .ready()
      .map(_ => {
        val encryptedMessage = AeadHelper.encrypt(testMessage, associatedData, key)
        AeadHelper.decrypt(encryptedMessage.get, associatedData, otherKey)
      })
      .map {
        case Failure(_) => ()
        case Success(_) => fail("")
      }
  }

  test("decrypt should fail with wrong associated data") {
    AeadHelper
      .ready()
      .map(_ => {
        val encryptedMessage = AeadHelper.encrypt(testMessage, associatedData, key)
        AeadHelper.decrypt(encryptedMessage.get, "Not the associated data", otherKey)
      })
      .map {
        case Failure(_) =>
        case Success(_) => fail("")
      }
  }
}
