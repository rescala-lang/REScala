package com.github.ckuessner.aead

import org.scalacheck.{Arbitrary, Gen}

object Generators {
  val emptyByteArray: Array[Byte] = Array()

  given byteArrayGen: Gen[Array[Byte]] =
    Gen.containerOf(Arbitrary.arbByte.arbitrary)
}
