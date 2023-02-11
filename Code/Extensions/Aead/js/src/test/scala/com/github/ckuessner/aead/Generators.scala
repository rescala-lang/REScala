package com.github.ckuessner.aead

import org.scalacheck.{Arbitrary, Gen}

import scala.scalajs.js.JSConverters.JSRichIterable
import scala.scalajs.js.typedarray.Uint8Array

object Generators {
  val emptyByteArray: Uint8Array = new Uint8Array(0)

  given byteArrayGen: Gen[Uint8Array] =
    Gen
      .listOf(Gen.chooseNum[Short](0, 255))
      .map(list => Uint8Array.from(list.toJSIterable))
}
