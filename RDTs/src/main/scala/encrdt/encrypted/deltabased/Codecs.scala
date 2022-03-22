package de.ckuessner
package encrdt.encrypted.deltabased

import encrdt.causality.DotStore.DotSet

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

object Codecs {
  implicit val dotSetJsonCodec: JsonValueCodec[DotSet] = JsonCodecMaker.make
}
