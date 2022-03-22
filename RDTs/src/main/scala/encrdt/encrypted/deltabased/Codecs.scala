package de.ckuessner
package encrdt.encrypted.deltabased

import encrdt.causality.DotStore.DotSet
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.ckuessner.encrdt.causality.CausalContext

object Codecs {
  implicit val dotSetJsonCodec: JsonValueCodec[DotSet]        = JsonCodecMaker.make
  implicit val causalContextCodec: JsonValueCodec[CausalContext] = JsonCodecMaker.make
}
