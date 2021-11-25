package de.ckuessner
package encrdt.benchmarks

import encrdt.crdts.AddWinsLastWriterWinsMap

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

object Codecs {
  implicit val awlwwmapJsonCodec: JsonValueCodec[AddWinsLastWriterWinsMap.LatticeType[String, String]] = JsonCodecMaker.make[AddWinsLastWriterWinsMap.LatticeType[String, String]]
}
