package de.ckuessner
package encrdt.util

object CodecConfig {
  import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig

  val relaxedJsonCodecConfig: CodecMakerConfig =
    CodecMakerConfig.withSetMaxInsertNumber(Int.MaxValue).withMapMaxInsertNumber(Int.MaxValue)

}
