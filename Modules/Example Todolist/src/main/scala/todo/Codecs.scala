package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}

object Codecs {

  implicit val taskRefCodec: JsonValueCodec[TaskRef] = JsonCodecMaker.make

  implicit val codecState: JsonValueCodec[List[TaskRef]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit val codecLww: JsonValueCodec[Option[TaskData]] = JsonCodecMaker.make

}
