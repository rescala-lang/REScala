package dtn

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import java.nio.charset.StandardCharsets
import java.util.Base64

case class SendBundle(src: String, dst: String, data: String, delivery_notification: Boolean, lifetime: Long)
case class ReceivedBundle(bid: String, src: String, dst: String, data: String)
// automatic JSON encoder/decoder for case classes
given JsonValueCodec[ReceivedBundle] = JsonCodecMaker.make
given JsonValueCodec[SendBundle] = JsonCodecMaker.make

case class Bundle(bid: String = "", src: String = "", dst: String = "", data: String = "", delivery_notification: Boolean = false, lifetime: Long = 3600 * 24 * 1000) {
  def toDtnWsJson: Array[Byte] = {
    val bundle: SendBundle = SendBundle(src = src, dst = dst, data = data, delivery_notification = delivery_notification, lifetime = lifetime)
    writeToArray(bundle)
  }

  override def toString: String = s"Bundle(bid: $bid, src: $src, dst: $dst, data: $data, delivery_notification: $delivery_notification, lifetime: $lifetime)"

  def getDataAsUTF8Text: String = String(Base64.getDecoder.decode(data), StandardCharsets.UTF_8)

  def getDataAsBytes: Array[Byte] = Base64.getDecoder.decode(data)
}
object Bundle {
  def createWithUTF8TextAsData(src: String, dst: String, strng: String, delivery_notification: Boolean = false, lifetime: Long = 3600 * 24 * 1000): Bundle = {
    val data = Base64.getEncoder.encodeToString(strng.getBytes(StandardCharsets.UTF_8))
    Bundle(src = src, dst = dst, data = data, delivery_notification = delivery_notification, lifetime = lifetime)
  }

  def createWithBytesAsData(src: String, dst: String, bytes: Array[Byte], delivery_notification: Boolean = false, lifetime: Long = 3600 * 24 * 1000): Bundle = {
    val data = Base64.getEncoder.encodeToString(bytes)
    Bundle(src = src, dst = dst, data = data, delivery_notification = delivery_notification, lifetime = lifetime)
  }

  def createFromDtnWsJson(input: Array[Byte]): Bundle = {
    val bundle: ReceivedBundle = readFromArray[ReceivedBundle](input)
    Bundle(bid = bundle.bid, src = bundle.src, dst = bundle.dst, data = bundle.data)
  }
}
