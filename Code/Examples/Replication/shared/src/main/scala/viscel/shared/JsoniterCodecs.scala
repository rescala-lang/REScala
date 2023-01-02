package viscel.shared

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

import scala.annotation.nowarn

object JsoniterCodecs {
  def writeString[T: JsonValueCodec](value: T) = writeToString(value)
  def writeArray[T: JsonValueCodec](value: T)  = writeToArray(value)

  def readString[T: JsonValueCodec](str: String) = readFromString[T](str)

  implicit val StringRw: JsonValueCodec[String] = JsonCodecMaker.make

  implicit def OptionCodec[T: JsonValueCodec]: JsonValueCodec[Option[T]] = JsonCodecMaker.make

}
