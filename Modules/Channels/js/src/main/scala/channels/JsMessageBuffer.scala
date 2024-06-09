package channels

import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array, given}

class JsArrayBufferMessageBuffer(val inner: ArrayBuffer) extends MessageBuffer {
  override def asArray: Array[Byte] = new Int8Array(inner).toArray
}

object MesageBufferExtensions {
  extension (mb: MessageBuffer)
    def asArrayBuffer: ArrayBuffer =
      mb match
        case buf: JsArrayBufferMessageBuffer => buf.inner
        case other                           => other.asArray.toTypedArray.buffer
}
