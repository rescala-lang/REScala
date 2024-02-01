package loci
package messaging

import scala.util.Try

case class Message[M: Message.Method](
    method: M,
    properties: Map[String, Seq[String]],
    payload: MessageBuffer) {
  override def toString: String = {
    val builder = new StringBuilder

    builder ++= "["
    builder ++= implicitly[Message.Method[M]].apply(method)

    if (properties.nonEmpty) {
      builder ++= " {"
      val entries = properties map { case (key, values) => s"$key: ${values mkString ","}" }
      builder ++= entries mkString "; "
      builder ++= "}"
    }

    if (payload.nonEmpty) {
      builder ++= ": "
      builder ++= payload.toString
    }

    builder ++= "]"
    builder.toString
  }
}

object Message {
  trait Method[M] {
    def apply(method: M): String
    def apply(method: String): Option[M]
  }

  object Method {
    def apply[M](methods: (M, String)*) = new Method[M] {
      def apply(method: M) = methods collectFirst {
        case (`method`, value) => value
      } getOrElse {
        throw new IllegalArgumentException(
          s"No string representation for method $method")
      }

      def apply(method: String) = methods collectFirst {
        case (value, `method`) => value
      }
    }
  }

  class Exception(msg: String) extends IllegalArgumentException(msg)

  def serialize[M: Method](message: Message[M]): MessageBuffer = {
    val builder = new StringBuilder

    builder ++= implicitly[Method[M]].apply(message.method)
    builder ++= "\r\n"

    message.properties foreach { case (key, values) =>
      values foreach { value =>
        builder ++= s"$key: $value\r\n"
      }
    }

    if (message.payload.nonEmpty) {
      builder ++= "\r\n"
      MessageBuffer.encodeString(builder.toString).concat(message.payload)
    }
    else
      MessageBuffer.encodeString(builder.toString)
  }

  def deserialize[M: Method](buffer: MessageBuffer): Try[Message[M]] = Try {
    val (method, properties, payload) = parse(buffer)
    Message(
      implicitly[Method[M]].apply(method) match {
        case Some(method) => method
        case _ => throwInvalidMethod
      },
      properties,
      payload)
  }

  private def throwNoMethod =
    throw new Exception("Invalid message: no method")
  private def throwInvalidMethod =
    throw new Exception("Invalid message: invalid method")
  private def throwInvalidLineBreak =
    throw new Exception("Invalid message: invalid line break")
  private def throwEmptyKey =
    throw new Exception("Invalid message: empty key")
  private def throwEmptyValue =
    throw new Exception("Invalid message: empty value")
  private def throwMissingValue =
    throw new Exception("Invalid message: missing value")

  private def parse(buffer: MessageBuffer) = {
    val stateHeader = 0
    val stateBeforeKey = 1
    val stateKey = 2
    val stateValue = 3
    val stateBeforePayload = 4
    val statePayload = 5

    var state = stateHeader
    var offset = 0
    var mark = 0
    var method = ""
    var key = ""
    var payload = MessageBuffer.empty
    var properties = Map.empty[String, Seq[String]]

    def parseString = buffer.decodeString(mark, offset - mark - 1)

    def makeMethod() = {
      method = parseString.trim
      if (method.isEmpty)
        throwNoMethod
    }

    def makeKey() = {
      key = parseString.trim
      if (key.isEmpty)
        throwEmptyKey
    }

    def makeProperty() = {
      val value = parseString.trim
      properties += key -> (properties.getOrElse(key, Seq.empty) :+ value)
      if (value.isEmpty)
        throwEmptyValue
    }

    def makePayload() =
      payload = buffer.copy(mark, buffer.length - mark)

    while (offset < buffer.size && state != statePayload) {
      val ch = buffer(offset)
      offset += 1

      state match {
        case `stateHeader` =>
          ch match {
            case '\r' =>
              makeMethod()
              state = stateBeforeKey
            case _  =>
          }

        case `stateBeforeKey` =>
          ch match {
            case '\n' => state = stateKey; mark = offset
            case _  => throwInvalidLineBreak
          }

        case `stateKey` =>
          ch match {
            case '\r' =>
              if (parseString.trim.nonEmpty)
                throwMissingValue
              state = stateBeforePayload
            case ':' =>
              makeKey()
              state = stateValue; mark = offset
            case _  =>
          }

        case `stateValue` =>
          ch match {
            case '\r' =>
              makeProperty()
              state = stateBeforeKey
            case _  =>
          }

        case `stateBeforePayload` =>
          ch match {
            case '\n' => state = statePayload; mark = offset
            case _  => throwInvalidLineBreak
          }

        case _ =>
          assert(assertion = false, "invalid state")
      }
    }

    offset += 1

    state match {
      case `stateHeader` => makeMethod()
      case `stateKey` => if (parseString.trim.nonEmpty) throwMissingValue
      case `stateValue` => makeProperty()
      case `statePayload` => makePayload()
      case `stateBeforeKey` | `stateBeforePayload` =>
      case _ => assert(assertion = false, "invalid state")
    }

    (method, properties, payload)
  }
}
