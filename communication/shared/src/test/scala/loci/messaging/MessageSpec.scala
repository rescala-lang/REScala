package loci
package messaging

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{OptionValues, TryValues}

class MessageSpec extends AnyFlatSpec with Matchers with OptionValues with TryValues with NoLogging {
  behavior of "Message"

  implicit class cleanOp(string: String) {
    def clean = string.linesIterator map { _.stripMargin } mkString "\r\n"
  }

  case object TestMessage {
    implicit def method: Message.Method[TestMessage.type] =
      messaging.Message.Method(TestMessage -> "TestMessage")
  }

  it should "serialize messages correctly" in {
    Message.serialize(Message(TestMessage, Map("a" -> Seq("0"), "b" -> Seq("1")), MessageBuffer.empty)).decodeString should
      (be ("""TestMessage
             |a: 0
             |b: 1
             |""".clean) or
       be ("""TestMessage
             |b: 1
             |a: 0
             |""".clean))

    Message.serialize(Message(TestMessage, Map("a" -> Seq("0", "1")), MessageBuffer encodeString "dummy")).decodeString should
      (be ("""TestMessage
             |a: 0
             |a: 1
             |
             |dummy""".clean) or
       be ("""TestMessage
             |a: 1
             |a: 0
             |
             |dummy""".clean))

    Message.serialize(Message(TestMessage, Map.empty, MessageBuffer.empty)).decodeString should
      be ("""TestMessage
             |""".clean)

    Message.serialize(Message(TestMessage, Map.empty, MessageBuffer encodeString "dummy\r")).decodeString should
      be ("""TestMessage
             |
             |dummy""".clean + "\r")

    Message.serialize(Message(TestMessage, Map.empty, MessageBuffer encodeString "dummy\n")).decodeString should
      be ("""TestMessage
             |
             |dummy""".clean + "\n")
  }

  it should "deserialize messages correctly" in {
    val a = Message.deserialize[TestMessage.type](MessageBuffer encodeString "TestMessage").success.value

    a.method should be (TestMessage)
    a.payload should be (empty)
    a.properties.toSeq should have size 0

    val b = Message.deserialize[TestMessage.type](MessageBuffer encodeString
      """TestMessage
        | a: 0""".clean).success.value

    b.method should be (TestMessage)
    b.payload should be (empty)
    b.properties should be (Map("a" -> Seq("0")))

    val c = Message.deserialize[TestMessage.type](MessageBuffer encodeString
      """TestMessage
        | a: 0
        | b: 0
        |""".clean).success.value

    c.method should be (TestMessage)
    c.payload should be (empty)
    c.properties should be (Map("a" -> Seq("0"), "b" -> Seq("0")))

    val d = Message.deserialize[TestMessage.type] (MessageBuffer encodeString
      """TestMessage
        | a: 0
        | a: 1
        | b: 0
        |
        |""".clean).success.value

    d.method should be (TestMessage)
    d.payload should be (empty)
    d.properties should be (Map("a" -> Seq("0", "1"), "b" -> Seq("0")))

    val e = Message.deserialize[TestMessage.type] (MessageBuffer encodeString
      """TestMessage
        | a: 0
        | a: 1
        | b: 0
        | a: 2
        |
        |
        |""".clean).success.value

    e.method should be (TestMessage)
    e.payload.decodeString should be ("\r\n")
    e.properties should be (Map("a" -> Seq("0", "1", "2"), "b" -> Seq("0")))
  }

  it should "not deserialize messages incorrectly" in {
    Message.deserialize[TestMessage.type](MessageBuffer encodeString "Dummy").failure.exception should
      have message "Invalid message: invalid method"

    Message.deserialize[TestMessage.type](MessageBuffer encodeString
      """TestMessage
        | : 0
        | b: 0
        |""".clean).failure.exception should have message "Invalid message: empty key"

    Message.deserialize[TestMessage.type](MessageBuffer encodeString
      """TestMessage
        | a:
        | b: 0
        |""".clean).failure.exception should have message "Invalid message: empty value"

    Message.deserialize[TestMessage.type](MessageBuffer encodeString
      """TestMessage
        | a
        | b: 0
        |""".clean).failure.exception should have message "Invalid message: missing value"
  }
}
