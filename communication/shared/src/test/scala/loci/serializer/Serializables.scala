package loci
package serializer

import transmitter.Parser._

import scala.util.Try

object Serializables {
  def intSerializer(value: Int) = string(value.toString)
  def intDeserializer(value: Deserializer) = value.asString.toInt

  def longSerializer(value: Long) = string(value.toString)
  def longDeserializer(value: Deserializer) = value.asString.toLong

  def booleanSerializer(value: Boolean) = string(value.toString)
  def booleanDeserializer(value: Deserializer) = value.asString.toBoolean

  def stringSerializer(value: String) = string(value)
  def stringDeserializer(value: Deserializer) = value.asString

  def unitSerializer(value: Unit) = string("")
  def unitDeserializer(value: Deserializer) = ()

  def optionSerializer(value: Option[Serializer]) = list(value.toList)
  def optionDeserializer(value: Deserializer) = value.asList.headOption

  def setSerializer(value: Set[Serializer]) = list(value.toList)
  def setDeserializer(value: Deserializer) = value.asList.toSet

  def eitherSerializer(value: Either[Serializer, Serializer]) =
    tag(if (value.isLeft) "left" else "right", value.merge)
  def eitherDeserializer(value: Deserializer) = {
    val (tag, content) = value.asTag("left", "right")
    if (tag == 0) Left(content) else Right(content)
  }

  def pairSerializer(value: (Serializer, Serializer)) =
    elements(value._1, value._2)
  def pairDeserializer(value: Deserializer) = {
    val Seq(element1, element2) = value.asElements(2): @unchecked
    (element1, element2)
  }

  implicit object serializableInt extends Serializable[Int] {
    override def serialize(value: Int) =
      MessageBuffer.encodeString(intSerializer(value).toString)
    override def deserialize(value: MessageBuffer) = Try {
      intDeserializer(parse(value.decodeString))
    }
  }

  implicit object serializableLong extends Serializable[Long] {
    override def serialize(value: Long) =
      MessageBuffer.encodeString(longSerializer(value).toString)
    override def deserialize(value: MessageBuffer) = Try {
      longDeserializer(parse(value.decodeString))
    }
  }

  implicit object serializableBoolean extends Serializable[Boolean] {
    override def serialize(value: Boolean) =
      MessageBuffer.encodeString(booleanSerializer(value).toString)
    override def deserialize(value: MessageBuffer) = Try {
      booleanDeserializer(parse(value.decodeString))
    }
  }

  implicit object serializableString extends Serializable[String] {
    override def serialize(value: String) =
      MessageBuffer.encodeString(stringSerializer(value).toString)
    override def deserialize(value: MessageBuffer) = Try {
      stringDeserializer(parse(value.decodeString))
    }
  }

  implicit object unitString extends Serializable[Unit] {
    override def serialize(value: Unit) =
      MessageBuffer.encodeString(unitSerializer(value).toString)
    override def deserialize(value: MessageBuffer) = Try {
      unitDeserializer(parse(value.decodeString))
    }
  }

  implicit object serializableOptionString extends Serializable[Option[String]] {
    override def serialize(value: Option[String]) =
      MessageBuffer.encodeString(optionSerializer(value map stringSerializer).toString)
    override def deserialize(value: MessageBuffer) = Try {
      optionDeserializer(parse(value.decodeString)) map stringDeserializer
    }
  }

  implicit object serializableSetString extends Serializable[Set[String]] {
    override def serialize(value: Set[String]) =
      MessageBuffer.encodeString(setSerializer(value map stringSerializer).toString)
    override def deserialize(value: MessageBuffer) = Try {
      setDeserializer(parse(value.decodeString)) map stringDeserializer
    }
  }

  implicit object serializableSetInt extends Serializable[Set[Int]] {
    override def serialize(value: Set[Int]) =
      MessageBuffer.encodeString(setSerializer(value map intSerializer).toString)
    override def deserialize(value: MessageBuffer) = Try {
      setDeserializer(parse(value.decodeString)) map intDeserializer
    }
  }

  implicit object serializableEitherIntString extends Serializable[Either[Int, String]] {
    override def serialize(value: Either[Int, String]) =
      MessageBuffer.encodeString(eitherSerializer(value.fold(
        value => Left(intSerializer(value)),
        value => Right(stringSerializer(value)))).toString)
    override def deserialize(value: MessageBuffer) = Try {
      eitherDeserializer(parse(value.decodeString)).fold(
        value => Left(intDeserializer(value)),
        value => Right(stringDeserializer(value)))
    }
  }

  implicit object serializableLongString extends Serializable[(Long, String)] {
    override def serialize(value: (Long, String)) =
      MessageBuffer.encodeString(pairSerializer((longSerializer(value._1), stringSerializer(value._2))).toString)
    override def deserialize(value: MessageBuffer) = Try {
      val (element1, element2) = pairDeserializer(parse(value.decodeString))
      (longDeserializer(element1), stringDeserializer(element2))
    }
  }

  implicit object serializableOptionIntOptionString extends Serializable[(Option[Int], Option[String])] {
    override def serialize(value: (Option[Int], Option[String])) =
      MessageBuffer.encodeString(pairSerializer(
        (optionSerializer(value._1 map intSerializer),
         optionSerializer(value._2 map stringSerializer))).toString)
    override def deserialize(value: MessageBuffer) = Try {
      val (element1, element2) = pairDeserializer(parse(value.decodeString))
      (optionDeserializer(element1) map intDeserializer,
       optionDeserializer(element2) map stringDeserializer)
    }
  }

  implicit object serializableOptionStringOptionString extends Serializable[(Option[String], Option[String])] {
    override def serialize(value: (Option[String], Option[String])) =
      MessageBuffer.encodeString(pairSerializer(
        (optionSerializer(value._1 map stringSerializer),
         optionSerializer(value._2 map stringSerializer))).toString)
    override def deserialize(value: MessageBuffer) = Try {
      val (element1, element2) = pairDeserializer(parse(value.decodeString))
      (optionDeserializer(element1) map stringDeserializer,
       optionDeserializer(element2) map stringDeserializer)
    }
  }

  implicit object serializableOptionIntStringOptionString extends Serializable[(Option[(Int, String)], Option[String])] {
    override def serialize(value: (Option[(Int, String)], Option[String])) =
      MessageBuffer.encodeString(pairSerializer(
        (optionSerializer(value._1 map { value => pairSerializer((intSerializer(value._1), stringSerializer(value._2))) }),
         optionSerializer(value._2 map stringSerializer))).toString)
    override def deserialize(value: MessageBuffer) = Try {
      val (element1, element2) = pairDeserializer(parse(value.decodeString))
      (optionDeserializer(element1) map { value =>
         val (element1, element2) = pairDeserializer(value)
         (intDeserializer(element1), stringDeserializer(element2))
       },
       optionDeserializer(element2) map stringDeserializer)
    }
  }
}
