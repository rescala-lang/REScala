package loci
package communicator

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.Try

trait ConnectionSetupParser {
  val self: ConnectionSetupFactory.Implementation[_]

  type Properties

  protected abstract class PropertyParser[T](
    val parse: List[String] => Option[T])

  protected abstract class PropertyParserTry[T](parse: List[String] => T)
    extends PropertyParser[T](value => Try { parse(value) }.toOption)

  protected implicit class PropertiesParsingOp(properties: self.Properties) {
    def set[T: PropertyParser]
        (key: String)(transform: T => self.Properties => self.Properties)
        (implicit props: ConnectionSetupFactory.Properties) =
      (props get key
        flatMap { implicitly[PropertyParser[T]] parse _ }
        map { transform(_)(properties) }
        getOrElse properties)
  }
}

trait SimpleConnectionSetupProperties { this: ConnectionSetupParser =>
  protected implicit object booleanParser
    extends PropertyParserTry[Boolean](_.head.toBoolean)
  protected implicit object byteParser
    extends PropertyParserTry[Byte](_.head.toByte)
  protected implicit object shortParser
    extends PropertyParserTry[Short](_.head.toShort)
  protected implicit object intParser
    extends PropertyParserTry[Int](_.head.toInt)
  protected implicit object longParser
    extends PropertyParserTry[Long](_.head.toLong)
  protected implicit object floatParser
    extends PropertyParserTry[Float](_.head.toFloat)
  protected implicit object doubleParser
    extends PropertyParserTry[Double](_.head.toDouble)
  protected implicit object stringParser
    extends PropertyParserTry[String](_.head)
  protected implicit object durationParser
    extends PropertyParserTry[Duration](value => Duration(value.head))
  protected implicit object finiteDurationParser
    extends PropertyParserTry[FiniteDuration](value => Duration fromNanos Duration(value.head).toNanos)
  protected implicit object byteListParser
    extends PropertyParserTry[List[Byte]](_ map { _.toByte })
  protected implicit object shortListParser
    extends PropertyParserTry[List[Short]](_ map { _.toShort })
  protected implicit object intListParser
    extends PropertyParserTry[List[Int]](_ map { _.toInt })
  protected implicit object longListParser
    extends PropertyParserTry[List[Long]](_ map { _.toLong })
  protected implicit object floatListParser
    extends PropertyParserTry[List[Float]](_ map { _.toFloat })
  protected implicit object doubleListParser
    extends PropertyParserTry[List[Double]](_ map { _.toDouble })
  protected implicit object stringListParser
    extends PropertyParserTry[List[String]](identity)
  protected implicit object durationListParser
    extends PropertyParserTry[List[Duration]](_ map { Duration(_) })
  protected implicit object finiteDurationListParser
    extends PropertyParserTry[List[FiniteDuration]](_ map { Duration fromNanos Duration(_).toNanos })
}
