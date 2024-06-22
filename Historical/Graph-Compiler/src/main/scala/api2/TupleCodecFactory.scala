package api2

import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.github.plokhotnyuk.jsoniter_scala.core.*

import scala.quoted.*

type Codecs[T <: Tuple] <: Tuple = T match
  case EmptyTuple => EmptyTuple
  case t *: ts    => JsonValueCodec[t] *: Codecs[ts]

trait TupleCodec[T <: Tuple] {
  def decodeValue(
      codecs: Codecs[OptionsFromTuple[T]],
      in: JsonReader,
      default: OptionsFromTuple[T],
      checkComma: Boolean
  ): OptionsFromTuple[T]

  def encodeValue(codecs: Codecs[OptionsFromTuple[T]], x: OptionsFromTuple[T], out: JsonWriter): Unit

  def nullValue(codecs: Codecs[OptionsFromTuple[T]]): OptionsFromTuple[T]
}

given TupleCodec[EmptyTuple] with {
  def decodeValue(
      codecs: Codecs[OptionsFromTuple[EmptyTuple]],
      in: JsonReader,
      default: OptionsFromTuple[EmptyTuple],
      checkComma: Boolean
  ): OptionsFromTuple[EmptyTuple] = EmptyTuple

  def encodeValue(
      codecs: Codecs[OptionsFromTuple[EmptyTuple]],
      x: OptionsFromTuple[EmptyTuple],
      out: JsonWriter
  ): Unit = ()

  def nullValue(codecs: Codecs[OptionsFromTuple[EmptyTuple]]): OptionsFromTuple[EmptyTuple] = EmptyTuple
}

given [T, TS <: Tuple: TupleCodec]: TupleCodec[T *: TS] with {
  def decodeValue(
      codecs: Codecs[OptionsFromTuple[T *: TS]],
      in: JsonReader,
      default: OptionsFromTuple[T *: TS],
      checkComma: Boolean
  ): OptionsFromTuple[T *: TS] = {
    val codecsHead *: codecsTail   = codecs
    val defaultHead *: defaultTail = default

    val x = if !checkComma || in.isNextToken(44) then codecsHead.decodeValue(in, defaultHead) else in.commaError()

    x *: summon[TupleCodec[TS]].decodeValue(codecsTail, in, defaultTail, true)
  }

  def encodeValue(
      codecs: Codecs[OptionsFromTuple[T *: TS]],
      x: OptionsFromTuple[T *: TS],
      out: JsonWriter
  ): Unit = {
    val codecsHead *: codecsTail = codecs
    val xHead *: xTail           = x

    codecsHead.encodeValue(xHead, out)

    summon[TupleCodec[TS]].encodeValue(codecsTail, xTail, out)
  }

  def nullValue(
      codecs: Codecs[OptionsFromTuple[T *: TS]]
  ): OptionsFromTuple[T *: TS] = {
    val codecsHead *: codecsTail = codecs

    codecsHead.nullValue *: summon[TupleCodec[TS]].nullValue(codecsTail)
  }
}

object TupleCodecFactory {
  inline def generateEventCodecsTuple[T <: Tuple]: Codecs[OptionsFromTuple[TupleFromEvents[T]]] =
    ${ generateEventCodecsTupleCode }

  def generateEventCodecsTupleCode[T <: Tuple](using
      Type[T],
      Quotes
  ): Expr[Codecs[OptionsFromTuple[TupleFromEvents[T]]]] = {
    import quotes.reflect.*

    val AppliedType(_, evTypes) = TypeRepr.of[T]: @unchecked

    val codecs = evTypes.collect {
      case AppliedType(_, List(inner)) =>
        inner.asType match
          case '[t] => '{ JsonCodecMaker.make[Option[t]] }
    }

    Expr.ofTupleFromSeq(codecs).asInstanceOf[Expr[Codecs[OptionsFromTuple[TupleFromEvents[T]]]]]
  }

  inline def generateCEventCodecsTuple[T <: Tuple]: Codecs[OptionsFromTuple[TupleFromCEvents[T]]] =
    ${ generateCEventCodecsTupleCode }

  def generateCEventCodecsTupleCode[T <: Tuple](using
      Type[T],
      Quotes
  ): Expr[Codecs[OptionsFromTuple[TupleFromCEvents[T]]]] = {
    import quotes.reflect.*

    val AppliedType(_, evTypes) = TypeRepr.of[T]: @unchecked

    val codecs = evTypes.collect {
      case AppliedType(_, List(inner)) =>
        inner.asType match
          case '[t] => '{ JsonCodecMaker.make[Option[t]] }
    }

    Expr.ofTupleFromSeq(codecs).asInstanceOf[Expr[Codecs[OptionsFromTuple[TupleFromCEvents[T]]]]]
  }

  def combineTupleCodecs[T <: Tuple](codecs: Codecs[OptionsFromTuple[T]])(using
      TupleCodec[T]
  ): JsonValueCodec[OptionsFromTuple[T]] =
    new JsonValueCodec[OptionsFromTuple[T]] {
      override def decodeValue(in: JsonReader, default: OptionsFromTuple[T]): OptionsFromTuple[T] =
        if in.isNextToken(91) then {
          val res = summon[TupleCodec[T]].decodeValue(codecs, in, default, false)

          if in.isNextToken(93) then res else in.arrayEndError()
        } else in.readNullOrTokenError(default, 91)

      override def encodeValue(x: OptionsFromTuple[T], out: JsonWriter): Unit = {
        out.writeArrayStart()
        summon[TupleCodec[T]].encodeValue(codecs, x, out)
        out.writeArrayEnd()
      }

      override def nullValue: OptionsFromTuple[T] = summon[TupleCodec[T]].nullValue(codecs)
    }
}
