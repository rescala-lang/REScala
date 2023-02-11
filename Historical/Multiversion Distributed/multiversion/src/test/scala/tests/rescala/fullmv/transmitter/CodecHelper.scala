package tests.rescala.fullmv.transmitter

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._

object CodecHelper {

  type FullMvTransport[ActualType] = scala.Tuple2[
    scala.Long,
    scala.Tuple3[java.lang.String, scala.collection.immutable.List[scala.Tuple4[
      scala.Long,
      scala.Int,
      scala.Option[scala.Tuple2[rescala.fullmv.CaseClassTransactionSpanningTreeNode[scala.Tuple2[
        rescala.fullmv.mirrors.Host.GUID,
        rescala.fullmv.TurnPhase.Type
      ]], scala.Int]],
      scala.Option[scala.Tuple2[
        scala.Option[ActualType],
        scala.Option[scala.Array[scala.Byte]]
      ]]
    ]], scala.Array[scala.Byte]]
  ]

  given fullCodec[T: JsonValueCodec]: JsonValueCodec[FullMvTransport[T]] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

}
