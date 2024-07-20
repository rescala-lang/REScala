package lofi_acl.access

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import rdts.base.Bottom
import rdts.dotted.{Dotted, Obrem}
import rdts.time.Dots

import scala.compiletime.{constValue, erasedValue, summonAll}
import scala.deriving.Mirror

case class IsolatedDeltaParts(inner: Map[String, IsolatedDeltaParts] | Array[Byte]) {
  def isEmpty: Boolean = inner match
    case array: Array[Byte] => false
    case map: Map[?, ?]     => map.isEmpty
}

trait DeltaSurgeon[T] {
  def isolate(delta: T): IsolatedDeltaParts

  def recombine(parts: IsolatedDeltaParts): T

  def filter(isolatedDeltaParts: IsolatedDeltaParts, permissionTree: PermissionTree): IsolatedDeltaParts = {
    (isolatedDeltaParts.inner, permissionTree) match
      case (atomicValue: Array[Byte], PermissionTree(Permission.ALLOW, _))                 => isolatedDeltaParts
      case (parts: Map[String, IsolatedDeltaParts], PermissionTree(Permission.ALLOW, _))   => isolatedDeltaParts
      case (parts: Map[String, IsolatedDeltaParts], PermissionTree(Permission.PARTIAL, _)) =>
        // TODO: Check after implementing ORMap DeltaSurgeon
        val wildcard = permissionTree.children.getOrElse("*", PermissionTree.empty)
        IsolatedDeltaParts(
          parts.map { (pathElement, childParts) =>
            val childPermission = permissionTree.children.getOrElse(pathElement, wildcard)
            pathElement -> filter(childParts, childPermission)
          }.filterNot(_._2.isEmpty)
        )
      case (atomicValue: Array[Byte], PermissionTree(Permission.PARTIAL, _)) =>
        ??? // Invalid PermissionTree or IsolatedDeltaParts
  }
}

object DeltaSurgeon {
  // From https://blog.philipp-martini.de/blog/magic-mirror-scala3/
  private inline def getFactorLabels[A <: Tuple]: List[String] = inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => constValue[t].toString :: getFactorLabels[ts]
  }

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T], productBottom: Bottom[T]): DeltaSurgeon[T] =
    val factorBottoms = summonAll[Tuple.Map[pm.MirroredElemTypes, Bottom]].toIArray.map(_.asInstanceOf[Bottom[Any]])
    val factorSurgeons =
      summonAll[Tuple.Map[pm.MirroredElemTypes, DeltaSurgeon]].toIArray.map(_.asInstanceOf[DeltaSurgeon[Any]])
    val factorLabels = getFactorLabels[pm.MirroredElemLabels]
    ProductTypeSurgeon[T](pm, productBottom, factorLabels, factorBottoms, factorSurgeons)

  class ProductTypeSurgeon[T](
      pm: Mirror.ProductOf[T],
      productBottom: Bottom[T],                  // The bottom of the product (derivable as the product of bottoms)
      factorLabels: List[String],                // Maps the factor label to the factor index
      factorBottoms: IArray[Bottom[Any]],        // The Bottom TypeClass instance for each factor
      factorSurgeons: IArray[DeltaSurgeon[Any]], // The DeltaSurgeon TypeClass instance for each factor
  ) extends DeltaSurgeon[T]:
    private val factorLabelToIndexMap = factorLabels.zipWithIndex.toMap

    override def isolate(delta: T): IsolatedDeltaParts = {
      if productBottom.isEmpty(delta) then IsolatedDeltaParts(Map.empty)
      else isolateProduct(delta.asInstanceOf[Product])
    }

    private def isolateProduct(product: Product): IsolatedDeltaParts = {
      IsolatedDeltaParts(
        factorLabels.zipWithIndex.flatMap { (factorLabel, factorIdx) =>
          val factorSurgeon = factorSurgeons(factorIdx)
          val factor        = product.productElement(factorIdx)
          if factorBottoms(factorIdx).isEmpty(factor)
          then None
          else Some(factorLabel -> factorSurgeon.isolate(factor))
        }.toMap
      )
    }

    override def recombine(delta: IsolatedDeltaParts): T = {
      // A Product is a compound type and therefore only as represented as a path, but never as a serialized value
      require(delta.inner.isInstanceOf[Map[?, ?]])
      delta.inner match
        case parts: Map[String, IsolatedDeltaParts] =>
          val recombinedFactors = parts.map { (pathElement, factorParts) =>
            val factorIdx = factorLabelToIndexMap(pathElement)
            factorIdx -> factorSurgeons(factorIdx).recombine(factorParts)
          }
          val recombinedProduct: Product = new Product:
            def canEqual(that: Any): Boolean = false
            def productArity: Int            = factorBottoms.length
            def productElement(i: Int): Any  = recombinedFactors.getOrElse(i, factorBottoms(i).empty)
          pm.fromProduct(recombinedProduct)
        case byteArray => ???
    }

  import lofi_acl.sync.JsoniterCodecs.dotsCodec

  given dotsDeltaSurgeon: DeltaSurgeon[Dots]                                 = ofTerminalValue[Dots]
  given dottedDeltaSurgeon[T: DeltaSurgeon: Bottom]: DeltaSurgeon[Dotted[T]] = DeltaSurgeon.derived
  given obremDeltaSurgeon[T: DeltaSurgeon: Bottom]: DeltaSurgeon[Obrem[T]]   = DeltaSurgeon.derived

  // Used for values that should not be further isolated
  def ofTerminalValue[V: Bottom: JsonValueCodec]: DeltaSurgeon[V] = new TerminalValueDeltaSurgeon[V]

  private class TerminalValueDeltaSurgeon[V: Bottom: JsonValueCodec] extends DeltaSurgeon[V] {
    override def isolate(delta: V): IsolatedDeltaParts =
      if Bottom[V].isEmpty(delta) then IsolatedDeltaParts(Map.empty)
      else IsolatedDeltaParts(writeToArray(delta))

    override def recombine(parts: IsolatedDeltaParts): V = parts.inner match
      case map: Map[?, ?] =>
        require(map.isEmpty) // Terminal values cant be isolated further
        Bottom[V].empty
      case serializedValue: Array[Byte] => readFromArray(serializedValue)
  }

}
