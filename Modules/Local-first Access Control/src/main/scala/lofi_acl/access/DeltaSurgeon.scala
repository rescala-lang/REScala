package lofi_acl.access

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import rdts.base.Bottom
import rdts.dotted.{Dotted, Obrem}
import rdts.time.Dots

import scala.compiletime.*
import scala.deriving.Mirror

case class IsolatedDeltaParts(inner: Map[String, IsolatedDeltaParts] | Array[Byte]) {
  def isEmpty: Boolean = inner match
    case array: Array[Byte] => false
    case map: Map[?, ?]     => map.isEmpty
}

object IsolatedDeltaParts {
  val empty: IsolatedDeltaParts = IsolatedDeltaParts(Map.empty)
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
      case (atomicValue: Array[Byte], PermissionTree(Permission.PARTIAL, children)) if children.isEmpty =>
        IsolatedDeltaParts.empty
      case _ => ??? // Invalid PermissionTree or IsolatedDeltaParts
  }
}

object DeltaSurgeon {
  inline def apply[T](using deltaSurgeon: DeltaSurgeon[T]): DeltaSurgeon[T] = deltaSurgeon

  // From https://blog.philipp-martini.de/blog/magic-mirror-scala3/
  private inline def getLabels[A <: Tuple]: List[String] = inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => constValue[t].toString :: getLabels[ts]
  }

  inline def derived[T](using m: Mirror.Of[T], bottom: Bottom[T]): DeltaSurgeon[T] =
    val elementLabels = getLabels[m.MirroredElemLabels].toArray
    // TODO: Don't summon singleton delta surgeons but derive from here
    val elementSurgeons =
      summonAll[Tuple.Map[m.MirroredElemTypes, DeltaSurgeon]].toIArray.map(_.asInstanceOf[DeltaSurgeon[Any]])
    inline m match
      case sumMirror: Mirror.SumOf[T] => SumTypeDeltaSurgeon[T](elementLabels, elementSurgeons)(using sumMirror)
      case singletonMirror: Mirror.Singleton =>
        given bottom: Bottom[T] = Bottom.provide(singletonMirror.fromProduct(null))
        ProductTypeSurgeon[T](bottom, elementLabels, IArray.empty, elementSurgeons)(using singletonMirror)
      case productMirror: Mirror.ProductOf[T] =>
        val elementBottoms = summonAll[Tuple.Map[m.MirroredElemTypes, Bottom]].toIArray.map(_.asInstanceOf[Bottom[Any]])
        ProductTypeSurgeon[T](bottom, elementLabels, elementBottoms, elementSurgeons)(using productMirror)

  private inline given sumElemLabels[T](using sm: Mirror.SumOf[T]): List[Any] =
    constValueTuple[sm.MirroredElemLabels].map[[X] =>> String]([X] => (x: X) => x.toString).toList

  class ProductTypeSurgeon[T](
      productBottom: Bottom[T],                  // The bottom of the product (derivable as the product of bottoms)
      factorLabels: Array[String],               // Maps the factor label to the factor index
      factorBottoms: IArray[Bottom[Any]],        // The Bottom TypeClass instance for each factor
      factorSurgeons: IArray[DeltaSurgeon[Any]], // The DeltaSurgeon TypeClass instance for each factor
  )(using pm: Mirror.ProductOf[T]) extends DeltaSurgeon[T]:
    require(factorLabels.toSet.size == factorLabels.length)
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

  class SumTypeDeltaSurgeon[T](
      elementLabels: Array[String], // Maps the ordinal value to the string representation of the element type
      elementSurgeons: IArray[DeltaSurgeon[Any]], // The DeltaSurgeon TypeClass instance for each type
  )(using sm: Mirror.SumOf[T]) extends DeltaSurgeon[T]:
    require(elementLabels.toSet.size == elementLabels.length)
    private val ordinalLookup = elementLabels.zipWithIndex.toMap

    override def isolate(delta: T): IsolatedDeltaParts = {
      val ordinal         = sm.ordinal(delta)
      val label           = elementLabels(sm.ordinal(delta))
      val isolatedElement = elementSurgeons(ordinal).isolate(delta)
      IsolatedDeltaParts(Map(label -> isolatedElement))
    }

    override def recombine(parts: IsolatedDeltaParts): T = {
      parts.inner match
        case map: Map[String, IsolatedDeltaParts] =>
          require(map.size == 1)
          map.head match
            case (elementType, element) =>
              val ordinal = ordinalLookup(elementType)
              elementSurgeons(ordinal).recombine(element).asInstanceOf[T]
        case arr: Array[Byte] => ???
    }

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

  import lofi_acl.sync.JsoniterCodecs.dotsCodec

  given dotsDeltaSurgeon: DeltaSurgeon[Dots]                                 = ofTerminalValue[Dots]
  given dottedDeltaSurgeon[T: DeltaSurgeon: Bottom]: DeltaSurgeon[Dotted[T]] = DeltaSurgeon.derived
  given obremDeltaSurgeon[T: DeltaSurgeon: Bottom]: DeltaSurgeon[Obrem[T]]   = DeltaSurgeon.derived
  given optionSurgeon[T: Bottom: DeltaSurgeon]: DeltaSurgeon[Option[T]] = {
    given noneBottom: Bottom[None.type] = Bottom.provide(None) // TODO: Bottom for singletons should be derivable
    given noneDeltaSurgeon: DeltaSurgeon[None.type] = DeltaSurgeon.derived
    given someBottom: Bottom[Some[T]]               = Bottom.derived
    given someSurgeon: DeltaSurgeon[Some[T]]        = DeltaSurgeon.derived
    DeltaSurgeon.derived[Option[T]]
  }

}
