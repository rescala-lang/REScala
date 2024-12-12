package lofi_acl.access

import lofi_acl.access.Permission.{ALLOW, PARTIAL}
import lofi_acl.access.PermissionTree.allow
import rdts.base.Bottom
import rdts.dotted.{Dotted, Obrem}
import rdts.time.{ArrayRanges, Dots}

import scala.compiletime.{constValue, erasedValue, summonAll}
import scala.deriving.Mirror

trait Filter[T]:
  def filter(delta: T, permission: PermissionTree): T

  /** Checks whether the permission tree is valid.
    * <li> Not DENY & ALLOW on the same level
    * <li> All fields exist
    *
    * @param permissionTree The tree to check
    * @return Success(the validated permission tree) or a Failure(with the cause).
    */
  def validatePermissionTree(permissionTree: PermissionTree): Unit

  def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree

object Filter:
  inline def apply[T](using filter: Filter[T]): Filter[T] = filter

  // From https://blog.philipp-martini.de/blog/magic-mirror-scala3/
  private inline def getElementNames[A <: Tuple]: List[String] = inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => constValue[t].toString :: getElementNames[ts]
  }

  inline def derived[T](using m: Mirror.Of[T], productBottom: Bottom[T]): Filter[T] =
    // Element is either a factor of a product or an element in a sum
    val elementBottoms = summonAll[Tuple.Map[m.MirroredElemTypes, Bottom]].toIArray.map(_.asInstanceOf[Bottom[Any]])
    val elementFilters = summonAll[Tuple.Map[m.MirroredElemTypes, Filter]].toIArray.map(_.asInstanceOf[Filter[Any]])
    val elementNames   = getElementNames[m.MirroredElemLabels]
    require(elementNames.toSet.size == elementNames.length) // Ensure uniqueness of element names
    inline m match
      case prodMirror: Mirror.ProductOf[T] =>
        ProductTypeFilter[T](prodMirror, productBottom, elementNames.zipWithIndex.toMap, elementBottoms, elementFilters)
      case sumMirror: Mirror.SumOf[T] =>
        SumTypeFilter[T](sumMirror, productBottom, elementNames.toArray, elementBottoms, elementFilters)

  protected abstract class AlgebraicFilter[T](
      elementLabels: Map[String, Int],
      elementFilters: IArray[Filter[Any]],
  ) extends Filter[T] {

    /** Checks whether all children labels are the field/type names of this product and validates the filters for the children.
      *
      * @param permissionTree The tree to check
      */
    override def validatePermissionTree(permissionTree: PermissionTree): Unit = {
      permissionTree.children.foreach { case (factorLabel, childPermissionTree) =>
        elementLabels.get(factorLabel) match
          case Some(factorIdx) =>
            try {
              elementFilters(factorIdx).validatePermissionTree(childPermissionTree)
            } catch {
              case InvalidPathException(path) => throw InvalidPathException(factorLabel :: path)
            }
          case None if factorLabel == "*" =>
            try {
              elementFilters.foreach(_.validatePermissionTree(childPermissionTree))
            } catch {
              case InvalidPathException(labels) => throw InvalidPathException("*" :: labels)
            }
          case None => throw InvalidPathException(List(factorLabel))
      }
    }

    // Assumes a valid permission tree
    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree = {
      if permissionTree.permission == ALLOW then return allow

      val minimizedChildren = permissionTree.children.map { (label, subtree) =>
        val idx                            = elementLabels(label)
        val filter                         = elementFilters(idx)
        val minimizedChild: PermissionTree = filter.minimizePermissionTree(subtree)
        label -> minimizedChild
      }

      if minimizedChildren.size == elementFilters.size && minimizedChildren.forall(_._2.permission == ALLOW)
      then allow
      else PermissionTree(PARTIAL, minimizedChildren)
    }
  }

  class ProductTypeFilter[T](
      pm: Mirror.ProductOf[T],
      productBottom: Bottom[T],           // The bottom of the product (derivable as the product of bottoms)
      factorLabels: Map[String, Int],     // Maps the factor label to the factor index
      factorBottoms: IArray[Bottom[Any]], // The Bottom TypeClass instance for each factor
      factorFilters: IArray[Filter[Any]]  // The Filter TypeClass instance for each factor
  ) extends AlgebraicFilter[T](factorLabels, factorFilters):
    override def filter(delta: T, permissionTree: PermissionTree): T = {
      permissionTree match
        case PermissionTree(ALLOW, _)                              => delta
        case PermissionTree(PARTIAL, children) if children.isEmpty => productBottom.empty
        case PermissionTree(PARTIAL, children)                     =>
          // Apply filters to factors, if rule for factor is specified.
          // Otherwise use bottom for factor
          val filteredProduct = filterProduct(delta.asInstanceOf[Product], permissionTree)
          pm.fromProduct(filteredProduct)
    }

    private def filterProduct(product: Product, permissionTree: PermissionTree): Product = {
      permissionTree.children.get("*") match
        case None =>
          val filteredFactors = permissionTree.children.map { case (factorName, permissionSubTree) =>
            // Assumes that permission tree is valid (i.e., factorName is a valid element)
            val factorIndex   = factorLabels(factorName)
            val factorOfDelta = product.productElement(factorIndex)
            factorIndex -> factorFilters(factorIndex).filter(factorOfDelta, permissionSubTree)
          }
          new Product:
            def canEqual(that: Any): Boolean = false
            def productArity: Int            = factorBottoms.length
            def productElement(i: Int): Any  = filteredFactors.getOrElse(i, factorBottoms(i).empty)
        case Some(wildcard) =>
          val filteredFactors = factorLabels.map { (label, factorIndex) =>
            val permissions   = permissionTree.children.getOrElse(label, wildcard)
            val factorOfDelta = product.productElement(factorIndex)
            factorIndex -> factorFilters(factorIndex).filter(factorOfDelta, permissions)
          }
          new Product:
            def canEqual(that: Any): Boolean = false
            def productArity: Int            = factorBottoms.length
            def productElement(i: Int): Any  = filteredFactors(i)
    }

  class SumTypeFilter[T](
      sm: Mirror.SumOf[T],
      bottom: Bottom[T],                   // The bottom of the sum
      elementNames: Array[String],         // The names of the types
      elementBottoms: IArray[Bottom[Any]], // The Bottom TypeClass instance for each element
      elementFilters: IArray[Filter[Any]]  // The Filter TypeClass instance for each element
  ) extends AlgebraicFilter[T](elementNames.zipWithIndex.toMap, elementFilters):
    override def filter(delta: T, permission: PermissionTree): T =
      permission match
        case PermissionTree(ALLOW, _)                              => delta
        case PermissionTree(PARTIAL, children) if children.isEmpty => bottom.empty
        case PermissionTree(PARTIAL, children) =>
          val ordinal     = sm.ordinal(delta)
          val elementName = elementNames(ordinal)
          children.getOrElse(elementName, children.getOrElse("*", PermissionTree.empty)) match
            case PermissionTree(ALLOW, _)                              => delta
            case PermissionTree(PARTIAL, children) if children.isEmpty => bottom.empty
            case childPerm @ PermissionTree(PARTIAL, children) =>
              elementFilters(ordinal).filter(delta, childPerm).asInstanceOf[T]

  given dotsFilter: Filter[Dots] with
    override def filter(delta: Dots, permission: PermissionTree): Dots =
      permission match
        case PermissionTree(ALLOW, _)                              => delta
        case PermissionTree(PARTIAL, children) if children.isEmpty => Dots.empty
        case PermissionTree(PARTIAL, perms) =>
          Dots(delta.internal.flatMap((uid, ranges) =>
            perms.get(uid.delegate) match
              case Some(PermissionTree(ALLOW, _)) => Some(uid -> ranges)
              case Some(PermissionTree(PARTIAL, dotPermissions)) =>
                val allowedRanges = ArrayRanges.from(
                  dotPermissions.flatMap { (timeAsString, perm) =>
                    if perm.isEmpty
                    then None
                    else Some(java.lang.Long.parseUnsignedLong(timeAsString))
                  }
                )
                Some(uid -> ranges.intersect(allowedRanges))
              case None => None
          ))

    override def validatePermissionTree(permissionTree: PermissionTree): Unit = {
      permissionTree.children.foreach { (_, childPerm) =>
        childPerm.children.foreach { (key, value) =>
          try {
            val _ = java.lang.Long.parseUnsignedLong(key)
          } catch {
            case e: NumberFormatException => throw InvalidPathException(key :: Nil)
          }
        }
      }
    }

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      if permissionTree.permission == ALLOW then return PermissionTree.allow
      PermissionTree(
        PARTIAL,
        permissionTree.children.flatMap {
          case (uid, PermissionTree(ALLOW, _))                              => Some(uid -> PermissionTree.allow)
          case (uid, PermissionTree(PARTIAL, dotPerms)) if dotPerms.isEmpty => None
          case (uid, PermissionTree(PARTIAL, dotPerms)) =>
            dotPerms.flatMap {
              case (time, PermissionTree(ALLOW, _))   => Some(time -> PermissionTree.allow)
              case (time, PermissionTree(PARTIAL, _)) => None
            }
        }
      )

  class TerminalFilter[T: Bottom] extends Filter[T]:
    override def filter(delta: T, permission: PermissionTree): T =
      permission match
        case PermissionTree(ALLOW, _)   => delta
        case PermissionTree(PARTIAL, _) => Bottom[T].empty
    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      require(permissionTree.children.isEmpty)
    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      permissionTree match
        case PermissionTree(ALLOW, _)   => PermissionTree.allow
        case PermissionTree(PARTIAL, _) => PermissionTree.empty

  def ofTerminalValue[T: Bottom]: Filter[T] = TerminalFilter[T]()

  given dottedFilter[A: { Filter, Bottom }]: Filter[Dotted[A]] = Filter.derived

  given obremFilter[A: { Filter, Bottom }]: Filter[Obrem[A]] = Filter.derived
