package lofi_acl.access

import lofi_acl.access.Permission.{ALLOW, PARTIAL}
import lofi_acl.access.PermissionTree
import lofi_acl.access.PermissionTreeValidationException.InvalidPathException
import rdts.base.Bottom

import scala.compiletime.{constValue, erasedValue, summonAll}
import scala.deriving.Mirror
import scala.util.{Failure, Success, Try}

trait Filter[T]:
  // TODO: Maybe the identity should be fixed at the creation point of the Filter. This would allow us to simply replace
  // the $userid variable when generating the filter instead of querying at runtime.
  // This would also allow us to simplify the creation of filters for custom derived RDTs by preprocessing the ACL and
  // Replacing the occurrence of $… with concrete values.
  def filter(delta: T, permission: PermissionTree): T

  /** Checks whether the permission tree is valid.
    * <li> Not DENY & ALLOW on the same level
    * <li> All fields exist
    *
    * @param permissionTree The tree to check
    * @return Success(the validated permission tree) or a Failure(with the cause).
    */
  def validatePermissionTree(permissionTree: PermissionTree): Try[PermissionTree]

object Filter:
  inline def apply[T](using filter: Filter[T]): Filter[T] = filter

  // From https://blog.philipp-martini.de/blog/magic-mirror-scala3/
  private inline def getFactorLabels[A <: Tuple]: List[String] = inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => constValue[t].toString :: getFactorLabels[ts]
  }

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T], productBottom: Bottom[T]): Filter[T] =
    val factorBottoms = summonAll[Tuple.Map[pm.MirroredElemTypes, Bottom]].toIArray.map(_.asInstanceOf[Bottom[Any]])
    val factorFilters = summonAll[Tuple.Map[pm.MirroredElemTypes, Filter]].toIArray.map(_.asInstanceOf[Filter[Any]])
    val factorLabels  = getFactorLabels[pm.MirroredElemLabels].zipWithIndex.toMap
    ProductTypeFilter[T](pm, productBottom, factorLabels, factorBottoms, factorFilters)

  class ProductTypeFilter[T](
      pm: Mirror.ProductOf[T],
      productBottom: Bottom[T],           // The bottom of the product (derivable as the product of bottoms)
      factorLabels: Map[String, Int],     // Maps the factor label to the factor index
      factorBottoms: IArray[Bottom[Any]], // The Bottom TypeClass instance for each factor
      factorFilters: IArray[Filter[Any]]  // The Filter TypeClass instance for each factor
  ) extends Filter[T]:
    override def filter(delta: T, permissionTree: PermissionTree): T = {
      permissionTree match
        case PermissionTree(ALLOW, _)          => delta
        case PermissionTree(PARTIAL, children) =>
          // Apply filters to factors, if rule for factor is specified.
          // Otherwise use bottom for factor
          val filteredProduct = filterProduct(delta.asInstanceOf[Product], permissionTree)
          pm.fromProduct(filteredProduct)
    }

    private def filterProduct(product: Product, permissionTree: PermissionTree): Product = {
      val filteredFactors = permissionTree.children.map { case (factorName, permissionSubTree) =>
        // Assumes that permission tree is valid (i.e., factorName is a valid element)
        val factorIndex   = factorLabels(factorName)
        val factorOfDelta = product.productElement(factorIndex)
        factorIndex -> factorFilters(factorIndex).filter(factorOfDelta, permissionSubTree)
      }

      new Product:
        def canEqual(that: Any): Boolean = false
        def productArity: Int            = factorBottoms.length
        def productElement(i: Int): Any  = filteredFactors.getOrElse(i, () => factorBottoms(i))
    }

    /** Checks whether all children labels are the field names of this product and validates the filters for the children.
      *
      * @param permissionTree The tree to check
      *  @return Success(the validated permission tree) or a Failure(with the cause).
      */
    def validatePermissionTree(permissionTree: PermissionTree): Try[PermissionTree] = {
      permissionTree match
        case PermissionTree(_, children) =>
          val validationResultsOfChildren = children.map { case (factorLabel, factorPermissionTree) =>
            factorLabels.get(factorLabel) match
              case None => Failure(InvalidPathException(List(factorLabel)))
              case Some(factorIdx) =>
                factorFilters(factorIdx).validatePermissionTree(factorPermissionTree) match
                  case s @ Success(_) => s
                  case Failure(InvalidPathException(path: List[String])) =>
                    Failure(InvalidPathException(factorLabel :: path))
                  case f @ Failure(_) => f
          }
          validationResultsOfChildren.find { _.isInstanceOf[Failure[?]] }.getOrElse(Success(permissionTree))
    }
