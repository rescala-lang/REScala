package lofi_acl.ardt.base

import lofi_acl.access.*
import lofi_acl.access.Permission.{ALLOW, PARTIAL}
import lofi_acl.access.PermissionTreeValidationException.InvalidPathException
import rdts.base.{Bottom, Lattice}

import scala.util.{Failure, Success, Try}

object StandardLibrary:

  // Option[T] with Some > None
  object OptionLattice:
    // TODO: This can also be derived, using the sum type Lattice
    given lattice[T: Lattice]: Lattice[Option[T]] =
      case (l, None)                    => l
      case (None, r)                    => r
      case (Some(lInner), Some(rInner)) => Some(Lattice.merge(lInner, rInner))

    given bottom[Nothing]: Bottom[Option[Nothing]] with
      override val empty: Option[Nothing] = None

    given filter[T: Filter]: Filter[Option[T]] with
      override def filter(delta: Option[T], permission: PermissionTree): Option[T] =
        delta match
          case None => delta
          case Some(value) => permission match
              case PermissionTree(ALLOW, _)          => delta
              case PermissionTree(PARTIAL, children) =>
                // NOTE: Some(bottom) is not None.
                // NOTE: PermissionTree(PARTIAL, Map("a" -> allow)) on Option[T] keeps the field a of T.
                Some(Filter[T].filter(value, permission))

      override def validatePermissionTree(permissionTree: PermissionTree): Try[PermissionTree] =
        if permissionTree.children.isEmpty then Success(permissionTree)
        else Filter[T].validatePermissionTree(permissionTree)

  // Set[T] can be treated as a grow only set
  object GrowOnlySet:
    object mutators:
      def add[T](element: T): Set[T] = Set(element)

    given lattice[T]: Lattice[Set[T]] = (l, r) => l.union(r)

    given bottom[T]: Bottom[Set[T]] with
      override val empty: Set[T] = Set.empty

    given terminalFilter[T]: Filter[Set[T]] with
      override def filter(delta: Set[T], permission: PermissionTree): Set[T] = permission match
        case PermissionTree(ALLOW, _)                                              => delta
        case PermissionTree(PARTIAL, entryPermissions) if entryPermissions.isEmpty => Set.empty
        case PermissionTree(PARTIAL, entryPermissions) =>
          throw IllegalArgumentException("Non-terminal rule used in terminal filter")

      override def validatePermissionTree(permission: PermissionTree): Try[PermissionTree] = permission match
        case PermissionTree(ALLOW, _)                                              => Success(permission)
        case PermissionTree(PARTIAL, entryPermissions) if entryPermissions.isEmpty => Success(permission)
        case PermissionTree(PARTIAL, entryPermissions) => Failure(InvalidPathException(List.empty))

    given filter[T: Filter]: Filter[Set[T]] with
      override def filter(delta: Set[T], permission: PermissionTree): Set[T] = permission match
        case PermissionTree(ALLOW, _)                                              => delta
        case PermissionTree(PARTIAL, entryPermissions) if entryPermissions.isEmpty => Set.empty
        case PermissionTree(PARTIAL, entryPermissions)                             =>
          // TODO: Maybe add support for named child filters
          require(entryPermissions.size == 1, "Only * rules supported in Set filter")
          entryPermissions.get("*") match
            case Some(entryPermission) => delta.map(entry => Filter[T].filter(entry, entryPermission))
            case None                  => ???

      override def validatePermissionTree(permissionTree: PermissionTree): Try[PermissionTree] =
        if permissionTree.children.isEmpty
        then Success(permissionTree)
        else if permissionTree.children.size > 1
        then Failure(PermissionTreeValidationException.InvalidPathException(List.empty))
        else
          permissionTree.children.get("*") match
            case Some(entryPermission) => Filter[T].validatePermissionTree(entryPermission)
            case None                  => Failure(PermissionTreeValidationException.InvalidPathException(List.empty))

  // Map[K, V] can be treated as a grow only map, if V is mergeable (e.g., last-writer-wins register, …)
  object GrowOnlyMap:
    object mutators:
      def putMerged[K, V: Lattice](map: Map[K, V], key: K, rightValue: V): Map[K, V] =
        map.get(key) match
          case Some(leftValue) => Map.Map1(key, Lattice.merge(leftValue, rightValue))
          case None            => Map.Map1(key, rightValue)

    given lattice[K, V: Lattice]: Lattice[Map[K, V]] = (left, right) =>
      // Optimization stolen from REScalas RDT library
      val (small, large) =
        // compare unsigned treats the “unknown” value -1 as larger than any known size
        if 0 <= Integer.compareUnsigned(left.knownSize, right.knownSize)
        then (right, left)
        else (left, right)
      small.foldLeft(large) { case (current, (key, r)) =>
        current.updatedWith(key) {
          case Some(l) => Some(Lattice.merge(l, r))
          case None    => Some(r)
        }
      }

    given bottom[K, V]: Bottom[Map[K, V]] with
      override val empty: Map[K, V] = Map.empty

    given filter[K, V: Filter]: Filter[Map[K, V]] with
      override def filter(delta: Map[K, V], permission: PermissionTree): Map[K, V] =
        permission match
          case PermissionTree(ALLOW, _)                       => delta
          case PermissionTree(PARTIAL, mapOfEntryPermissions) =>
            // TODO: Add MapLikeFilter typeclass that extracts this and makes equality check of key explicit
            delta.flatMap { case key -> value =>
              // TODO: Replace key.toString based lookup with typeclass based equality
              mapOfEntryPermissions.get(key.toString) match
                case None /* No rule for key -> discard entry */ => None
                case Some(entryPermission) => Some(key -> Filter[V].filter(value, entryPermission))
            }

      override def validatePermissionTree(permissionTree: PermissionTree): Try[PermissionTree] =
        if permissionTree.children.isEmpty
        then Success(permissionTree)
        else
          permissionTree.children.foldLeft[(String, Try[PermissionTree])]("" -> Success(permissionTree)) {
            case (prevKeyPath -> prevResult, keyPath -> pt) =>
              if (prevResult.isFailure) prevKeyPath -> prevResult
              else keyPath                          -> Filter[V].validatePermissionTree(pt)
          } match
            case keyPath -> Failure(InvalidPathException(subPath)) => Failure(InvalidPathException(keyPath :: subPath))
            case (_, f @ scala.util.Failure(_))                    => f
            case _ -> Success(_)                                   => Success(permissionTree)
