package lofi_acl.ardt.base

import lofi_acl.access.*
import lofi_acl.access.Permission.{ALLOW, DENY}
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
        if delta.isEmpty
        then delta
        else
          permission match
            case PermissionTree(DENY, children) if children.isEmpty  => None
            case PermissionTree(ALLOW, children) if children.isEmpty => delta
            case PermissionTree(_, children)                         => Some(Filter[T].filter(delta.get, permission))

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

    given filter[T]: Filter[Set[T]] with
      override def filter(delta: Set[T], permission: PermissionTree): Set[T] = permission match
        case PermissionTree(ALLOW, children) if children.isEmpty => delta
        case PermissionTree(DENY, children) if children.isEmpty  => Set.empty
        case PermissionTree(_, _)                                => ??? // TODO: Maybe add support for child filters

      override def validatePermissionTree(permissionTree: PermissionTree): Try[PermissionTree] =
        if permissionTree.children.isEmpty
        then Success(permissionTree)
        else Failure(PermissionTreeValidationException.InvalidPathException(List.empty))

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
          case PermissionTree(DENY, children) if children.isEmpty   => Map.empty
          case PermissionTree(ALLOW, children) if children.isEmpty  => delta
          case PermissionTree(parentPermission, childrenPermission) =>
            // TODO: Add MapLikeFilter typeclass that extracts this and makes equality check of key explicit
            delta.flatMap { case key -> value =>
              // TODO: Replace key.toString based lookup with typeclass based equality
              childrenPermission.get(key.toString) match
                case Some(keyPermissionTree) =>
                  if keyPermissionTree.children.isEmpty then
                    if keyPermissionTree.permission == DENY
                    then None
                    else Some(key -> value)
                  else Some(key -> Filter[V].filter(value, keyPermissionTree))
                case None /* No rule specific to mapping -> use parent permission */ =>
                  if parentPermission == ALLOW
                  then Some(key -> value)
                  else None
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
