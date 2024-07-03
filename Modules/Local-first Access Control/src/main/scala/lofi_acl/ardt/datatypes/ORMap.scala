package lofi_acl.ardt.datatypes

import lofi_acl.access.Permission.*
import lofi_acl.access.PermissionTreeValidationException.InvalidPathException
import lofi_acl.access.{Filter, PermissionTree}
import rdts.datatypes.contextual.ObserveRemoveMap

import scala.util.{Failure, Success, Try}

type ORMap[K, V] = ObserveRemoveMap[K, V]

object ORMap {
  given stringKeyORMapFilter[V: Filter]: Filter[ObserveRemoveMap[String, V]] with
    override def filter(delta: ORMap[String, V], permission: PermissionTree): ORMap[String, V] =
      permission match
        case PermissionTree(ALLOW, _) => delta
        case PermissionTree(PARTIAL, mapOfEntryPermissions) =>
          ObserveRemoveMap(
            delta.inner.flatMap { case key -> value =>
              mapOfEntryPermissions.get(key) match
                case None /* No rule for key -> discard entry */ => None
                case Some(entryPermission) => Some(key -> Filter[V].filter(value, entryPermission))
            }
          )

    override def validatePermissionTree(permissionTree: PermissionTree): Try[PermissionTree] =
      if permissionTree.children.isEmpty
      then Success(permissionTree)
      else
        permissionTree.children.foldLeft[(String, Try[PermissionTree])]("" -> Success(permissionTree)) {
          case (prevKeyPath -> prevResult, keyPath -> pt) =>
            if prevResult.isFailure then prevKeyPath -> prevResult
            else keyPath                             -> Filter[V].validatePermissionTree(pt)
        } match
          case keyPath -> Failure(InvalidPathException(subPath)) => Failure(InvalidPathException(keyPath :: subPath))
          case (_, f @ scala.util.Failure(_))                    => f
          case _ -> Success(_)                                   => Success(permissionTree)

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      val minimized = PermissionTree(
        permission = permissionTree.permission,
        children = permissionTree.children.map((label, child) => label -> Filter[V].minimizePermissionTree(child))
      )

      if minimized.children.contains("*")
      then PermissionTree.lattice.normalizeWildcards(minimized)
      else minimized
}
