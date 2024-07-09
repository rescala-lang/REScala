package lofi_acl.ardt.datatypes

import lofi_acl.access.Permission.*
import lofi_acl.access.PermissionTreeValidationException.InvalidPathException
import lofi_acl.access.{Filter, PermissionTree}
import rdts.datatypes.contextual.ObserveRemoveMap

import scala.util.{Failure, Success, Try}

object ORMap {
  given stringKeyORMapFilter[V: Filter]: Filter[ObserveRemoveMap[String, V]] with
    override def filter(delta: ObserveRemoveMap[String, V], permission: PermissionTree): ObserveRemoveMap[String, V] =
      permission match
        case PermissionTree(ALLOW, _) => delta
        case PermissionTree(PARTIAL, mapOfEntryPermissions) =>
          val wildcardPermission = mapOfEntryPermissions.get("*")
          ObserveRemoveMap(
            delta.inner.flatMap { case key -> value =>
              // Assumes normalized PermissionTree
              mapOfEntryPermissions.get(key) match
                case Some(entryPermission) => Some(key -> Filter[V].filter(value, entryPermission))
                case None =>
                  if wildcardPermission.nonEmpty
                  then Some(key -> Filter[V].filter(value, wildcardPermission.get))
                  else None /* No rule for key -> discard entry */
            }
          )

    override def validatePermissionTree(permissionTree: PermissionTree): Try[PermissionTree] =
      if permissionTree.children.isEmpty
      then Success(permissionTree)
      else
        Try {
          PermissionTree(
            PARTIAL,
            permissionTree.children.map { (key, perm) =>
              Filter[V].validatePermissionTree(perm) match
                case Failure(e: InvalidPathException) => throw InvalidPathException(key :: e.path)
                case Failure(e)                       => throw InvalidPathException(List(key))
                case Success(validated)               => key -> validated
            }
          )
        }

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      val minimized = PermissionTree(
        permission = permissionTree.permission,
        children = permissionTree.children.map((label, child) => label -> Filter[V].minimizePermissionTree(child))
      )

      if minimized.children.contains("*")
      then PermissionTree.lattice.normalizeWildcards(minimized)
      else minimized
}
