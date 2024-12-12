package lofi_acl.ardt.datatypes

import lofi_acl.access.Permission.*
import lofi_acl.access.InvalidPathException
import lofi_acl.access.{Filter, PermissionTree}
import rdts.base.Bottom
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.datatypes.contextual.ObserveRemoveMap.Entry

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

    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      if permissionTree.children.nonEmpty
      then
        permissionTree.children.foreach { (key, perm) =>
          try {
            Filter[V].validatePermissionTree(perm)
          } catch
            case e: InvalidPathException => throw InvalidPathException(key :: e.path)
            case _                       => throw InvalidPathException(List(key))
        }

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      val minimized = PermissionTree(
        permission = permissionTree.permission,
        children = permissionTree.children.map((label, child) => label -> Filter[V].minimizePermissionTree(child))
      )

      minimized.children.get("*") match
        case Some(wildcard) =>
          PermissionTree.lattice.normalizeWildcards(
            permissionTree.copy(
              children = minimized.children.filter { (label, child) => label == "*" || !(child <= wildcard) }
            )
          )
        case None => minimized

  given observeRemoveMapEntryFilter[A: { Filter, Bottom }]: Filter[Entry[A]] = Filter.derived
}
