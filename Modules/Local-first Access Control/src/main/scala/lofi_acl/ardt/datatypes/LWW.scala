package lofi_acl.ardt.datatypes

import lofi_acl.access.Permission.{ALLOW, PARTIAL}
import lofi_acl.access.InvalidPathException
import lofi_acl.access.{Filter, PermissionTree}
import rdts.base.Bottom
import rdts.datatypes.LastWriterWins

type LWW[V] = LastWriterWins[V]

object LWW {
  given recursiveFilter[V: Filter: Bottom]: Filter[LWW[V]] with
    override def filter(delta: LWW[V], permission: PermissionTree): LWW[V] = permission match
      case PermissionTree(ALLOW, _)   => delta
      case PermissionTree(PARTIAL, _) => delta.copy(payload = Filter[V].filter(delta.read, permission))

    override def validatePermissionTree(permissionTree: PermissionTree): Unit = permissionTree match
      case PermissionTree(ALLOW, _)   =>
      case PermissionTree(PARTIAL, _) => Filter[V].validatePermissionTree(permissionTree)

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree =
      Filter[V].minimizePermissionTree(permissionTree)

  given terminalFilter[V: Bottom]: Filter[LWW[V]] with
    override def filter(delta: LWW[V], permission: PermissionTree): LWW[V] = permission match
      case PermissionTree(ALLOW, _)                 => delta
      case PermissionTree(PARTIAL, valuePermission) =>
        // This is actually never reached, if using normalized permission trees
        require(valuePermission.isEmpty)
        LastWriterWins.bottom[V].empty

    override def validatePermissionTree(permissionTree: PermissionTree): Unit =
      if permissionTree.children.nonEmpty then throw InvalidPathException(permissionTree.children.keys.head :: Nil)

    override def minimizePermissionTree(permissionTree: PermissionTree): PermissionTree = permissionTree
}
