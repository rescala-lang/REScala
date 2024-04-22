package lofi_acl.access

import lofi_acl.access.Permission.{ALLOW, DENY}
import lofi_acl.access.PermissionTreeValidationException.InvalidPathException

enum Permission:
  case ALLOW
  case DENY

case class PermissionTree(permission: Permission, children: Map[String, PermissionTree]):
  override def toString: String =
    if children.isEmpty then
      permission match
        case Permission.ALLOW => "PermissionTree.allow"
        case Permission.DENY  => "PermissionTree.deny"
    else s"PermissionTree($permission, $children)"

object PermissionTree:
  val allow: PermissionTree = PermissionTree(ALLOW, Map.empty)
  val deny: PermissionTree  = PermissionTree(DENY, Map.empty)

  def fromPermission(permission: Permission): PermissionTree = permission match
    case Permission.ALLOW => allow
    case Permission.DENY  => deny

  def fromPathList(permissionPath: List[(String, Permission)]): PermissionTree =
    // TODO: Implement handling of * (wildcard)
    val permissionPathsAsMap = permissionPath.toMap
    // TODO: Should this check also be present inside of mergeIntoTree? (Depends on rule duplication, etc.)
    require(permissionPathsAsMap.size == permissionPath.size, "Duplicate path in path list")
    permissionPathsAsMap.foreach { case (path, p) =>
      require(!path.endsWith("."), s"Found trailing . in $path -> $p")
    }
    createPermissionTree(permissionPathsAsMap, DENY)

  private def createPermissionTree(
      permissionMap: Map[String, Permission],
      inheritedPermission: Permission
  ): PermissionTree = {
    if permissionMap.isEmpty then return PermissionTree(inheritedPermission, Map.empty)

    val topLevelPermission =
      (permissionMap.get(""), permissionMap.get("*")) match
        case (Some(permission), None) => PermissionTree.fromPermission(permission)
        case (None, Some(permission)) => PermissionTree.fromPermission(permission)
        case (Some(_), Some(_))       => throw IllegalArgumentException("Duplicate top level wildcard rule")
        case (None, None)             => PermissionTree.deny

    // sort by ascending length of path (in this case, the length is the depth of tree, not the string length)
    val sortedPermissions = permissionMap.toArray
      // Path elements are terminated by '.'
      .map { case (path, permission) => path.split('.').toList -> permission }
      .sortInPlaceBy(_._1.length) // Sorts by length as described above

    // TODO: Should we handle wildcards first? Or last?

    // Empty path element is only permitted at root level (e.g., "" -> ALLOW, but not "a..b" -> ALLOW)
    sortedPermissions.find(_._1.drop(1).exists(_.isEmpty)).foreach { case (path, permission) =>
      throw InvalidPathException(path)
    }

    // Now we merge every rule into the permission tree in (shortest paths first, more specific paths later).
    // This is required for longest-prefix-matching semantics.
    sortedPermissions.foldLeft(topLevelPermission) { case (tree, rule) =>
      // Merges the more specific rule into the more general permission tree
      mergeIntoTree(tree, path = rule._1, pathPermission = rule._2)
    }
  }

  // Creates a path that has the permission
  private def pathToTree(path: List[String], inheritedPermission: Permission, permission: Permission): PermissionTree =
    path.foldRight(PermissionTree(permission, Map.empty)) {
      case (pathElement, subtree) => PermissionTree(inheritedPermission, Map(pathElement -> subtree))
    }

  /** This method assumes that the rules are <b>processed from shortest to longest prefix!</b> */
  private def mergeIntoTree(tree: PermissionTree, path: List[String], pathPermission: Permission): PermissionTree =
    require(path.nonEmpty)
    // Same permission on a shorter terminal prefix, no need to add more specific but redundant permission
    if tree.permission == pathPermission && !tree.children.contains(path.head) then return tree

    // In case the new rule isn't redundant, merge the rule into the tree.
    (tree, path) match
      case (parent, pathPrefix :: Nil) => // We are at the end of the inserted path
        require(
          // We found a branch that is created by either a more specific path, or is the same path.
          // But, we assumed that the rules are merged from shortest to longest prefix.
          !parent.children.contains(pathPrefix)
        )
        PermissionTree(parent.permission, parent.children + (pathPrefix -> fromPermission(pathPermission)))
      case (parent, pathPrefix :: pathSuffix) =>
        // Check whether branch exists
        parent.children.get(pathPrefix) match
          case Some(existingSubtree) =>
            // Branch already exists, merge into branch
            PermissionTree(
              parent.permission,
              parent.children + (pathPrefix -> mergeIntoTree(existingSubtree, pathSuffix, pathPermission))
            )
          case None =>
            // Branch doesn't exist, create new branch
            val newSubtree = pathToTree(pathSuffix, parent.permission, pathPermission)
            PermissionTree(parent.permission, parent.children + (pathPrefix -> newSubtree))
      case (parent, Nil) => ??? // Unreachable, see require(path.nonEmpty) above
