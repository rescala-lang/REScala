package lofi_acl.access

import lofi_acl.access.Operation.{READ, WRITE}
import lofi_acl.access.PermissionManager.{mergePermissionIntoPermissionMap, rulesToGroupedPermissionTrees}
import rdts.base.Uid

import scala.annotation.tailrec

class PermissionManager[RDT] {
  private var _groups: Set[Group]                   = Set.empty
  private var _membersOfGroup: Map[Group, Set[Uid]] = _groups.map(group => group -> Set.empty).toMap
  private var _groupsOfUser: Map[Uid, Set[Group]]   = Map.empty
  private var _users: Set[Uid]                      = Set.empty
  private var _rules: Set[Rule]                     = Set.empty

  private var _readPermissionsOfUser: Map[Uid, PermissionTree]  = Map.empty
  private var _writePermissionsOfUser: Map[Uid, PermissionTree] = Map.empty

  def getCurrentPermissionsOfUser(user: Uid, operation: Operation): Option[PermissionTree] =
    this.synchronized(
      operation match
        case Operation.READ  => _readPermissionsOfUser.get(user)
        case Operation.WRITE => _writePermissionsOfUser.get(user)
    )

  def userAdded(newUser: Uid, initialGroups: Set[Group]): Unit =
    this.synchronized {
      _users = _users + newUser
      userJoinedGroups(newUser, initialGroups)
    }

  def groupAdded(newGroup: Group): Unit =
    this.synchronized {
      _groups = _groups + newGroup
    }

  def rulesAdded(newRules: Set[Rule]): Unit = {
    val newReadPermissions  = rulesToGroupedPermissionTrees(newRules, READ)
    val newWritePermissions = rulesToGroupedPermissionTrees(newRules, WRITE)

    this.synchronized {
      _readPermissionsOfUser = newReadPermissions.foldLeft(_readPermissionsOfUser) {
        case (permissionTree, (group, addedPermission)) =>
          mergePermissionIntoPermissionMap(permissionTree, addedPermission, _membersOfGroup.getOrElse(group, Set.empty))
      }
      _writePermissionsOfUser = newWritePermissions.foldLeft(_writePermissionsOfUser) {
        case (permissionTree, (group, addedPermission)) =>
          mergePermissionIntoPermissionMap(permissionTree, addedPermission, _membersOfGroup.getOrElse(group, Set.empty))
      }

      _rules = _rules ++ newRules
    }
  }

  def userJoinedGroups(newUser: Uid, joinedGroups: Set[Group]): Unit =
    this.synchronized {
      // TODO: / NOTE: We currently assume that the group was created before any user can be added to the group.
      assert(joinedGroups.forall(_groups.contains))
      // Same goes for user
      assert(_users.contains(newUser))

      _groupsOfUser = _groupsOfUser.updatedWith(newUser) {
        case Some(existingGroups) => Some(existingGroups ++ joinedGroups)
        case None                 => Some(joinedGroups)
      }

      _membersOfGroup = joinedGroups.foldLeft(_membersOfGroup) { (membersMap, group) =>
        membersMap.updatedWith(group) {
          case Some(existingMembers) => Some(existingMembers + newUser)
          case None                  => Some(Set(newUser))
        }
      }

      val newReadPermissions  = PermissionTree.fromPathSet(_rules.flatMap(_.toPathIfAllowed(joinedGroups, READ)))
      val newWritePermissions = PermissionTree.fromPathSet(_rules.flatMap(_.toPathIfAllowed(joinedGroups, WRITE)))

      val updatedReadPermissions = _readPermissionsOfUser.get(newUser) match
        case Some(existingPermissions) => existingPermissions.merge(newReadPermissions)
        case None                      => newReadPermissions

      val updatedWritePermissions = _writePermissionsOfUser.get(newUser) match
        case Some(existingPermissions) => existingPermissions.merge(newWritePermissions)
        case None                      => newWritePermissions

      _readPermissionsOfUser = _readPermissionsOfUser + (newUser   -> updatedReadPermissions)
      _writePermissionsOfUser = _writePermissionsOfUser + (newUser -> updatedWritePermissions)
    }

}

object PermissionManager {
  @tailrec
  private def mergePermissionIntoPermissionMap(
      permissionMap: Map[Uid, PermissionTree],
      permissionTree: PermissionTree,
      applicableUsers: Set[Uid]
  ): Map[Uid, PermissionTree] = {
    if applicableUsers.isEmpty then permissionMap
    else
      val currentUser             = applicableUsers.head
      val currentPermissionOfUser = permissionMap.getOrElse(applicableUsers.head, PermissionTree.empty)
      mergePermissionIntoPermissionMap(
        permissionMap + (applicableUsers.head -> currentPermissionOfUser.merge(permissionTree)),
        permissionTree,
        applicableUsers.tail
      )
  }

  private def rulesToGroupedPermissionTrees(rules: Set[Rule], operation: Operation): Map[Group, PermissionTree] =
    rules
      .groupBy(_.group)
      .map((group, rules) =>
        group ->
        PermissionTree.fromPathSet(rules.flatMap(_.toPathIfAllowed(Set(group), operation)))
      )

}
