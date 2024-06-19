package lofi_acl.sync.acl.monotonic

import lofi_acl.access.{Filter, Operation, PermissionTree}
import lofi_acl.crypto.PublicIdentity

class MonotonicAcl[RDT](
    private val read: Map[PublicIdentity, PermissionTree],
    private val write: Map[PublicIdentity, PermissionTree]
) {
  def addReadPermissionIfAllowed(
      forPrincipal: PublicIdentity,
      delegatingFrom: PublicIdentity,
      realm: PermissionTree
  )(using Filter[RDT] /* required for minimization of PermissionTree */ ): Option[MonotonicAcl[RDT]] = {
    // Check if delegation is valid
    if !(realm <= read.getOrElse(delegatingFrom, PermissionTree.empty))
    then None
    else
      Some(MonotonicAcl(
        addPermissionWithValidationAndMinimization(read, forPrincipal, realm),
        write
      ))
  }

  def addWritePermissionIfAllowed(
      forPrincipal: PublicIdentity,
      delegatingFrom: PublicIdentity,
      realm: PermissionTree
  )(using Filter[RDT] /* required for minimization of PermissionTree */ ): Option[MonotonicAcl[RDT]] = {
    // Check if delegation is valid
    if !(realm <= write.getOrElse(delegatingFrom, PermissionTree.empty))
    then None
    else
      Some(MonotonicAcl(
        addPermissionWithValidationAndMinimization(read, forPrincipal, realm),
        addPermissionWithValidationAndMinimization(
          write,
          forPrincipal,
          realm
        ) // Write permission implies read permission
      ))
  }

  def filterReceivedDelta(delta: RDT, sender: PublicIdentity)(using filter: Filter[RDT]): RDT =
    val permission = write.getOrElse(sender, PermissionTree.empty)
    filter.filter(delta, permission)

  def filterDeltaToSend(delta: RDT, receiver: PublicIdentity)(using filter: Filter[RDT]): RDT =
    val permission = read.getOrElse(receiver, PermissionTree.empty)
    filter.filter(delta, permission)

  def addPermissionIfAllowed(
      forPrincipal: PublicIdentity,
      delegatingFrom: PublicIdentity,
      realm: PermissionTree,
      permissionLevel: Operation
  )(using Filter[RDT] /* required for minimization of PermissionTree */ ): Option[MonotonicAcl[RDT]] =
    permissionLevel match
      case Operation.READ  => addReadPermissionIfAllowed(forPrincipal, delegatingFrom, realm)
      case Operation.WRITE => addWritePermissionIfAllowed(forPrincipal, delegatingFrom, realm)

  def containsPrincipal(publicIdentity: PublicIdentity): Boolean =
    read.contains(publicIdentity)

  private def addPermissionWithValidationAndMinimization(
      mapOfPermissions: Map[PublicIdentity, PermissionTree],
      principal: PublicIdentity,
      permissionToAdd: PermissionTree,
  )(using Filter[RDT]): Map[PublicIdentity, PermissionTree] = {
    mapOfPermissions.updatedWith(principal) { old =>
      val updated = old match
        case None                      => permissionToAdd
        case Some(existingPermissions) => existingPermissions.merge(permissionToAdd)
      // TODO: Probably a good idea to log invalid updates...
      Filter[RDT].validatePermissionTree(Filter[RDT].minimizePermissionTree(updated)).toOption
    }
  }
}

object MonotonicAcl {
  def empty[RDT]: MonotonicAcl[RDT] = MonotonicAcl(Map.empty, Map.empty)
}
