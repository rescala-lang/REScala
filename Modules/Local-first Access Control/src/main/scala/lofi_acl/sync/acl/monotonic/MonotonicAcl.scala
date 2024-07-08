package lofi_acl.sync.acl.monotonic

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import lofi_acl.access.Operation.WRITE
import lofi_acl.access.{Filter, Operation, PermissionTree}
import lofi_acl.crypto.{Ed25519Util, PrivateIdentity, PublicIdentity}
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.{AclDelta, Signature}
import rdts.time.{Dot, Dots}

case class MonotonicAcl[RDT](
    root: PublicIdentity,
    read: Map[PublicIdentity, PermissionTree],
    write: Map[PublicIdentity, PermissionTree]
) {
  def addReadPermissionIfAllowed(
      forPrincipal: PublicIdentity,
      delegatingFrom: PublicIdentity,
      realm: PermissionTree
  )(using Filter[RDT] /* required for minimization of PermissionTree */ ): Option[MonotonicAcl[RDT]] = {
    // Check if delegation is valid
    if realm <= read.getOrElse(delegatingFrom, PermissionTree.empty) || delegatingFrom == root
    then
      Some(MonotonicAcl(
        root,
        addPermissionWithValidationAndMinimization(read, forPrincipal, realm),
        write
      ))
    else
      None
  }

  def addWritePermissionIfAllowed(
      forPrincipal: PublicIdentity,
      delegatingFrom: PublicIdentity,
      realm: PermissionTree
  )(using Filter[RDT] /* required for minimization of PermissionTree */ ): Option[MonotonicAcl[RDT]] = {
    // Check if delegation is valid
    if realm <= write.getOrElse(delegatingFrom, PermissionTree.empty) || delegatingFrom == root
    then
      Some(MonotonicAcl(
        root,
        addPermissionWithValidationAndMinimization(read, forPrincipal, realm),
        addPermissionWithValidationAndMinimization(
          write,
          forPrincipal,
          realm
        ) // Write permission implies read permission
      ))
    else
      None
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
  def createRootOfTrust[RDT](root: PrivateIdentity)(using
      JsonValueCodec[MonotonicAclSyncMessage[RDT]]
  ): AclDelta[RDT] = {
    val delta: AclDelta[RDT] =
      AclDelta[RDT](root.getPublic, PermissionTree.allow, WRITE, Dot(root.getPublic.toUid, 0), Dots.empty, null)
    signDelta(delta, root)
  }

  def signDelta[RDT](unsignedDelta: AclDelta[RDT], localIdentity: PrivateIdentity)(using
      JsonValueCodec[MonotonicAclSyncMessage[RDT]]
  ): AclDelta[RDT] = {
    val encodedMsg = writeToArray(unsignedDelta.asInstanceOf[MonotonicAclSyncMessage[RDT]])
    val signature  = Ed25519Util.sign(encodedMsg, localIdentity.identityKey.getPrivate)
    unsignedDelta.copy(signature = Signature(signature))
  }
}
