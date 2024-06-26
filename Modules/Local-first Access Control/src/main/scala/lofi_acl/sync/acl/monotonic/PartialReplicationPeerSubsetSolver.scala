package lofi_acl.sync.acl.monotonic

import lofi_acl.access.PermissionTree
import lofi_acl.crypto.PublicIdentity

import scala.annotation.tailrec
import scala.util.Random

object PartialReplicationPeerSubsetSolver {
  private val random = Random()

  // Only allows reconstruction if other replicas store delta.
  def randomSubsetThatAllowsReconstruction(
      contributingPermissions: Map[PublicIdentity, PermissionTree],
      requiredPermissions: PermissionTree
  ): (Set[PublicIdentity], PermissionTree) = {
    if contributingPermissions.isEmpty
    then (Set.empty, PermissionTree.empty)
    else
      val arr = contributingPermissions.toArray
      randomSubsetThatAllowsReconstruction(arr, arr.length, Set.empty, PermissionTree.empty, requiredPermissions)
  }

  @tailrec
  private def randomSubsetThatAllowsReconstruction(
      contributingPermissions: Array[(PublicIdentity, PermissionTree)],
      length: Int,
      includedRemotes: Set[PublicIdentity],
      progress: PermissionTree,
      goal: PermissionTree
  ): (Set[PublicIdentity], PermissionTree) = {
    if length == 0 || goal <= progress
    then return (includedRemotes, progress)

    val pickedReplicaIdx                      = random.nextInt(length)
    val (pickedReplica, permissionsOfReplica) = contributingPermissions(pickedReplicaIdx)

    // Move last element of array to position of picked element. This ensures that it is not picked again. Note that we
    // resize array, so moved element in original position is also not picked again. Ensures monotonically shrinking
    // array and therefore termination.
    if length != 1
    then contributingPermissions(pickedReplicaIdx) = contributingPermissions(length - 1)

    val progressWithPermissionsOfReplica = progress.merge(permissionsOfReplica)
    if progress == progressWithPermissionsOfReplica // No progress, don't pick new replica
    then randomSubsetThatAllowsReconstruction(contributingPermissions, length - 1, includedRemotes, progress, goal)
    else // Progress with picked replica, so include replica
      randomSubsetThatAllowsReconstruction(
        contributingPermissions,
        length - 1,
        includedRemotes + pickedReplica,
        progressWithPermissionsOfReplica,
        goal
      )
  }

}
