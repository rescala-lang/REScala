package lofi_acl.sync.acl.monotonic

import crypto.PublicIdentity
import lofi_acl.access.PermissionTree

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
      val (replicas, achievedPermission) =
        randomSubsetThatAllowsReconstruction(arr, arr.length, Set.empty, PermissionTree.empty, requiredPermissions)
      // Drop all replicas whose permissions are <= than another replica from the set.
      // This is still not necessarily a minimal set (e.g., A can write a1,a2; B can write a1;b1; C can write a2;b1).
      val maximalReplicas = replicas.filter(r1 =>
        replicas.forall(r2 => (r2 `eq` r1) || contributingPermissions(r1) <= contributingPermissions(r2))
      )
      (maximalReplicas, achievedPermission)
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
