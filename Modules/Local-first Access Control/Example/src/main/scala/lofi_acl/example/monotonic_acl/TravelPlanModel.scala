package lofi_acl.example.monotonic_acl

import lofi_acl.collections.DeltaMapWithPrefix
import lofi_acl.crypto.{Ed25519Util, IdentityFactory, PrivateIdentity, PublicIdentity}
import lofi_acl.example.travelplanner.TravelPlan
import lofi_acl.sync.JsoniterCodecs.messageJsonCodec
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.AclDelta
import lofi_acl.sync.acl.monotonic.{MonotonicAcl, SyncWithMonotonicAcl}

import java.util.concurrent.atomic.AtomicReference

class TravelPlanModel(
    private val localIdentity: PrivateIdentity,
    rootOfTrust: PublicIdentity,
    initialAclDeltas: List[AclDelta[TravelPlan]] = List.empty
) {
  private val crdt = AtomicReference[TravelPlan]()

  private val sync: SyncWithMonotonicAcl[TravelPlan] =
    SyncWithMonotonicAcl[TravelPlan](
      localIdentity,
      rootOfTrust,
      initialAclDeltas,
      DeltaMapWithPrefix.empty
    )
  sync.start()

  def createInvitation: Invitation =
    Invitation(rootOfTrust, Ed25519Util.generateNewKeyPair, localIdentity.getPublic, sync.connectionString)

  def shutdown(): Unit = {
    sync.stop()
  }

  def addConnection(remoteUser: PublicIdentity, address: String): Unit = {
    sync.connect(remoteUser, address)
  }
}

object TravelPlanModel {
  def createNewDocument: TravelPlanModel = {
    val privateId = IdentityFactory.createNewIdentity
    MonotonicAcl.createRootOfTrust[TravelPlan](privateId)
    TravelPlanModel(privateId, privateId.getPublic, List.empty)
  }

  def joinDocument(inviteString: String): TravelPlanModel = {
    val invite          = Invitation.decode(inviteString)
    val identity        = IdentityFactory.fromIdentityKey(invite.identityKey)
    val travelPlanModel = TravelPlanModel(identity, invite.rootOfTrust, List.empty)
    travelPlanModel.addConnection(invite.inviter, invite.joinAddress)
    travelPlanModel
  }
}
