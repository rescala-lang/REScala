package lofi_acl.example.monotonic_acl

import lofi_acl.collections.DeltaMapWithPrefix
import lofi_acl.crypto.{Ed25519Util, IdentityFactory, PrivateIdentity, PublicIdentity}
import lofi_acl.example.travelplanner.{Expense, TravelPlan}
import lofi_acl.sync.JsoniterCodecs.messageJsonCodec
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.AclDelta
import lofi_acl.sync.acl.monotonic.{MonotonicAcl, SyncWithMonotonicAcl}
import rdts.base.LocalUid
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.time.CausalTime
import scalafx.beans.property.StringProperty

import java.util.concurrent.atomic.AtomicReference

class TravelPlanModel(
    private val localIdentity: PrivateIdentity,
    rootOfTrust: PublicIdentity,
    initialAclDeltas: List[AclDelta[TravelPlan]] = List.empty
) {
  private given localUid: LocalUid = LocalUid(localIdentity.getPublic.toUid)
  private val crdt = AtomicReference[TravelPlan]({
    var fakeRdt = TravelPlan.empty
    fakeRdt = fakeRdt.merge(
      TravelPlan.empty.copy(bucketList =
        fakeRdt.bucketList.mod(_.update("1", LastWriterWins(CausalTime.now(), "A")))
      )
    )
    fakeRdt = fakeRdt.merge(
      TravelPlan.empty.copy(bucketList =
        fakeRdt.bucketList.mod(_.update("2", LastWriterWins(CausalTime.now(), "B")))
      )
    )
    fakeRdt = fakeRdt.merge(
      TravelPlan.empty.copy(expenses =
        fakeRdt.expenses.mod(_.update(
          "3",
          Expense(LastWriterWins.now(Some("Hello World!")), LastWriterWins.empty, ObserveRemoveMap.empty)
        ))
      )
    )
    fakeRdt
  })

  def state: TravelPlan                    = crdt.get()
  def currentAcl: MonotonicAcl[TravelPlan] = sync.currentAcl

  private val sync: SyncWithMonotonicAcl[TravelPlan] =
    SyncWithMonotonicAcl[TravelPlan](
      localIdentity,
      rootOfTrust,
      initialAclDeltas,
      DeltaMapWithPrefix.empty
    )
  sync.start()
  Runtime.getRuntime.addShutdownHook(new Thread(() => sync.stop()))

  def createInvitation: Invitation =
    Invitation(rootOfTrust, Ed25519Util.generateNewKeyPair, localIdentity.getPublic, sync.connectionString)

  def addConnection(remoteUser: PublicIdentity, address: String): Unit = {
    sync.connect(remoteUser, address)
  }

  // TODO: Bind to crdt
  val title: StringProperty = StringProperty("New Travel Plan")
}

object TravelPlanModel {
  def createNewDocument: TravelPlanModel = {
    val privateId = IdentityFactory.createNewIdentity
    val aclDelta  = MonotonicAcl.createRootOfTrust[TravelPlan](privateId)
    TravelPlanModel(privateId, privateId.getPublic, List(aclDelta))
  }

  def joinDocument(inviteString: String): TravelPlanModel = {
    val invite          = Invitation.decode(inviteString)
    val identity        = IdentityFactory.fromIdentityKey(invite.identityKey)
    val travelPlanModel = TravelPlanModel(identity, invite.rootOfTrust, List.empty)
    travelPlanModel.addConnection(invite.inviter, invite.joinAddress)
    travelPlanModel
  }
}
