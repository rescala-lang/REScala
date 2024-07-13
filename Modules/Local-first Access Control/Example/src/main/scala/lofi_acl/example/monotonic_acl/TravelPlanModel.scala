package lofi_acl.example.monotonic_acl

import lofi_acl.access.Operation.{READ, WRITE}
import lofi_acl.access.PermissionTree
import lofi_acl.collections.DeltaMapWithPrefix
import lofi_acl.crypto.{Ed25519Util, IdentityFactory, PrivateIdentity, PublicIdentity}
import lofi_acl.example.travelplanner.TravelPlan
import lofi_acl.example.travelplanner.TravelPlan.given
import lofi_acl.sync.JsoniterCodecs.messageJsonCodec
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.AclDelta
import lofi_acl.sync.acl.monotonic.{MonotonicAcl, SyncWithMonotonicAcl}
import rdts.base.LocalUid
import rdts.datatypes.LastWriterWins
import scalafx.application.Platform
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.ExecutionContext.global

class TravelPlanModel(
    private val localIdentity: PrivateIdentity,
    rootOfTrust: PublicIdentity,
    initialAclDeltas: List[AclDelta[TravelPlan]] = List.empty
) {
  private given localUid: LocalUid = LocalUid(localIdentity.getPublic.toUid)

  def state: TravelPlan                    = sync.state
  def currentAcl: MonotonicAcl[TravelPlan] = sync.currentAcl

  private val sync: SyncWithMonotonicAcl[TravelPlan] =
    SyncWithMonotonicAcl[TravelPlan](
      localIdentity,
      rootOfTrust,
      initialAclDeltas,
      DeltaMapWithPrefix.empty,
      delta => Platform.runLater(deltaReceived(delta))
    )
  sync.start()
  Runtime.getRuntime.addShutdownHook(new Thread(() => sync.stop()))

  def createInvitation: Invitation =
    Invitation(rootOfTrust, Ed25519Util.generateNewKeyPair, localIdentity.getPublic, sync.connectionString)

  def grantPermission(
      affectedUser: PublicIdentity,
      readPermissions: PermissionTree,
      writePermissions: PermissionTree
  ): Unit = {
    sync.grantPermissions(affectedUser, readPermissions, READ)
    sync.grantPermissions(affectedUser, writePermissions, WRITE)
  }

  def addConnection(remoteUser: PublicIdentity, address: String): Unit = {
    sync.connect(remoteUser, address)
  }

  def changeTitle(newTitle: String): Unit = {
    mutateRdt(_.changeTitle(newTitle))
  }

  def addBucketListEntry(text: String): Unit = {
    mutateRdt(_.addBucketListEntry(text))
  }

  def setBucketListEntryText(bucketListId: String, text: String): Unit = {
    mutateRdt(_.setBucketListEntryText(bucketListId, text))
  }

  def addExpense(description: String, amount: Float): Unit = {
    mutateRdt(_.addExpense(description, amount))
  }

  def setExpenseAmount(expenseId: String, amount: Float): Unit = {
    mutateRdt(_.setExpenseAmount(expenseId, amount))
  }

  def setExpenseDescription(expenseId: String, description: String): Unit = {
    mutateRdt(_.setExpenseDescription(expenseId, description))
  }

  def setExpenseComment(expenseId: String, comment: String): Unit = {
    mutateRdt(_.setExpenseComment(expenseId, comment))
  }

  private def mutateRdt(mutator: TravelPlan => TravelPlan): Unit = {
    global.execute { () =>
      sync.mutateRdt(mutator)
    }
  }

  val title: StringProperty                   = StringProperty(state.title.read)
  val bucketListIds: ObservableBuffer[String] = ObservableBuffer.from(state.bucketList.data.inner.keySet)
  private var bucketListIdSet: Set[String]    = bucketListIds.toSet
  val bucketListProperties: AtomicReference[Map[String, StringProperty]] =
    AtomicReference(state.bucketList.data.inner.map((id, lww) => id -> StringProperty(lww.value.read)))

  private def deltaReceived(delta: TravelPlan): Unit = {
    val newTravelPlan = state
    if !delta.title.isEmpty then
      title.value = newTravelPlan.title.read

    val bucketListEntriesInDelta = delta.bucketList.data.inner
    if bucketListEntriesInDelta.nonEmpty then
      val newIds = bucketListEntriesInDelta.filter((id, entry) => !bucketListIdSet.contains(id)).keySet
      bucketListProperties.updateAndGet(oldProps =>
        val newProps = oldProps ++ newIds.map(id => id -> StringProperty(""))
        bucketListEntriesInDelta.foreach { (id, entry) =>
          newProps(id).value = entry.value.read
        }
        newProps
      )
      bucketListIds.addAll(newIds)
      bucketListIdSet = bucketListIdSet ++ newIds
  }
}

object TravelPlanModel {
  def createNewDocument: TravelPlanModel = {
    val privateId = IdentityFactory.createNewIdentity
    val aclDelta  = MonotonicAcl.createRootOfTrust[TravelPlan](privateId)
    val model     = TravelPlanModel(privateId, privateId.getPublic, List(aclDelta))

    model.addBucketListEntry("Porto")
    model.addBucketListEntry("Lisbon")
    model.addBucketListEntry("Faro")
    model.addExpense("Ice Cream", 3.14)

    model
  }

  def joinDocument(inviteString: String): TravelPlanModel = {
    val invite          = Invitation.decode(inviteString)
    val identity        = IdentityFactory.fromIdentityKey(invite.identityKey)
    val travelPlanModel = TravelPlanModel(identity, invite.rootOfTrust, List.empty)
    travelPlanModel.addConnection(invite.inviter, invite.joinAddress)
    travelPlanModel
  }
}
