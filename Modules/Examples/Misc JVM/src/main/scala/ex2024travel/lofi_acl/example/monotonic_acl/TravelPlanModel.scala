package ex2024travel.lofi_acl.example.monotonic_acl

import channels.tls.{IdentityFactory, PrivateIdentity}
import crypto.{Ed25519Util, PublicIdentity}
import lofi_acl.access.Operation.{READ, WRITE}
import lofi_acl.access.PermissionTree
import lofi_acl.collections.DeltaMapWithPrefix
import ex2024travel.lofi_acl.example.travelplanner.TravelPlan
import ex2024travel.lofi_acl.example.travelplanner.TravelPlan.given
import lofi_acl.sync.JsoniterCodecs.messageJsonCodec
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.AclDelta
import lofi_acl.sync.acl.monotonic.{MonotonicAcl, SyncWithMonotonicAcl}
import rdts.base.{LocalUid, Uid}
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
  val publicId: PublicIdentity     = localIdentity.getPublic
  private given localUid: LocalUid = LocalUid(Uid(publicId.id))

  def state: TravelPlan                    = sync.state
  def currentAcl: MonotonicAcl[TravelPlan] = sync.currentAcl

  private val sync = SyncWithMonotonicAcl[TravelPlan](
    localIdentity,
    rootOfTrust,
    initialAclDeltas,
    DeltaMapWithPrefix.empty,
    delta => Platform.runLater(deltaReceived(delta))
  )
  sync.start()
  Runtime.getRuntime.addShutdownHook(new Thread(() => sync.stop()))

  def createInvitation: Invitation =
    Invitation(rootOfTrust, Ed25519Util.generateNewKeyPair, publicId, sync.connectionString)

  def grantPermission(
      affectedUser: PublicIdentity,
      readPermissions: PermissionTree,
      writePermissions: PermissionTree
  ): Unit = {
    sync.grantPermissions(affectedUser, readPermissions, READ)
    if !writePermissions.isEmpty
    then sync.grantPermissions(affectedUser, writePermissions, WRITE)
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

  def addExpense(description: String, amount: String): Unit = {
    mutateRdt(_.addExpense(description, amount))
  }

  def setExpenseAmount(expenseId: String, amount: String): Unit = {
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

  val title: StringProperty                      = StringProperty(state.title.read)
  val bucketListIdList: ObservableBuffer[String] = ObservableBuffer.from(state.bucketList.data.inner.keySet)
  private var bucketListIdSet: Set[String]       = bucketListIdList.toSet
  val bucketListProperties: AtomicReference[Map[String, StringProperty]] =
    AtomicReference(state.bucketList.data.inner.map((id, lww) => id -> StringProperty(lww.value.read)))
  val expenseIdList: ObservableBuffer[String] = ObservableBuffer.from(state.bucketList.data.inner.keySet)
  private var expenseIdSet: Set[String]       = expenseIdList.toSet
  val expenseListProperties: AtomicReference[Map[String, (StringProperty, StringProperty, StringProperty)]] =
    AtomicReference(state.expenses.data.inner.map((id, orMapEntry) => {
      val expense = orMapEntry.value
      id -> (
        StringProperty(expense.description.read.getOrElse("")),
        StringProperty(expense.amount.read.getOrElse("0.00 €")),
        StringProperty(expense.comment.read.getOrElse(""))
      )
    }))

  private def deltaReceived(delta: TravelPlan): Unit = {
    val newTravelPlan = state
    // Title
    if !delta.title.isEmpty then
      title.value = newTravelPlan.title.read

    // Bucket List Entries
    val bucketListEntriesInDelta = delta.bucketList.data.inner
    if bucketListEntriesInDelta.nonEmpty then
      val newIds = bucketListEntriesInDelta.keySet.diff(bucketListIdSet)
      val props = bucketListProperties.updateAndGet(oldProps =>
        oldProps ++ newIds.map(id => id -> StringProperty(""))
      )
      bucketListEntriesInDelta.foreach { (id, entry) =>
        props(id).value = entry.value.read
      }
      bucketListIdList.addAll(newIds)
      bucketListIdSet = bucketListIdSet ++ newIds

    // Expenses
    val expenseEntriesInDelta = delta.expenses.data.inner
    if expenseEntriesInDelta.nonEmpty then
      val newIds = expenseEntriesInDelta.keySet.diff(expenseIdSet)
      val props = expenseListProperties.updateAndGet(oldProps =>
        oldProps ++ newIds.map(id =>
          id -> (StringProperty(""), StringProperty("0.00 €"), StringProperty(""))
        )
      )
      expenseEntriesInDelta.foreach { (id, _) =>
        val (description, amount, comment) = props(id)
        // TODO: Would fail on removal of entries
        val curValue = newTravelPlan.expenses.data.get(id).get.value
        description.value = curValue.description.read.getOrElse("")
        amount.value = curValue.amount.read.getOrElse("0.00 €")
        comment.value = curValue.comment.read.getOrElse("")

      }
      expenseIdList.addAll(newIds)
      expenseIdSet = expenseIdSet ++ newIds
  }
}

object TravelPlanModel {
  def createNewDocument: TravelPlanModel = {
    val privateId = IdentityFactory.createNewIdentity
    val aclDelta  = MonotonicAcl.createRootOfTrust[TravelPlan](privateId)
    val model     = TravelPlanModel(privateId, privateId.getPublic, List(aclDelta))

    model.changeTitle("Portugal Trip")
    model.addBucketListEntry("Porto")
    model.addBucketListEntry("Lisbon")
    model.addBucketListEntry("Faro")
    model.addExpense("Ice Cream", "3.14 €")

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
