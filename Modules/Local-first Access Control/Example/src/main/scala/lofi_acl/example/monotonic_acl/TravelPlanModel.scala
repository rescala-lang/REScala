package lofi_acl.example.monotonic_acl

import com.softwaremill.quicklens.*
import lofi_acl.access.Operation.{READ, WRITE}
import lofi_acl.access.PermissionTree
import lofi_acl.collections.DeltaMapWithPrefix
import lofi_acl.crypto.{Ed25519Util, IdentityFactory, PrivateIdentity, PublicIdentity}
import lofi_acl.example.travelplanner.TravelPlan.given
import lofi_acl.example.travelplanner.{Expense, TravelPlan}
import lofi_acl.sync.JsoniterCodecs.messageJsonCodec
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.AclDelta
import lofi_acl.sync.acl.monotonic.{MonotonicAcl, SyncWithMonotonicAcl}
import rdts.base.LocalUid
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.datatypes.contextual.ObserveRemoveMap.Entry
import rdts.time.Dots
import scalafx.application.Platform
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer

import java.util.Base64
import java.util.concurrent.atomic.AtomicReference
import scala.util.Random

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

  val bucketListIds: ObservableBuffer[String] = ObservableBuffer.from(state.bucketList.data.inner.keySet)
  val bucketListProperties: AtomicReference[Map[String, StringProperty]] =
    AtomicReference(state.bucketList.data.inner.map((id, lww) => id -> StringProperty(lww.value.read)))

  // TODO: Bind to crdt
  val title: StringProperty = StringProperty("")
  def changeTitle(newTitle: String): Unit = {
    sync.mutateRdt { travelPlan =>
      val delta = travelPlan.deltaModify(_.title).using(_.write(newTitle))
      println(delta)
      delta
    }
  }

  private val base64Encoder = Base64.getEncoder
  def addBucketListEntry(text: String): Unit = {
    val key = base64Encoder.encodeToString(Random.nextBytes(4))
    sync.mutateRdt { travelPlan =>
      travelPlan.deltaModify(_.bucketList).using {
        _.mod { (context: Dots) ?=> (ormap: ObserveRemoveMap[String, Entry[LastWriterWins[String]]]) =>
          val nextDot = Dots.single(context.nextDot(localUid.uid))
          ormap.transformPlain(key) {
            case None => Some(Entry(nextDot, LastWriterWins.now(text)))
            case _    => ???
          }
        }
      }
    }
  }

  def setBucketListEntryText(key: String, text: String): Unit = {
    sync.mutateRdt { travelPlan =>
      travelPlan.deltaModify(_.bucketList).using {
        _.mod { (context: Dots) ?=> (ormap: ObserveRemoveMap[String, Entry[LastWriterWins[String]]]) =>
          val nextDot = Dots.single(context.nextDot(localUid.uid))
          ormap.transformPlain(key) {
            case Some(prior) => Some(Entry(nextDot, prior.value.write(text)))
            case None        => Some(Entry(nextDot, LastWriterWins.now(text)))
          }
        }
      }
    }
  }

  def addExpense(description: String, amount: Float): Unit = {
    val key = base64Encoder.encodeToString(Random.nextBytes(4))
    sync.mutateRdt { travelPlan =>
      travelPlan.deltaModify(_.expenses).using {
        _.mod { (context: Dots) ?=> (ormap: ObserveRemoveMap[String, Entry[Expense]]) =>
          val nextDot = Dots.single(context.nextDot(localUid.uid))
          val expense =
            Expense(LastWriterWins.now(Some(description)), LastWriterWins.now(Some(amount)), LastWriterWins.now(None))
          ormap.transformPlain(key) {
            case None => Some(Entry(nextDot, expense))
            case _    => ???
          }
        }
      }
    }
  }

  def setExpenseAmount(key: String, amount: Float): Unit = {
    sync.mutateRdt { travelPlan =>
      travelPlan.deltaModify(_.expenses).using {
        _.mod { (context: Dots) ?=> (ormap: ObserveRemoveMap[String, Entry[Expense]]) =>
          val nextDot = Dots.single(context.nextDot(localUid.uid))
          ormap.transformPlain(key) {
            case Some(prior: Entry[Expense]) =>
              Some(Entry(nextDot, prior.value.deltaModify(_.amount).using(_.write(Some(amount)))))
            case None => ???
          }
        }
      }
    }
  }

  def setExpenseDescription(key: String, description: String): Unit = {
    sync.mutateRdt { travelPlan =>
      travelPlan.deltaModify(_.expenses).using {
        _.mod { (context: Dots) ?=> (ormap: ObserveRemoveMap[String, Entry[Expense]]) =>
          val nextDot = Dots.single(context.nextDot(localUid.uid))
          ormap.transformPlain(key) {
            case Some(prior: Entry[Expense]) =>
              Some(Entry(nextDot, prior.value.deltaModify(_.description).using(_.write(Some(description)))))
            case None => ???
          }
        }
      }
    }
  }

  def setExpenseComment(key: String, comment: String): Unit = {
    val commentValue = if comment.isEmpty then None else Some(comment)
    sync.mutateRdt { travelPlan =>
      travelPlan.deltaModify(_.expenses).using {
        _.mod { (context: Dots) ?=> (ormap: ObserveRemoveMap[String, Entry[Expense]]) =>
          val nextDot = Dots.single(context.nextDot(localUid.uid))
          ormap.transformPlain(key) {
            case Some(prior: Entry[Expense]) =>
              Some(Entry(nextDot, prior.value.deltaModify(_.comment).using(_.write(commentValue))))
            case None => ???
          }
        }
      }
    }
  }

  private def deltaReceived(delta: TravelPlan): Unit = {
    val newTravelPlan = state
    if !delta.title.isEmpty
    then title.value = newTravelPlan.title.read
    println(s"Delta received: $delta")
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
