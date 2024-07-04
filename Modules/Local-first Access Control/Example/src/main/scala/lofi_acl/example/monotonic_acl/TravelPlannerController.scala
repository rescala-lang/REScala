package lofi_acl.example.monotonic_acl

import lofi_acl.collections.DeltaMapWithPrefix
import lofi_acl.crypto.{IdentityFactory, PrivateIdentity}
import lofi_acl.example.travelplanner.TravelPlan
import lofi_acl.sync.acl.monotonic.SyncWithMonotonicAcl

import java.util.concurrent.atomic.AtomicReference

class TravelPlannerController private (isNewDocument: Boolean) {
  private val privateId: PrivateIdentity = IdentityFactory.createNewIdentity
  private val sync: SyncWithMonotonicAcl[TravelPlan] = SyncWithMonotonicAcl[TravelPlan](
    privateId,
    List.empty,
    DeltaMapWithPrefix.empty
  )
  sync.start()

  println(sync.connectionString)

  def connect(connectionString: String): Unit = {
    sync.connect(connectionString)
  }

  def shutdown(): Unit = {
    sync.stop()
  }

  private val crdt = AtomicReference[TravelPlan]()
}

object TravelPlannerController {
  def forNewDocument: TravelPlannerController = TravelPlannerController(true)

  def forExistingDocument(connectionString: String): TravelPlannerController =
    val controller = TravelPlannerController(false)
    controller.connect(connectionString)
    controller
}
