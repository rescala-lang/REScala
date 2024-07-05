package lofi_acl.example.monotonic_acl

import lofi_acl.collections.DeltaMapWithPrefix
import lofi_acl.crypto.{IdentityFactory, PrivateIdentity}
import lofi_acl.example.travelplanner.TravelPlan
import lofi_acl.sync.acl.monotonic.SyncWithMonotonicAcl

import java.util.concurrent.atomic.AtomicReference

class TravelPlanModel(joinedDocument: Option[String]) {
  private val privateId: PrivateIdentity = IdentityFactory.createNewIdentity

  private val crdt = AtomicReference[TravelPlan]()

  private val sync: SyncWithMonotonicAcl[TravelPlan] = SyncWithMonotonicAcl[TravelPlan](
    privateId,
    List.empty,
    DeltaMapWithPrefix.empty
  )
  sync.start()
  joinedDocument.foreach(sync.connect)

  def connectionString: Option[String] = Some(sync.connectionString)

  def shutdown(): Unit = {
    sync.stop()
  }
}
