package rescala.fullmv.mirrors

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap, ThreadLocalRandom}

import rescala.fullmv.mirrors.Host.GUID
import rescala.fullmv.sgt.synchronization.{SubsumableLock}

import scala.annotation.tailrec

trait Hosted[R] {
  val host: Host[R]
  def remotelyEquals(obj: Hosted[R]): Boolean = obj.guid == guid
  val guid: Host.GUID
  val hc: Int                  = (guid ^ (guid >>> 32)).toInt
  override def hashCode(): Int = hc
}

object Host {
  type GUID = Long
  val dummyGuid: GUID = 0L
  val DEBUG           = false
}
sealed trait CacheResult[T, +U <: T] { val instance: T }
case class Found[T](instance: T)                extends CacheResult[T, Nothing]
case class Instantiated[T, U <: T](instance: U) extends CacheResult[T, U]
trait Host[T] {
  val dummy: T
  def getInstance(guid: Host.GUID): Option[T]
  def getCachedOrReceiveRemote[U <: T](guid: Host.GUID, instantiateReflection: => U): CacheResult[T, U]
  def dropInstance(guid: GUID, instance: T): Unit
  def createLocal[U <: T](create: Host.GUID => U): U
}

trait HostImpl[T] extends Host[T] {
  val instances: ConcurrentMap[GUID, T]           = new ConcurrentHashMap()
  override def getInstance(guid: GUID): Option[T] = Option(instances.get(guid))
  override def getCachedOrReceiveRemote[U <: T](guid: Host.GUID, instantiateReflection: => U): CacheResult[T, U] = {
    @inline @tailrec def findOrReserveInstance(): T = {
      val found = instances.putIfAbsent(guid, dummy)
      if (found != dummy) {
        found
      } else {
        if (Host.DEBUG) println(s"[${Thread.currentThread().getName}] on $this cache contended for $guid")
        Thread.`yield`()
        findOrReserveInstance()
      }
    }
    val known = findOrReserveInstance()
    if (known != null) {
      if (Host.DEBUG) println(s"[${Thread.currentThread().getName}] on $this cache hit $known")
      Found(known)
    } else {
      if (Host.DEBUG)
        println(s"[${Thread.currentThread().getName}] on $this cache miss for $guid, invoking instantiation...")
      val instance = instantiateReflection
      val replaced = instances.replace(guid, dummy, instance)
      assert(replaced, s"someone stole the dummy placeholder while instantiating remotely received $guid on $this!")
      if (Host.DEBUG) println(s"[${Thread.currentThread().getName}] on $this cached newly instantiated $instance")
      Instantiated(instance)
    }
  }
  override def dropInstance(guid: GUID, instance: T): Unit = {
    val removed = instances.remove(guid, instance)
    assert(removed, s"removal of $instance on $this failed")
    if (Host.DEBUG) println(s"[${Thread.currentThread().getName}] on $this evicted $instance")
  }
  override def createLocal[U <: T](create: GUID => U): U = {
    @inline @tailrec def redoId(): GUID = {
      val id = ThreadLocalRandom.current().nextLong()
      if (id == Host.dummyGuid) {
        redoId()
      } else {
        val known = instances.putIfAbsent(id, dummy)
        if (known == null) id else redoId()
      }
    }
    val guid     = redoId()
    val instance = create(guid)
    val replaced = instances.replace(guid, dummy, instance)
    assert(replaced, s"someone stole the dummy placeholder while creating new instance $guid on $this!")
    if (Host.DEBUG) println(s"[${Thread.currentThread().getName}] on $this created local instance $instance")
    instance
  }
}
trait SubsumableLockHost extends Host[SubsumableLock] {
  def getCachedOrReceiveRemoteWithReference(guid: Host.GUID, remoteProxy: => SubsumableLockProxy): SubsumableLock = {
    @tailrec def retry(): SubsumableLock = {
      getCachedOrReceiveRemote(guid, new SubsumableLockReflection(this, guid, remoteProxy)) match {
        case ins : Instantiated[_, _] =>
          ins.instance
        case Found(instance) =>
          if (instance.tryLocalAddRefs(1)) {
            if (Host.DEBUG)
              println(
                s"[${Thread.currentThread().getName}] $this secured new local ref on cached $instance, dropping connection establishment remote reference."
              )
            remoteProxy.asyncRemoteRefDropped()
            instance
          } else {
            if (Host.DEBUG)
              println(
                s"[${Thread.currentThread().getName}] $this remotely received $instance cache hit was concurrently deallocated; retrying cache lookup."
              )
            retry()
          }
      }
    }
    retry()
  }
}
