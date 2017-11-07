package rescala.fullmv.mirrors

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap, ThreadLocalRandom}

import rescala.fullmv.FullMVTurn
import rescala.fullmv.mirrors.Host.GUID
import rescala.fullmv.sgt.synchronization.{SubsumableLock, SubsumableLockImpl}

import scala.annotation.tailrec

trait Hosted {
  val host: Host[_]
  val guid: Host.GUID
  override def equals(obj: scala.Any): Boolean = obj.isInstanceOf[Hosted] && obj.asInstanceOf[Hosted].guid == guid
  val hc = (guid ^ (guid >>> 32)).toInt
  override def hashCode(): Int = hc
}

object Host {
  type GUID = Long
  val dummyGuid: GUID = 0L
}
trait Host[T] {
  def getInstance(guid: Host.GUID): Option[T]
  def getCachedOrReceiveRemote(guid: Host.GUID, instantiateReflection: (T => Unit) => T, wasFound: => Unit): T
  def dropInstance(guid: GUID, instance: T): Unit
  def createLocal[U <: T](create: Host.GUID => U): U
}

trait HostImpl[T] extends Host[T] {
  val instances: ConcurrentMap[GUID, T] = new ConcurrentHashMap()
  val dummy: T

  override def getInstance(guid: GUID): Option[T] = Option(instances.get(guid))
  override def getCachedOrReceiveRemote(guid: Host.GUID, instantiateReflection: (T => Unit) => T, wasFound: => Unit): T = {
    @inline @tailrec def findOrReserveInstance(): T = {
      val found = instances.putIfAbsent(guid, dummy)
      if(found != dummy) {
        found
      } else {
        Thread.`yield`()
        findOrReserveInstance()
      }
    }
    val known = findOrReserveInstance()
    if(known != null) {
      wasFound
      known
    } else {
      instantiateReflection { instance: T =>
        val replaced = instances.replace(guid, dummy, instance)
        assert(replaced, s"someone stole the dummy placeholder while instantiating remotely received $guid on $this!")
      }
    }
  }
  override def dropInstance(guid: GUID, instance: T): Unit = {
    val removed = instances.remove(guid, instance)
    assert(removed, s"removal of $instance on $this failed")
  }
  override def createLocal[U <: T](create: GUID => U): U = {
    @inline @tailrec def redoId(): GUID = {
      val id = ThreadLocalRandom.current().nextLong()
      if(id == Host.dummyGuid) {
        redoId()
      } else {
        val known = instances.putIfAbsent(id, dummy)
        if(known == null) id else redoId()
      }
    }
    val guid = redoId()
    val instance = create(guid)
    val replaced = instances.replace(guid, dummy, instance)
    assert(replaced, s"someone stole the dummy placeholder while creating new instance $guid on $this!")
    instance
  }
}
trait SubsumableLockHost extends Host[SubsumableLock]

class SubsumableLockHostImpl extends SubsumableLockHost with HostImpl[SubsumableLock] {
  override val dummy = new SubsumableLockImpl(this, Host.dummyGuid)
  def newLock(): SubsumableLockImpl = createLocal(new SubsumableLockImpl(this, _))
}
trait FullMVTurnHost extends Host[FullMVTurn] {
  val lockHost: SubsumableLockHost
}
