package tests.rescala.fullmv.mirror

import org.scalatest.FunSuite
import rescala.fullmv.FullMVEngine
import rescala.fullmv.TurnPhase
import rescala.fullmv.mirrors.localcloning.FullMVTurnLocalClone

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, TimeoutException}

class LockUnionFindMirrorTest extends FunSuite {
  test("single remote subsume and gc works") {
    val host1 = new FullMVEngine(Duration.Zero, "host1")

    val turn1 = host1.newTurn()
    turn1.awaitAndSwitchPhase(TurnPhase.Executing)
    val lock1 = turn1.subsumableLock.get

    val host2 = new FullMVEngine(Duration.Zero, "host2")
    val turn1on2 = FullMVTurnLocalClone(turn1, host2)
    val lock1on2 = Await.result(turn1on2.lock(), Duration.Zero)

    assert(lock1on2 === lock1)
    assert((lock1on2 ne lock1) === true)

    assert(lock1.refCount.get === 2)
    assert(lock1on2.refCount.get === 1)

    val turn2 = host2.newTurn()
    turn2.awaitAndSwitchPhase(TurnPhase.Executing)
    val lock2 = turn2.subsumableLock.get

    assert(Await.result(lock2.remoteTrySubsume(lock1on2), Duration.Zero) === true)

    assert(lock1.refCount.get === 2)
    assert(lock1on2.refCount.get === 2)

    // TODO this same call *should* be necessary if lock was called on a local turn!
    lock1on2.asyncRemoteRefDropped()
    assert(lock1.refCount.get === 2)
    assert(lock1on2.refCount.get === 1)

  }
}
