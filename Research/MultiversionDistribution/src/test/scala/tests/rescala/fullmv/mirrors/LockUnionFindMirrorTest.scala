package tests.rescala.fullmv.mirrors

import org.scalatest.FunSuite
import rescala.fullmv.FullMVEngine
import rescala.fullmv.TurnPhase
import rescala.fullmv.mirrors.localcloning.FullMVTurnLocalClone
import rescala.fullmv.sgt.synchronization.Successful

import scala.concurrent.duration.Duration
import scala.concurrent.Await

class LockUnionFindMirrorTest extends FunSuite {
  test("single remote subsume and gc works") {
    val host1 = new FullMVEngine(Duration.Zero, "host1")

    val turn1 = host1.newTurn()
    turn1.awaitAndSwitchPhase(TurnPhase.Executing)
    val lock1 = turn1.subsumableLock.get

    val host2 = new FullMVEngine(Duration.Zero, "host2")
    val turn1on2 = FullMVTurnLocalClone(turn1, host2)
    val lock1on2 = Await.result(turn1on2.tryLock(), Duration.Zero)

    assert(lock1on2 === lock1)
    assert((lock1on2 ne lock1) === true)

    assert(lock1.refCount.get === 2) // turn1, lock1on2
    assert(lock1on2.refCount.get === 1) // thread

    val turn2 = host2.newTurn()
    turn2.awaitAndSwitchPhase(TurnPhase.Executing)
    val lock2 = turn2.subsumableLock.get

    assert(Await.result(turn2.trySubsume(lock1on2), Duration.Zero) === Successful)

    assert(lock1.refCount.get === 2) // turn1, lock1on2
    assert(lock1on2.refCount.get === 2) // thread, turn2
    assert(lock2.refCount.get <= 0)

    lock1on2.asyncUnlock()
    assert(lock1.refCount.get === 2) // turn1, lock1on2
    assert(lock1on2.refCount.get === 1) // turn2
    assert(lock2.refCount.get <= 0)

    turn1.awaitAndSwitchPhase(TurnPhase.Completed)
    assert(lock1.refCount.get === 1) // lock1on2
    assert(lock1on2.refCount.get === 1) // turn2
    assert(lock2.refCount.get <= 0)

    turn2.awaitAndSwitchPhase(TurnPhase.Completed)
    assert(lock1.refCount.get <= 0)
    assert(lock1on2.refCount.get <= 0)
    assert(lock2.refCount.get <= 0)
  }
}
