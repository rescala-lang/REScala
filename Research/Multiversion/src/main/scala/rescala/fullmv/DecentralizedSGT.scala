package rescala.fullmv

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object DecentralizedSGT extends SerializationGraphTracking[FullMVTurn] {
  // look mom, no centralized state!

  override def getOrder(defender: FullMVTurn, contender: FullMVTurn): PartialOrderResult = {
    assert(defender != contender, s"$defender compared with itself..?")
    assert(defender.host == contender.host, s"$defender and $contender are of different hosts ${defender.host} and ${contender.host}")
    assert(defender.phase > TurnPhase.Initialized, s"$defender is not started and should thus not be involved in any operations")
    assert(contender.phase > TurnPhase.Initialized, s"$contender is not started and should thus not be involved in any operations")
    assert(contender.phase < TurnPhase.Completed, s"$contender cannot be a searcher (already completed).")
    if(defender.phase == TurnPhase.Completed || contender.isTransitivePredecessor(defender)) {
      FirstFirst
    } else if (defender.isTransitivePredecessor(contender)) {
      SecondFirst
    } else {
      Unordered
    }
  }

  override def ensureOrder(defender: FullMVTurn, contender: FullMVTurn, timeout: Duration): OrderResult = {
    assert(defender != contender, s"cannot establish order between equal defender $defender and contender $contender")
    assert(defender.host == contender.host, s"$defender and $contender are of different hosts ${defender.host} and ${contender.host}")
    assert(defender.phase > TurnPhase.Initialized, s"$defender is not started and should thus not be involved in any operations")
    assert(contender.phase > TurnPhase.Initialized, s"$contender is not started and should thus not be involved in any operations")
    assert(contender.phase < TurnPhase.Completed, s"$contender cannot be a contender (already completed).")
    assert(Await.result(contender.getLockedRoot, timeout).isDefined, s"$contender is not locked")
    if(defender.phase == TurnPhase.Completed) {
      FirstFirst
    } else{
      assert(Await.result(defender.getLockedRoot, timeout).isDefined, s"$defender is not locked")
      assert(Await.result(defender.getLockedRoot, timeout).get == Await.result(contender.getLockedRoot, timeout).get, s"$defender and $contender not merged (roots ${Await.result(defender.getLockedRoot, timeout).get} and ${Await.result(contender.getLockedRoot, timeout).get})")
      if(contender.isTransitivePredecessor(defender)) {
        FirstFirst
      } else if (defender.isTransitivePredecessor(contender)) {
        SecondFirst
      } else {
        // unordered nested acquisition of two monitors here is safe against deadlocks because the turns' locks
        // (see assertions) ensure that only a single thread at a time will ever attempt to do so.

        val contenderBundle = contender.acquirePhaseLockAndGetEstablishmentBundle()
        val defenderBundle = defender.acquirePhaseLockAndGetEstablishmentBundle()
        val (contenderPhase, contenderPredecessorsSpanningTree) = Await.result(contenderBundle, timeout)
        val (defenderPhase, defenderPredecessorsSpanningTree) = Await.result(defenderBundle, timeout)
        if (defenderPhase < contenderPhase) {
          Await.result(defender.addPredecessorAndReleasePhaseLock(contenderPredecessorsSpanningTree), timeout)
          contender.asyncReleasePhaseLock()
          SecondFirst
        } else {
          Await.result(contender.addPredecessorAndReleasePhaseLock(defenderPredecessorsSpanningTree), timeout)
          defender.asyncReleasePhaseLock()
          FirstFirst
        }
      }
    }
  }
}
