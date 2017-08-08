package rescala.fullmv

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object DecentralizedSGT extends SerializationGraphTracking[FullMVTurn] {
  // look mom, no centralized state!

  override def getOrder(defender: FullMVTurn, contender: FullMVTurn): PartialOrderResult = {
    assert(defender != contender, s"$defender compared with itself..?")
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

  override def ensureOrder(defender: FullMVTurn, contender: FullMVTurn): OrderResult = {
    assert(defender != contender, s"cannot establish order between equal defender $defender and contender $contender")
    assert(defender.phase > TurnPhase.Initialized, s"$defender is not started and should thus not be involved in any operations")
    assert(contender.phase > TurnPhase.Initialized, s"$contender is not started and should thus not be involved in any operations")
    assert(contender.phase < TurnPhase.Completed, s"$contender cannot be a contender (already completed).")
    assert(contender.getLockedRoot.isDefined, s"$contender is not locked")
    if(defender.phase == TurnPhase.Completed) {
      FirstFirst
    } else{
      assert(defender.getLockedRoot.isDefined, s"$defender is not locked")
      assert(defender.getLockedRoot.get == contender.getLockedRoot.get, s"$defender and $contender not merged (roots ${defender.getLockedRoot.get} and ${contender.getLockedRoot.get})")
      if(contender.isTransitivePredecessor(defender)) {
        FirstFirst
      } else if (defender.isTransitivePredecessor(contender)) {
        SecondFirst
      } else {
        // unordered nested acquisition of two monitors here is safe against deadlocks because the turns' locks
        // (see assertions) ensure that only a single thread at a time will ever attempt to do so.

        val contenderBundle = contender.acquirePhaseLockAndGetEstablishmentBundle()
        val defenderBundle = defender.acquirePhaseLockAndGetEstablishmentBundle()
        val (contenderPhase, contenderPredecessorsSpanningTree) = Await.result(contenderBundle, Duration.Zero) // TODO Duration.Inf
        val (defenderPhase, defenderPredecessorsSpanningTree) = Await.result(defenderBundle, Duration.Zero) // TODO Duration.Inf
        if (defenderPhase < contenderPhase) {
          defender.blockingAddPredecessorAndReleasePhaseLock(contenderPredecessorsSpanningTree)
          contender.asyncReleasePhaseLock()
          SecondFirst
        } else {
          contender.blockingAddPredecessorAndReleasePhaseLock(defenderPredecessorsSpanningTree)
          defender.asyncReleasePhaseLock()
          FirstFirst
        }
      }
    }
  }
}
