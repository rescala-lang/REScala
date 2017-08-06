package rescala.fullmv

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
    assert(contender.lock.getLockedRoot.isDefined, s"$contender is not locked (${contender.lock})")
    if(defender.phase == TurnPhase.Completed) {
      FirstFirst
    } else{
      assert(defender.lock.getLockedRoot.isDefined, s"$defender is not locked (${defender.lock})")
      assert(defender.lock.getLockedRoot.get == contender.lock.getLockedRoot.get, s"$defender and $contender not merged (roots ${defender.lock.getLockedRoot.get} and ${contender.lock.getLockedRoot.get})")
      if(contender.isTransitivePredecessor(defender)) {
        FirstFirst
      } else if (defender.isTransitivePredecessor(contender)) {
        SecondFirst
      } else {
        // unordered nested acquisition of two monitors here is safe against deadlocks because the turns' locks
        // (see assertions) ensure that only a single thread at a time will ever attempt to do so.
        contender.phaseLock.synchronized {
          defender.phaseLock.synchronized {
            if (defender.phase < contender.phase) {
              defender.addPredecessor(contender)
              SecondFirst
            } else {
              contender.addPredecessor(defender)
              FirstFirst
            }
          }
        }
      }
    }
  }
}
