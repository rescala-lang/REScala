package rescala.fullmv

object DumbSGT extends SerializationGraphTracking[FullMVTurn] {
  val DEBUG = false

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
    assert(defender.lock.getLockedRoot(Thread.currentThread()).isDefined, s"$defender is not locked")
    assert(contender.lock.getLockedRoot(Thread.currentThread()).isDefined, s"$contender is not locked")
    assert(defender.lock.getLockedRoot(Thread.currentThread()).get == contender.lock.getLockedRoot(Thread.currentThread()).get, s"$defender is not locked")
    if(defender.phase == TurnPhase.Completed || contender.isTransitivePredecessor(defender)) {
      FirstFirst
    } else if (defender.isTransitivePredecessor(contender)) {
      SecondFirst
    } else {
      // contender is executing this thread, thus will not increase phase concurrently,
      // thus no need to synchronize this addition with his state changes.
      // defender however may increase his phase, thus perform compare-and-maybe-insert
      // on him mutually exclusive against his phase switches (see also comment in method)
      if(defender.addPredecessorIfHisPhaseIsLarger(contender)) {
        SecondFirst
      } else {
        contender.addPredecessor(defender)
        FirstFirst
      }
    }
  }
}
