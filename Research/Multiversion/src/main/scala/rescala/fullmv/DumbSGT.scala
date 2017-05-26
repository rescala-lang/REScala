package rescala.fullmv

object DumbSGT extends SerializationGraphTracking[FullMVTurn] {
  val DEBUG = false
  private var predecessors = Map[FullMVTurn, Set[FullMVTurn]]().withDefaultValue(Set())
  private var successors = Map[FullMVTurn, Set[FullMVTurn]]().withDefaultValue(Set())

  override def ensureOrder(defender: FullMVTurn, contender: FullMVTurn): OrderResult = synchronized {
    assert(defender != contender, s"cannot establish order between equal defender $defender and contender $contender")
    assert(defender.phase > TurnPhase.Initialized, s"$defender is not started and should thus not be involved in any operations")
    assert(contender.phase > TurnPhase.Initialized, s"$contender is not started and should thus not be involved in any operations")
    assert(contender.phase < TurnPhase.Completed, s"$contender cannot be a contender (already completed).")
    if(defender.phase == TurnPhase.Completed) {
      FirstFirst
    } else if (predecessors(defender)(contender)) {
      SecondFirst
    } else if (predecessors(contender)(defender)) {
      FirstFirst
    } else if(defender.phase < contender.phase) {
      establishOrder(contender, defender)
      SecondFirst
    } else {
      establishOrder(defender, contender)
      FirstFirst
    }
  }

  private def establishOrder(first: FullMVTurn, second: FullMVTurn): Unit = {
    if(DEBUG) println(s"[${Thread.currentThread().getName}] establish SGT order $first -> $second")
    val allAfter = successors(second) + second
    val allBefore = predecessors(first) + first
    for(succ <- allAfter) predecessors += succ -> (predecessors(succ) ++ allBefore)
    for(pred <- allBefore) successors += pred -> (successors(pred) ++ allAfter)
  }

  override def getOrder(found: FullMVTurn, searcher: FullMVTurn): PartialOrderResult = synchronized {
    assert(found != searcher, s"$found compared with itself..?")
    assert(found.phase > TurnPhase.Initialized, s"$found is not started and should thus not be involved in any operations")
    assert(searcher.phase > TurnPhase.Initialized, s"$searcher is not started and should thus not be involved in any operations")
    assert(searcher.phase < TurnPhase.Completed, s"$searcher cannot be a searcher (already completed).")
    if(found.phase == TurnPhase.Completed) {
      FirstFirst
    } else if (searcher.phase == TurnPhase.Completed) {
      SecondFirst
    } else if (predecessors(found)(searcher)) {
      SecondFirst
    } else if (predecessors(searcher)(found)) {
      FirstFirst
    } else {
      Unordered
    }
  }

  override def completed(turn: FullMVTurn): Unit = synchronized {
    assert(turn.phase == TurnPhase.Completed, s"Trying to discard incomplete $turn")
    for(succ <- successors(turn)) {
      val previousPredecessors = predecessors(succ)
      if(previousPredecessors.size == 1) {
        assert(previousPredecessors == Set(turn), s"predecessor tracking for $succ was inaccurate: should only contain $turn, but contained $previousPredecessors.")
        predecessors -= succ
      } else {
        val remainingPredecessors = predecessors(succ) - turn
        predecessors += succ -> remainingPredecessors
      }
    }
    successors -= turn
  }

  override def awaitAllPredecessorsState(turn: FullMVTurn, atLeast: TurnPhase.Type): Unit = {
    // Note that each turn on which this suspends has the opportunity to add additional predecessors to this turn
    // transitively. We do, however, not need to repeatedly lookup the set of all predecessors, thereby ignoring these,
    // because the turn on which this suspended will hold the suspension until these new transitive predecessors have
    // reached the same stage first. Thus, looking up our own predecessors again after a suspension might reveal
    // additional predecessors, but they would all have reached the required state already.
    synchronized{predecessors(turn)}.foreach { waitFor =>
      if(DEBUG) println(s"[${Thread.currentThread().getName}] $turn awaiting state $atLeast of $waitFor")
      waitFor.awaitState(atLeast)
    }
  }
}
