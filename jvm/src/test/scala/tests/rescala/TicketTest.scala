package tests.rescala


import rescala.pipelining.PipelineEngine




class TicketTest extends RETests {


  /* this test uses some shady planned()(identity) to get the turn object out of the transaction
   * you should not do this. */
  def getTurn[S2 <: rescala.graph.Struct](implicit engine: rescala.engines.Engine[S2, rescala.propagation.Turn[S2]]): rescala.propagation.Turn[S2] = engine.plan()(identity)

  allEngines("none Dynamic NoImplicit"){ engine => import engine._
    assert(implicitly[Ticket].self === Right(engine))
  }

  allEngines("some Dynamic NoImplicit") { engine => import engine._
    engine.plan() { (dynamicTurn: Turn) =>
      assert(implicitly[Ticket].self === Right(engine))
      assert(implicitly[Ticket].apply(identity) === dynamicTurn)
    }
  }

  allEngines("none Dynamic Some Implicit"){ engine => import engine._
    implicit val implicitTurn: Turn = getTurn
    assert(implicitly[Ticket].self === Left(implicitTurn))
    assert(implicitly[Ticket].apply(identity) === implicitTurn)
  }

  // Cannot run a turn inside a turn with pipelining
  allEngines("some Dynamic Some Implicit"){ engine => import engine._
    if (engine.isInstanceOf[PipelineEngine]) {
      throw new IllegalStateException("pipeline engine cannot run a turn inside a turn")
    }
    else {
      engine.plan() { (dynamicTurn: Turn) =>
        implicit val implicitTurn: Turn = getTurn
        assert(implicitly[Ticket].self === Left(implicitTurn))
        assert(implicitly[Ticket].apply(identity) === implicitTurn)
      }
}
}



  allEngines("implicit InClosures"){ engine => import engine._
    val closureDefinition = getTurn(engine)
    val closure = {
      implicit def it: Turn = closureDefinition
      () => implicitly[Ticket]
    }
    engine.plan() { dynamic =>
      assert(closure().self === Left(closureDefinition))
      assert(closure().apply(identity) === closureDefinition)
    }
  }

  allEngines("dynamic InClosures"){ engine => import engine._
    val closure = {
      engine.plan() { t =>
        () => implicitly[Ticket]
      }
    }
    engine.plan() { dynamic =>
      assert(closure().self === Right(engine))
      assert(closure().apply(identity) === dynamic)
    }
  }

}
