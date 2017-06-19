package tests.rescala


class CreationTicketTest extends RETests {

  /* this test uses some shady planned()(identity) to get the turn object out of the transaction
   * you should not do this. */
  def getTurn[S2 <: rescala.graph.Struct](implicit engine: rescala.engine.Engine[S2]): rescala.graph.CreationAllowed[S2] = engine.transaction()(identity)

  allEngines("none Dynamic No Implicit") { engine => import engine._
    assert(implicitly[TurnSource].self === Right(engine))
  }

  allEngines("some Dynamic No Implicit") { engine => import engine._
    engine.transaction() { (dynamicTurn: AdmissionTicket) =>
      assert(implicitly[TurnSource].self === Right(engine))
      assert(implicitly[TurnSource].apply(_.turn) === dynamicTurn.turn)
    }
  }

  allEngines("none Dynamic Some Implicit") { engine => import engine._
    implicit val implicitTurn: CreationAllowed = getTurn
    assert(implicitly[TurnSource].self === Left(implicitTurn))
    assert(implicitly[TurnSource].apply(identity).turn === implicitTurn)
  }

  // Cannot run a turn inside a turn with pipelining
  allEngines("some Dynamic Some Implicit") { engine => import engine._
    //    if (engine.isInstanceOf[PipelineEngine]) {
    //      throw new IllegalStateException("pipeline engine cannot run a turn inside a turn")
    //    }
    //    else {
    engine.transaction() { (dynamicTurn: AdmissionTicket) =>
      implicit val implicitTurn: CreationAllowed = getTurn
      assert(implicitly[TurnSource].self === Left(implicitTurn))
      assert(implicitly[TurnSource].apply(_.turn) === implicitTurn)
      //      }
    }
  }



  allEngines("implicit In Closures") { engine => import engine._
    val closureDefinition: CreationAllowed = getTurn(engine)
    val closure = {
      implicit def it: CreationAllowed = closureDefinition
      () => implicitly[TurnSource]
    }
    engine.transaction() { dynamic =>
      assert(closure().self === Left(closureDefinition))
      assert(closure().apply(_.turn) === closureDefinition)
    }
  }

  allEngines("dynamic In Closures") { engine => import engine._
    val closure = {
      engine.transaction() { t =>
        () => implicitly[TurnSource]
      }
    }
    engine.transaction() { dynamic =>
      assert(closure().self === Right(engine))
      assert(closure().apply(_.turn) === dynamic.turn)
    }
  }

}
