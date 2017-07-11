package tests.rescala

import rescala.core.Struct


class CreationTicketTest extends RETests {

  /* this test uses some shady planned()(identity) to get the turn object out of the transaction
   * you should not do this. */
  def getTurn[S2 <: Struct](implicit engine: rescala.core.Engine[S2]): rescala.core.Creation[S2] = engine.transaction()(_.creation)

  allEngines("none Dynamic No Implicit") { engine => import engine._
    assert(implicitly[CreationTicket].self === Right(engine))
  }

  allEngines("some Dynamic No Implicit") { engine => import engine._
    engine.transaction() { (dynamicTurn: AdmissionTicket) =>
      assert(implicitly[CreationTicket].self === Right(engine))
      assert(implicitly[CreationTicket].apply(identity) === dynamicTurn.creation)
    }
  }

  allEngines("none Dynamic Some Implicit") { engine => import engine._
    implicit val implicitTurn: Creation = getTurn
    assert(implicitly[CreationTicket].self === Left(implicitTurn))
    assert(implicitly[CreationTicket].apply(identity) === implicitTurn)
  }

  // Cannot run a turn inside a turn with pipelining
  allEngines("some Dynamic Some Implicit") { engine => import engine._
    //    if (engine.isInstanceOf[PipelineEngine]) {
    //      throw new IllegalStateException("pipeline engine cannot run a turn inside a turn")
    //    }
    //    else {
    engine.transaction() { (dynamicTurn: AdmissionTicket) =>
      implicit val implicitTurn: Creation = getTurn
      assert(implicitly[CreationTicket].self === Left(implicitTurn))
      assert(implicitly[CreationTicket].apply(identity) === implicitTurn)
      //      }
    }
  }



  allEngines("implicit In Closures") { engine => import engine._
    val closureDefinition: Creation = getTurn(engine)
    val closure = {
      implicit def it: Creation = closureDefinition
      () => implicitly[CreationTicket]
    }
    engine.transaction() { dynamic =>
      assert(closure().self === Left(closureDefinition))
      assert(closure().apply(identity) === closureDefinition)
    }
  }

  allEngines("dynamic In Closures") { engine => import engine._
    val closure = {
      engine.transaction() { t =>
        () => implicitly[CreationTicket]
      }
    }
    engine.transaction() { dynamic =>
      assert(closure().self === Right(engine))
      assert(closure().apply(identity) === dynamic.creation)
    }
  }

}
