package tests.rescala.misc

import rescala.core.Struct
import tests.rescala.testtools.RETests


class CreationTicketTest extends RETests { multiEngined { engine => import engine._

  /* this test uses some shady planned()(identity) to get the turn object out of the transaction
   * you should not do this. */
  def getTurn[S2 <: Struct](implicit engine: rescala.core.Scheduler[S2]): rescala.core.Initializer[S2] = engine.transaction()(_.creation)

  test("none Dynamic No Implicit") {
    assert(implicitly[CreationTicket].self === Right(engine))
  }

  test("some Dynamic No Implicit") {
    engine.transaction() { (dynamicTurn: AdmissionTicket) =>
      assert(implicitly[CreationTicket].self === Right(engine))
      assert(implicitly[CreationTicket].apply(identity) === dynamicTurn.creation)
    }
  }

  test("none Dynamic Some Implicit") {
    implicit val implicitTurn: Creation = getTurn
    assert(implicitly[CreationTicket].self === Left(implicitTurn))
    assert(implicitly[CreationTicket].apply(identity) === implicitTurn)
  }

  // Cannot run a turn inside a turn with pipelining
  test("some Dynamic Some Implicit") {
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



  test("implicit In Closures") {
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

  test("dynamic In Closures") {
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

} }
