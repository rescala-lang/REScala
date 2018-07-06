package tests.rescala.misc

import rescala.core.{Scheduler, Struct}
import tests.rescala.testtools.RETests


class CreationTicketTest extends RETests { multiEngined { engine => import engine._

  /* this test uses some shady planned()(identity) to get the turn object out of the transaction
   * you should not do this. */
  def getTurn[S2 <: Struct](implicit engine: Scheduler[S2]): rescala.core.Initializer[S2] = engine.executeTurn()(_.creation)

  test("none Dynamic No Implicit") {
    assert(implicitly[CreationTicket].self === Right(engine.scheduler))
  }

  test("some Dynamic No Implicit") {
    engine.transaction() { dynamicTurn: AdmissionTicket =>
      assert(implicitly[CreationTicket].self === Right(engine.scheduler))
      assert(implicitly[CreationTicket].apply(identity) === dynamicTurn.creation)
    }
  }

  test("none Dynamic Some Implicit") {
    implicit val implicitTurn: Creation = getTurn
    assert(implicitly[CreationTicket].self === Left(implicitTurn))
    assert(implicitly[CreationTicket].apply(identity) === implicitTurn)
  }

  test("some Dynamic Some Implicit") {
    engine.transaction() { dynamicTurn: AdmissionTicket =>
      implicit val implicitTurn: Creation = getTurn
      assert(implicitly[CreationTicket].self === Left(implicitTurn))
      assert(implicitly[CreationTicket].apply(identity) === implicitTurn)
    }
  }

  test("implicit In Closures") {
    val closureDefinition: Creation = getTurn(engine.scheduler)
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
    val closure: () => engine.CreationTicket = {
      engine.transaction() { t =>
        () => implicitly[CreationTicket]
      }
    }
    engine.transaction() { dynamic =>
      assert(closure().self === Right(engine.scheduler))
      assert(closure().apply(identity) === dynamic.creation)
    }
  }

} }
