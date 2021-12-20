package tests.rescala.misc

import tests.rescala.testtools.RETests

class CreationTicketTest extends RETests {
  multiEngined { engine =>
    import engine._

    if (engine != rescala.Schedulers.simple) {
      /* this test uses some shady planned()(identity) to get the turn object out of the transaction
       * you should not do this. */
      def getTurn(implicit engine: Scheduler): Initializer =
        engine.forceNewTransaction()(_.tx.initializer)

      test("none Dynamic No Implicit") {
        assert(implicitly[CreationTicket].self === Right(engine.scheduler))
      }

      test("some Dynamic No Implicit") {
        engine.transaction() { (dynamicTurn: AdmissionTicket) =>
          assert(implicitly[CreationTicket].self === Right(engine.scheduler))
          assert(implicitly[CreationTicket].dynamicCreation(identity) === dynamicTurn.tx.initializer)
        }
      }

      test("none Dynamic Some Implicit") {
        implicit val implicitTurn: Initializer = getTurn
        assert(implicitly[CreationTicket].self === Left(implicitTurn))
        assert(implicitly[CreationTicket].dynamicCreation(identity) === implicitTurn)
      }

      test("some Dynamic Some Implicit") {
        engine.transaction() { (dynamicTurn: AdmissionTicket) =>
          implicit val implicitTurn: Initializer = getTurn
          assert(implicitly[CreationTicket].self === Left(implicitTurn))
          assert(implicitly[CreationTicket].dynamicCreation(identity) === implicitTurn)
        }
      }

      test("implicit In Closures") {
        val closureDefinition: Initializer = getTurn(engine.scheduler)
        val closure = {
          implicit def it: Initializer = closureDefinition
          () => implicitly[CreationTicket]
        }
        engine.transaction() { dynamic =>
          assert(closure().self === Left(closureDefinition))
          assert(closure().dynamicCreation(identity) === closureDefinition)
        }
      }

      test("dynamic In Closures") {
        val closure: () => engine.CreationTicket = {
          engine.transaction() { t => () => implicitly[CreationTicket] }
        }
        engine.transaction() { dynamic =>
          assert(closure().self === Right(engine.scheduler))
          assert(closure().dynamicCreation(identity) === dynamic.tx.initializer)
        }
      }

    }
  }
}
