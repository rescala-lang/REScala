package tests.rescala.misc

import rescala.core.{AdmissionTicket, CreationTicket, Scheduler, Transaction}
import tests.rescala.testtools.RETests

class CreationTicketTest extends RETests {
  multiEngined { engine =>
    import engine._

    if (engine != rescala.Schedulers.toposort) {
      /* this test uses some shady planned()(identity) to get the turn object out of the transaction
       * you should not do this. */
      def getTurn(implicit engine: Scheduler[State]): Transaction[State] =
        engine.forceNewTransaction()(_.tx)

      test("none Dynamic No Implicit") {
        assert(implicitly[CreationTicket[State]].scope.self === Right(engine.scheduler))
      }

      test("some Dynamic No Implicit") {
        engine.transaction() { (dynamicTurn: AdmissionTicket[State]) =>
          assert(implicitly[CreationTicket[State]].scope.self === Right(engine.scheduler))
          assert(implicitly[CreationTicket[State]].scope.embedTransaction(identity) === dynamicTurn.tx)
        }
      }

      test("none Dynamic Some Implicit") {
        implicit val implicitTurn: Transaction[engine.State] = getTurn
        assert(implicitly[CreationTicket[State]].scope.self === Left(implicitTurn))
        assert(implicitly[CreationTicket[State]].scope.embedTransaction(identity) === implicitTurn)
      }

      test("some Dynamic Some Implicit") {
        engine.transaction() { (_: AdmissionTicket[State]) =>
          implicit val implicitTurn: Transaction[engine.State] = getTurn
          assert(implicitly[CreationTicket[State]].scope.self === Left(implicitTurn))
          assert(implicitly[CreationTicket[State]].scope.embedTransaction(identity) === implicitTurn)
        }
      }

      test("implicit In Closures") {
        val closureDefinition: Transaction[State] = getTurn(engine.scheduler)
        val closure = {
          implicit def it: Transaction[State] = closureDefinition
          () => implicitly[CreationTicket[State]]
        }
        engine.transaction() { _ =>
          assert(closure().scope.self === Left(closureDefinition))
          assert(closure().scope.embedTransaction(identity) === closureDefinition)
        }
      }

      test("dynamic In Closures") {
        val closure: () => CreationTicket[State] = {
          engine.transaction() { _ => () => implicitly[CreationTicket[State]] }
        }
        engine.transaction() { dynamic =>
          assert(closure().scope.self === Right(engine.scheduler))
          assert(closure().scope.embedTransaction(identity) === dynamic.tx)
        }
      }

    }
  }
}
