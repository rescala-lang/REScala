package tests.rescala.misc

import reactives.core.{AdmissionTicket, CreationTicket, Scheduler, Transaction}
import tests.rescala.testtools.RETests

class CreationTicketTest extends RETests {
  multiEngined { engine =>
    import engine._

    //if (engine != reactives.interfaces.toposort) {
      /* this test uses some shady planned()(identity) to get the turn object out of the transaction
       * you should not do this. */
      def getTurn(implicit engine: Scheduler[BundleState]): Transaction[BundleState] =
        engine.forceNewTransaction()(_.tx)

      test("none Dynamic No Implicit") {
        assert(implicitly[CreationTicket[BundleState]].scope.self === Right(engine.scheduler))
      }

      test("some Dynamic No Implicit") {
        engine.transaction() { (dynamicTurn: AdmissionTicket[BundleState]) =>
          assert(implicitly[CreationTicket[BundleState]].scope.self === Right(engine.scheduler))
          assert(implicitly[CreationTicket[BundleState]].scope.embedTransaction(identity) === dynamicTurn.tx)
        }
      }

      test("none Dynamic Some Implicit") {
        implicit val implicitTurn: Transaction[engine.BundleState] = getTurn
        assert(implicitly[CreationTicket[BundleState]].scope.self === Left(implicitTurn))
        assert(implicitly[CreationTicket[BundleState]].scope.embedTransaction(identity) === implicitTurn)
      }

      test("some Dynamic Some Implicit") {
        engine.transaction() { (_: AdmissionTicket[BundleState]) =>
          implicit val implicitTurn: Transaction[engine.BundleState] = getTurn
          assert(implicitly[CreationTicket[BundleState]].scope.self === Left(implicitTurn))
          assert(implicitly[CreationTicket[BundleState]].scope.embedTransaction(identity) === implicitTurn)
        }
      }

      test("implicit In Closures") {
        val closureDefinition: Transaction[BundleState] = getTurn(engine.scheduler)
        val closure = {
          implicit def it: Transaction[BundleState] = closureDefinition
          () => implicitly[CreationTicket[BundleState]]
        }
        engine.transaction() { _ =>
          assert(closure().scope.self === Left(closureDefinition))
          assert(closure().scope.embedTransaction(identity) === closureDefinition)
        }
      }

      test("dynamic In Closures") {
        val closure: () => CreationTicket[BundleState] = {
          engine.transaction() { _ => () => implicitly[CreationTicket[BundleState]] }
        }
        engine.transaction() { dynamic =>
          assert(closure().scope.self === Right(engine.scheduler))
          assert(closure().scope.embedTransaction(identity) === dynamic.tx)
        }
      }

    //}
  }
}
