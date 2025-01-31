package tests.rescala.misc

import munit.FunSuite
import reactives.SelectedScheduler.State
import reactives.core.CreationScope.{DynamicCreationScope, StaticCreationScope}
import reactives.core.CreationTicket.given
import reactives.core.{AdmissionTicket, CreationScope, CreationTicket, Scheduler, Transaction}
import reactives.default.transaction

class CreationTicketTest extends FunSuite {

  // if (engine != reactives.interfaces.toposort) {
  /* this test uses some shady planned()(identity) to get the turn object out of the transaction
   * you should not do this. */
  def getTurn(using engine: Scheduler[State]): Transaction[State] =
    engine.forceNewTransaction()(_.tx)

  test("none Dynamic No Implicit") {
    assert(summon[CreationTicket[State]].scope.isInstanceOf[DynamicCreationScope[State]])
  }

  test("some Dynamic No Implicit") {
    transaction() { (dynamicTurn: AdmissionTicket[State]) =>
      assert(summon[CreationTicket[State]].scope.isInstanceOf[DynamicCreationScope[State]])
      assertEquals(summon[CreationTicket[State]].scope, CreationScope.search)
      assertEquals(dynamicTurn.tx, summon[CreationTicket[State]].scope.embedCreation(x ?=> x))
    }
  }

  test("none Dynamic Some Implicit") {
    given implicitTurn: Transaction[State] = getTurn

    implicitly[CreationTicket[State]].scope match
      case StaticCreationScope(tx) => assertEquals(tx, implicitTurn)
      case other                   => assert(false)
    assertEquals(implicitTurn, summon[CreationTicket[State]].scope.embedCreation(x ?=> x))
  }

  test("some Dynamic Some Implicit") {
    transaction() {
      given implicitTurn: Transaction[State] = getTurn

      summon[CreationTicket[State]](using implicitTurn.convert).scope match
        case StaticCreationScope(tx) => assertEquals(tx, implicitTurn)
        case other                   => assert(false)
      assertEquals(implicitTurn, summon[CreationTicket[State]](using implicitTurn.convert).scope.embedCreation(x ?=> x))
    }
  }

  test("implicit In Closures") {
    val closureDefinition: Transaction[State] = getTurn(using reactives.SelectedScheduler.candidate.scheduler)
    val closure = {
      given it: Transaction[State] = closureDefinition

      () => implicitly[CreationTicket[State]]
    }
    transaction() {
      closure().scope match
        case StaticCreationScope(tx) => assertEquals(tx, closureDefinition)
        case other                   => assert(false)
      assertEquals(closureDefinition, closure().scope.embedCreation(x ?=> x))
    }
  }

  test("dynamic In Closures") {

    def otherScope = () => implicitly[CreationTicket[State]]

    val closure: () => CreationTicket[State] = {
      transaction() { otherScope }
    }
    transaction() { dynamic ?=>
      assert(closure().scope.isInstanceOf[DynamicCreationScope[State]])
      assertEquals(dynamic.tx, closure().scope.embedCreation(x ?=> x))
    }
  }

}
