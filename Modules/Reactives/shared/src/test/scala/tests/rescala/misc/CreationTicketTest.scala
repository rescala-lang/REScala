package tests.rescala.misc

import reactives.core.CreationScope.{DynamicCreationScope, StaticCreationScope}
import reactives.core.{AdmissionTicket, CreationScope, CreationTicket, DynamicScopeImpl, Scheduler, Transaction}
import reactives.default.transaction
import reactives.operator.Interface.State
import tests.rescala.testtools.RETests

class CreationTicketTest extends RETests {

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
      assertEquals(CreationScope.search, summon[CreationTicket[State]].scope)
      assertEquals(summon[CreationTicket[State]].scope.embedCreation(x ?=> x), dynamicTurn.tx)
    }
  }

  test("none Dynamic Some Implicit") {
    implicit val implicitTurn: Transaction[State] = getTurn
    implicitly[CreationTicket[State]].scope match
      case StaticCreationScope(tx) => assertEquals(tx, implicitTurn)
      case other                   => assert(false)
    assertEquals(summon[CreationTicket[State]].scope.embedCreation(x ?=> x), implicitTurn)
  }

  test("some Dynamic Some Implicit") {
    transaction() { (_: AdmissionTicket[State]) =>
      implicit val implicitTurn: Transaction[State] = getTurn
      implicitly[CreationTicket[State]].scope match
        case StaticCreationScope(tx) => assertEquals(tx, implicitTurn)
        case other                   => assert(false)
      assertEquals(summon[CreationTicket[State]].scope.embedCreation(x ?=> x), implicitTurn)
    }
  }

  test("implicit In Closures") {
    val closureDefinition: Transaction[State] = getTurn(using reactives.default.global.scheduler)
    val closure = {
      implicit def it: Transaction[State] = closureDefinition
      () => implicitly[CreationTicket[State]]
    }
    transaction() { _ =>
      closure().scope match
        case StaticCreationScope(tx) => assertEquals(tx, closureDefinition)
        case other                   => assert(false)
      assertEquals(closure().scope.embedCreation(x ?=> x), closureDefinition)
    }
  }

  test("dynamic In Closures") {
    val closure: () => CreationTicket[State] = {
      transaction() { _ => () => implicitly[CreationTicket[State]] }
    }
    transaction() { dynamic =>
      assert(closure().scope.isInstanceOf[DynamicCreationScope[State]])
      assertEquals(closure().scope.embedCreation(x ?=> x), dynamic.tx)
    }
  }

}
