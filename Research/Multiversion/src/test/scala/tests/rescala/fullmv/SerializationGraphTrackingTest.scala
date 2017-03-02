package tests.rescala.fullmv

import org.scalatest.{FlatSpec, Ignore, Matchers}
import rescala.fullmv.api._

@Ignore
class SerializationGraphTrackingTest extends FlatSpec with Matchers {
  private def assertOrder(sgt: SerializationGraphTracking, a: Transaction, b: Transaction) = {
    sgt.getOrder(a, b) === FirstFirst
    sgt.getOrder(b, a) === SecondFirst
    sgt.ensureOrder(a, b) === FirstFirst
    sgt.ensureOrder(b, a) === SecondFirst
  }

  "SGT" should "not establish orders for no reason" in {
    val a, b = Transaction()
    val sgt = SerializationGraphTracking()
    sgt.getOrder(a, b) === None
    sgt.getOrder(b, a) === None
  }

  it should "order preparing transactions first-come-first-serve" in {
    val a, b = Transaction()
    val sgt = SerializationGraphTracking()
    sgt.ensureOrder(a, b) === FirstFirst
    assertOrder(sgt, a, b)
  }

  it should "order executing transactions first-come-first-serve" in {
    val a, b = Transaction().start()
    val sgt = SerializationGraphTracking()
    sgt.ensureOrder(a, b) === FirstFirst
    assertOrder(sgt, a, b)
  }

  it should "refuse to order completed contenders" in {
    val c = Transaction()
    val b = Transaction().done()
    val sgt = SerializationGraphTracking()
    a [IllegalArgumentException] should be thrownBy {
      sgt.ensureOrder(c, b)
    }
  }

  it should "order preparing transactions after running ones" in {
    val a = Transaction()
    val b = Transaction().start()
    val sgt = SerializationGraphTracking()
    assertOrder(sgt, b, a)
  }

  it should "order preparing transactions after completed ones" in {
    val a, b, c = Transaction()
    val sgt = SerializationGraphTracking()
    sgt.ensureOrder(c, b) === FirstFirst
    b.done().phase === Completed
    assertOrder(sgt, b, a)
  }

  it should "order preparing transactions after obsolete ones" in {
    val a, b = Transaction()
    val sgt = SerializationGraphTracking()
    b.done().phase === Obsolete
    assertOrder(sgt, b, a)
  }

  it should "order executing transactions after completed ones" in {
    val a, b, c = Transaction().start()
    val sgt = SerializationGraphTracking()
    sgt.ensureOrder(c, b) === FirstFirst
    b.done().phase === Completed
    assertOrder(sgt, b, a)
  }

  it should "order executing transactions after obsolete ones" in {
    val a, b = Transaction().start()
    val sgt = SerializationGraphTracking()
    b.done().phase === Obsolete
    assertOrder(sgt, b, a)
  }

  it should "recognize transitive orderings" in {
    val a, b, c = Transaction().start()
    val sgt = SerializationGraphTracking()
    sgt.ensureOrder(a, b) === FirstFirst
    sgt.ensureOrder(b, c) === FirstFirst
    assertOrder(sgt, a, c)
  }
}
