package tests.rescala.fullmv

import org.scalatest._
import rescala.fullmv.api._

@Ignore
class TransactionTest extends FlatSpec with Matchers{
  "A newly created transaction" should "be preparing" in {
    Transaction().phase === Preparing
  }

  it should "be able to start" in {
    Transaction().start().phase === Running
  }

  it should "be able to finish into obsolete state" in {
    Transaction().done().phase === Obsolete
  }

  "A running transaction" should "throw IllegalState if started again" in {
    val t = Transaction().start()
    a [IllegalStateException] should be thrownBy {
      t.start()
    }
  }

  it should "be able to finish into obsolete state" in {
    Transaction().start().done().phase === Obsolete
  }

  "A successor transaction" should "become obsolete only after all predecessors" in {
    val a, b, c, d = Transaction()

    val sgt = SerializationGraphTracking()
    sgt.ensureOrder(a, b)
    sgt.ensureOrder(b, c)
    sgt.ensureOrder(d, c)

    c.done().phase === Completed
    b.done()
    c.phase === Completed
    d.done()
    c.phase === Completed
    a.done()
    c.phase === Obsolete
  }

  it should "become obsolete immediately if all predecessors are already obsolete" in {
    val a, b, c, d = Transaction()

    val sgt = SerializationGraphTracking()
    sgt.ensureOrder(a, b)
    sgt.ensureOrder(b, c)
    sgt.ensureOrder(d, c)

    a.done()
    b.done()
    d.done()

    c.done().phase === Obsolete
  }

  "A predecessor transaction" should "become obsolete regardless of incomplete successors" in {
    val a, b, c, d= Transaction()

    val sgt = SerializationGraphTracking()
    sgt.ensureOrder(a, b)
    sgt.ensureOrder(a, c)
    sgt.ensureOrder(a, d)

    c.start()
    d.done()

    a.done().phase === Obsolete
  }
}
