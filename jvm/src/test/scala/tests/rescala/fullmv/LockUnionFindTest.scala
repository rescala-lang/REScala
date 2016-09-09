package tests.rescala.fullmv

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import rescala.fullmv.Transaction

class LockUnionFindTest extends FlatSpec with Matchers {
  import RemoteTestHost.remote

  def twoNodeTest(a: Transaction, b: Transaction): Unit = {
    a.lock() should be(a)
    a.tryLock() should be(None)
    a.unlock() should be(a)
    a.tryLock() should be(Some(a))
    b.tryLock() should be(Some(b))
    val aOrB = a.union(b)
    a.tryLock() should be(None)
    b.tryLock() should be(None)
    a.unlock() should be(aOrB)
    b.lock() should be(aOrB)
    a.unlock() should be(aOrB)
    a.tryLock() should be(Some(aOrB))
    b.unlock() should be(aOrB)
  }

  "lock unlock union" should "work between two nodes" in {
    val a = Transaction("a")
    val b = Transaction("b")
    twoNodeTest(a, b)
  }

  it should "work between two remote nodes" taggedAs (RemoteTestHost) in {
    val a = Transaction("a")
    val b = remote.newTransaction("b")
    twoNodeTest(a, b)
  }

  def subgraphsTest(a: Transaction, b: Transaction, c: Transaction, d: Transaction, e: Transaction, f: Transaction): Unit = {
    val ab = a.union(b)
    val cd = c.union(d)
    val ef = e.union(f)
    val cdef = c.union(f)

    a.lock() should be(ab)
    e.lock() should be(cdef)
    b.tryLock() should be(None)
    c.tryLock() should be(None)
    val all = b.union(c)
    b.tryLock() should be(None)
    c.tryLock() should be(None)
    f.unlock() should be(all)
    d.lock() should be(all)
    d.unlock() should be(all)
  }
  it should "work between two subgraphs" in {
    val a = Transaction("a")
    val b = Transaction("b")
    val c = Transaction("c")
    val d = Transaction("d")
    val e = Transaction("e")
    val f = Transaction("f")

    subgraphsTest(a, b, c, d, e, f)
  }

  it should "work between two distributed subgraphs" taggedAs (RemoteTestHost) in {
    val a = Transaction("a")
    val b = remote.newTransaction("b")
    val c = Transaction("c")
    val d = remote.newTransaction("d")
    val e = Transaction("e")
    val f = remote.newTransaction("f")

    subgraphsTest(a, b, c, d, e, f)
  }

  it should "work between two subgraphs on different hosts" taggedAs (RemoteTestHost) in {
    val a = Transaction("a")
    val b = Transaction("b")
    val c = remote.newTransaction("c")
    val d = remote.newTransaction("d")
    val e = remote.newTransaction("e")
    val f = remote.newTransaction("f")

    subgraphsTest(a, b, c, d, e, f)
  }
}