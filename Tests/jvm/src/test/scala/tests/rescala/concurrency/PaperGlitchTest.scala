package tests.rescala.concurrency

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}

import org.scalatest.FunSuite
import rescala.Engines
import rescala.reactives.Var
import tests.rescala.testtools.Spawn

import scala.util.Random

class PaperGlitchTest extends FunSuite {

  ignore("execute paper glitch test"){
    /*val lnOf2 = scala.math.log(2) // natural log of 2*/
    /*def log2(x: Double): Double = scala.math.log(x) / lnOf2*/
    def isPowerOf2(x: Int) = (x & (x - 1)) == 0

    // ============================================================================================================

    // change here for FUN
    import Engines.unmanaged

    val price = Var(3)
    val tax = price.map { p => p / 3 }
    val quantity = Var(1)
    val total = unmanaged.Signal.static{ quantity.value * (price.value + tax.value) }

    // ============================================================================================================

    val glitches = new ConcurrentLinkedQueue[Int]()

    total.changed += { v => if (!isPowerOf2(v)) glitches.add(v) }

    // ============================================================================================================

    @volatile var cancelled = false

    val latch = new CountDownLatch(3)

    val t1 = Spawn {
      latch.countDown()
      latch.await()
      while (!cancelled) {
        try {
          price.set(3 * 2 << Random.nextInt(8))
        } catch {
          // occurs because of invalid buffer
          case e: AssertionError => glitches.add(-42)
          // can occur because of missing sync barriers
          case e: NoSuchElementException => glitches.add(-43)
        }
      }
    }
    val t2 = Spawn {
      latch.countDown()
      latch.await()
      while (!cancelled) {
        try {
          quantity.set(2 << Random.nextInt(8))
        } catch {
          // occurs because of invalid buffer
          case e: AssertionError => glitches.add(-42)
          // can occur because of missing sync barriers
          case e: NoSuchElementException => glitches.add(-43)
        }
      }
    }
    latch.countDown()
    latch.await()

    Thread.sleep(1000)
    cancelled = true
    assert(glitches.size() > 0)
    t1.join(1000)
    t2.join(1000)
  }
}
