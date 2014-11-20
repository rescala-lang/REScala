package rescala.test.concurrency

import rescala.signals.Signals
import rescala.signals.Var
import rescala.propagation.Turn
import scala.util.Random
import rescala.propagation.TurnFactory
import rescala.propagation.turns._

object PaperGlitchAppTest extends App {
  def thread(f: => Unit): Thread = {
    val t = new Thread(new Runnable {
      override def run(): Unit = f
    })
    t.start()
    t
  }
  val lnOf2 = scala.math.log(2) // natural log of 2
  def log2(x: Double): Double = scala.math.log(x) / lnOf2
  @annotation.tailrec
  def isPowerOf2(int: Int) : Boolean = {
    if(int < 1) {
      throw new IllegalArgumentException
    }
    if(int == 1) {
      true
    } else if(int % 2 == 0){
      isPowerOf2(int / 2)
    } else {
      false
    }
  }
  
  // ============================================================================================================
  
  implicit val turnType = UnSynchronized // <-- change here for FUN
  
  val price = Var(3)
  val tax = price.map { p => p / 3 }
  val quantity = Var(1)
  val total = Signals.mapping(quantity, price, tax) {
    t: Turn => quantity.getValue(t) * (price.getValue(t) + tax.getValue(t))
  }

  // ============================================================================================================

  total.changed += { v =>
    if (isPowerOf2(v)) {
      println("Ok: " + v)
    } else {
      System.err.println("Bad: " + v)
    }
  }

  // ============================================================================================================
  
  thread {
    for (i <- 0 to 100000) price.set(3 * 2 << Random.nextInt(8))
  }
  thread {
    for (i <- 0 to 100000) quantity.set(2 << Random.nextInt(8))
  }
}