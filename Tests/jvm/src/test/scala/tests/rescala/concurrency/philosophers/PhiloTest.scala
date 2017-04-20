package tests.rescala.concurrency.philosophers

import org.scalatest.FunSuite
import rescala.Engines
import rescala.engine.{Engine, Turn}
import rescala.graph.Struct
import tests.rescala.concurrency.Spawn
import tests.rescala.concurrency.philosophers.PhilosopherTable.{Seating, Thinking}

import scala.annotation.tailrec
import scala.util.Random


class PhiloTest extends FunSuite {

  @tailrec
  final def deal[A](deck: List[A], hands: List[List[A]]): List[List[A]] = deck match {
    case Nil => hands
    case card :: rest => deal(rest, hands.tail :+ (card :: hands.head))
  }


  def `eat!`[S <: Struct](engine: Engine[S, Turn[S]], dynamic: Boolean): Unit = {
    val philosophers = 5
    val threadCount = 3
    val table =
      if (!dynamic) new PhilosopherTable(philosophers, 0)(engine)
      else new DynamicPhilosopherTable(philosophers, 0)(engine)
    val blocks: Array[Array[Seating[S]]] = Array(table.seatings.toArray)

    @volatile var cancel = false

    val threads = for (threadIndex <- Range(0, threadCount)) yield Spawn(name = s"Worker $threadIndex", f = {
      while (!cancel) {
        val myBlock = blocks(threadIndex % blocks.length)
        val seating = myBlock(Random.nextInt(myBlock.length))
        table.eatOnce(seating)
        seating.philosopher.set(Thinking)(table.engine)
      }
    })

    println(s"philo party sleeping on $engine (dynamic $dynamic)")
    Thread.sleep(1000)
    cancel = true
    threads.foreach(_.join(1000))
    assert(threads.forall(!_.isAlive), "threads did not finish")
    println(s"philo party done sleeping on $engine (dynamic $dynamic)")
  }

  test("eating Contests Spinning") {`eat!`(Engines.parrp, dynamic = false)}

  test("eating Contests Spinning Dynamic") {`eat!`(Engines.parrp, dynamic = true)}

  test("eating Contests Spinning Locksweep") {`eat!`(Engines.locksweep, dynamic = false)}

  test("eating Contests Spinning Dynamic Locksweep") {`eat!`(Engines.locksweep, dynamic = true)}

  //  test("eatingContestsSpinningParallelLocksweep"){`eat!`(JVMEngines.parallellocksweep, dynamic = false)}
  //
  //  test("eatingContestsSpinningDynamicParallelLocksweep"){`eat!`(JVMEngines.parallellocksweep, dynamic = true)}

}
