package tests.rescala.concurrency.philosophers

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.graph.Spores
import rescala.synchronization.Engines
import rescala.turns.{Engine, Turn}
import tests.rescala.concurrency.Spawn
import tests.rescala.concurrency.philosophers.PhilosopherTable.{Thinking, Seating}

import scala.annotation.tailrec
import scala.util.Random


class PhiloTest extends AssertionsForJUnit {

  @tailrec
  final def deal[A](deck: List[A], hands: List[List[A]]): List[List[A]] = deck match {
    case Nil => hands
    case card :: rest => deal(rest, hands.tail :+ (card :: hands.head))
  }


  def `eat!`[S <: Spores](engine: Engine[S, Turn[S]], dynamic: Boolean): Unit = {
    val philosophers = 64
    val threadCount = 4
    val table =
      if (!dynamic) new PhilosopherTable(philosophers, 0)(engine)
      else new DynamicPhilosopherTable(philosophers, 0)(engine)
    val blocks: Array[Array[Seating[S]]] = deal(table.seatings.toList, List.fill(threadCount)(Nil)).map(_.toArray).toArray

    @volatile var cancel = false

    val threads = for (threadIndex <- Range(0, threadCount)) yield Spawn {
      while (!cancel) {
        val myBlock = blocks(threadIndex % blocks.length)
        val seating = myBlock(Random.nextInt(myBlock.length))
        table.eatOnce(seating)
        seating.philosopher.set(Thinking)(table.engine)
      }
    }

    println(s"philo party sleeping on $engine (dynamic $dynamic)")
    Thread.sleep(2000)
    cancel = true
    threads.foreach(_.join(1000))
    assert(threads.forall(!_.isAlive), "threads did not finish")
    println(s"philo party done sleeping on $engine (dynamic $dynamic)")
  }

  @Test def eatingContestsSpinning(): Unit = `eat!`(Engines.parRP, dynamic = false)

  @Test def eatingContestsSpinningDynamic(): Unit = `eat!`(Engines.parRP, dynamic = true)

}
