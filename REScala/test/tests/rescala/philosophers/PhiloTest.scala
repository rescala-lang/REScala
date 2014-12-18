package tests.rescala.philosophers

import benchmarks.PhilosopherTable
import benchmarks.PhilosopherTable.{Seating, Thinking}
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rescala.turns.Engines
import tests.rescala.concurrency.Spawn

import scala.annotation.tailrec
import scala.util.Random

class PhiloTest extends AssertionsForJUnit {

  @tailrec
  final def deal[A](deck: List[A], hands: List[List[A]]): List[List[A]] = deck match {
    case Nil => hands
    case card :: rest => deal(rest, hands.tail :+ (card :: hands.head))
  }


  @Test def `eat!`(): Unit = {
    val philosophers = 16
    val engineName = "yielding"
    val threadCount = 3
    val table = new PhilosopherTable(philosophers, 0)(Engines.byName(engineName))
    val blocks: Array[Array[Seating]] = deal(table.seatings.toList, List.fill(threadCount)(Nil)).map(_.toArray).toArray

    @volatile var cancel = false

    val threads = for (threadIndex <- Range(0, threadCount)) yield Spawn {
      while (!cancel) {
        val myBlock = blocks(threadIndex % blocks.length)
        val seating = myBlock(Random.nextInt(myBlock.length))
        table.eatOnce(seating)
        seating.philosopher.set(Thinking)(table.engine)
      }
    }

    Thread.sleep(5000)
    cancel = true
    threads.foreach(_.join(1000))
  }

}
