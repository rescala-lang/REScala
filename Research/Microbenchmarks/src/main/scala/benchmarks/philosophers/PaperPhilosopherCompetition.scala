package benchmarks.philosophers

import java.util
import java.util.concurrent.{ThreadLocalRandom, TimeUnit}
import java.util.concurrent.locks.{Lock, ReentrantLock}

import benchmarks.{EngineParam, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.{BenchmarkParams, ThreadParams}
import rescala.core.Struct
import rescala.parrp.Backoff

import scala.annotation.tailrec

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(2)
class PaperPhilosopherCompetition[S <: Struct] {
  @Benchmark
  def eat(comp: PaperCompetition[S], params: ThreadParams, work: Workload): Unit = {
    val myBlock = comp.blocks(params.getThreadIndex % comp.blocks.length)
    val bo = new Backoff()
    while ( {
      val seating: Int = myBlock(ThreadLocalRandom.current().nextInt(myBlock.length))
      if (comp.manualLocking)
        manualLocking(comp, seating)
      else
        tryUpdateCycle(comp, seating)
    }) {bo.backoff()}
  }

  def tryUpdateCycle(comp: PaperCompetition[S], seating: Int): Boolean = {
    comp.table.maybeEat(seating)
    if(comp.table.hasEaten(seating)) {
      comp.table.rest(seating)
      false
    } else {
      true
    }
  }

  private def manualLocking(comp: PaperCompetition[S], seating: Int): Boolean = {
    val pos = Array(seating, (seating + 1) % comp.philosophers, (seating + 2) % comp.philosophers)
    util.Arrays.sort(pos)
    val firstLock = comp.locks(pos(0))
    val secondLock = comp.locks(pos(1))
    val thirdLock = comp.locks(pos(2))
    firstLock.lock()
    try {
      secondLock.lock()
      try {
        thirdLock.lock()
        try {
          tryUpdateCycle(comp, seating)
        }
        finally {thirdLock.unlock()}
      }
      finally {secondLock.unlock()}
    }
    finally {firstLock.unlock()}
  }
}

@State(Scope.Benchmark)
class PaperCompetition[S <: Struct] {
  @Param(Array("16", "32"))
  var philosophers: Int = _

  var table: PaperPhilosophers[S] = _
  var blocks: Array[Array[Int]] = _

  var manualLocking: Boolean = _
  var locks: Array[Lock] = _

  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam[S]) = {
    manualLocking = engineParam.engineName == "unmanaged"
    if (manualLocking) {
      locks = Array.fill(philosophers)(new ReentrantLock())
    }
    table = new PaperPhilosophers(philosophers, engineParam.engine)
    blocks = deal((0 until philosophers).toList, params.getThreads).map(_.toArray).toArray
  }

  final def deal[A](initialDeck: List[A], numberOfHands: Int): List[List[A]] = {
    @tailrec
    def loop(deck: List[A], hands: List[List[A]]): List[List[A]] =
      deck match {
        case Nil => hands
        case card :: rest => loop(rest, hands.tail :+ (card :: hands.head))
      }
    loop(initialDeck, List.fill(numberOfHands)(Nil))
  }

}
