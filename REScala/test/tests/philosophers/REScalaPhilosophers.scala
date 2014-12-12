package tests.philosophers

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}
import rescala.Signals.lift
import rescala.graph.Pulsing
import rescala.synchronization.SyncUtil
import rescala.turns.Engines.pessimistic
import rescala.{DependentUpdate, Observe, Signal, Var}
import rescala.graph.Globals.named

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random


object REScalaPhilosophers extends App {
  val names = Random.shuffle(
    List("Agripina", "Alberto", "Alverta", "Beverlee", "Bill", "Bobby", "Brandy", "Caleb", "Cami", "Candice", "Candra",
      "Carter", "Cassidy", "Corene", "Danae", "Darby", "Debi", "Derrick", "Douglas", "Dung", "Edith", "Eleonor",
      "Eleonore", "Elvera", "Ewa", "Felisa", "Fidel", "Filiberto", "Francesco", "Georgia", "Glayds", "Hal", "Jacque",
      "Jeff", "Joane", "Johnny", "Lai", "Leeanne", "Lenard", "Lita", "Marc", "Marcelina", "Margret", "Maryalice",
      "Michale", "Mike", "Noriko", "Pete", "Regenia", "Rico", "Roderick", "Roxie", "Salena", "Scottie", "Sherill",
      "Sid", "Steve", "Susie", "Tyrell", "Viola", "Wilhemina", "Zenobia"))

  val size = 20

  if (size >= names.size) throw new IllegalArgumentException("Not enough names!")

  implicit val pool: ExecutionContext = ExecutionContext.fromExecutor(new ThreadPoolExecutor(
    0, size * 2 + 5, 1L, TimeUnit.SECONDS, new java.util.concurrent.SynchronousQueue[Runnable]))


  // ============================================= Infrastructure ========================================================

  sealed trait Philosopher
  case object Thinking extends Philosopher
  case object Hungry extends Philosopher

  sealed trait Fork
  case object Free extends Fork
  case class Taken(name: String) extends Fork

  sealed trait Vision
  case object Ready extends Vision
  case object Eating extends Vision
  case class WaitingFor(name: String) extends Vision

  def calcFork(leftName: String, rightName: String)(leftState: Philosopher, rightState: Philosopher): Fork =
    (leftState, rightState) match {
      case (Thinking, Thinking) => Free
      case (Hungry, _) => Taken(leftName)
      case (_, Hungry) => Taken(rightName)
    }

  def calcVision(ownName: String)(leftFork: Fork, rightFork: Fork): Vision =
    (leftFork, rightFork) match {
      case (Free, Free) => Ready
      case (Taken(`ownName`), Taken(`ownName`)) => Eating
      case (Taken(name), _) => WaitingFor(name)
      case (_, Taken(name)) => WaitingFor(name)
    }

  // ============================================ Entity Creation =========================================================

  case class Seating(placeNumber: Integer, philosopher: Var[Philosopher], leftFork: Signal[Fork], rightFork: Signal[Fork], vision: Signal[Vision])
  def createTable(tableSize: Int): Seq[Seating] = {
    def mod(n: Int): Int = (n + tableSize) % tableSize

    val phils = for (i <- 0 until tableSize) yield
      named(s"Phil-${ names(i) }")(Var[Philosopher](Thinking))

    val forks = for (i <- 0 until tableSize) yield {
      val nextCircularIndex = mod(i + 1)
      named(s"Fork-${ names(i) }-${ names(nextCircularIndex) }") {
        lift(phils(i), phils(nextCircularIndex))(calcFork(names(i), names(nextCircularIndex)))
      }
    }

    for (i <- 0 until tableSize) yield {
      val vision = named(s"Vision-${ names(i) }") {
        lift(forks(i), forks(mod(i - 1)))(calcVision(names(i)))
      }
      Seating(i, phils(i), forks(i), forks(mod(i - 1)), vision)
    }
  }

  val seatings = createTable(size)

  // ============================================== Logging =======================================================

  def log(msg: String): Unit = {
    println("[" + Thread.currentThread().getName + " @ " + System.currentTimeMillis() + "] " + msg)
  }
  def log[A](reactive: Pulsing[A]): Unit = {
    Observe(reactive) { value =>
      log(reactive + " now " + value)
    }
  }

//  seatings.foreach { seating =>
//    named(s"observePhil(${ names(seating.placeNumber) })")(log(seating.philosopher))
//    named(s"observeFork(${ names(seating.placeNumber) })")(log(seating.leftFork))
//    // right fork is the next guy's left fork
//    named(s"observeVision(${ names(seating.placeNumber) })")(log(seating.vision))
//  }


  val eaten = new AtomicInteger(0)
  @volatile var lastTime = System.nanoTime()

  seatings.foreach { seating =>
    seating.vision.observe { state =>
      if (state == Eating) {
        val eats = eaten.incrementAndGet()
        if (eats % 1000 == 0) {
          val time = System.nanoTime()
          log(s"eaten: $eats in ${ (time - lastTime) / 1000000 }ms (${ SyncUtil.counter.get() / eats }tpe)")
          lastTime = time
        }
      }
    }
  }

  // ============================================ Runtime Behavior  =========================================================

  seatings foreach { case Seating(i, philosopher, _, _, vision) =>
    named(s"think-${ names(i) }")(vision.observe { state =>
      if (state == Eating) Future { philosopher set Thinking }
    })
  }

  @annotation.tailrec // unrolled into loop by compiler
  def repeatUntilTrue(op: => Boolean): Unit = if (!op) repeatUntilTrue(op)

  def tryEat(seating: Seating) =
    DependentUpdate(seating.vision) { _ == Ready } { turn =>
      seating.philosopher.admit(Hungry)(turn)
      true // Don't try again
    } {
      false // Try again
    }

  def eatOnce(seating: Seating) = repeatUntilTrue(tryEat(seating))

  // ============================================== Thread management =======================================================

  object Spawn {
    def apply(name: String)(f: => Unit): Thread = {
      val t = new Thread(new Runnable {
        override def run(): Unit = f
      }, name)
      t.start()
      t
    }
  }

  // ===================== STARTUP =====================
  // start simulation
  @volatile private var killed = false
  log("Starting simulation. Press <Enter> to terminate!")
  val threads = seatings.map { seating =>
    val phil = seating.philosopher
    phil ->
      Spawn("Worker-" + names(seating.placeNumber)) {
        log("Controlling hunger on " + seating)
        while (!killed) {
          eatOnce(seating)
        }
        log(phil + " dies.")
      }
  }

  // ===================== SHUTDOWN =====================
  // wait for keyboard input
  System.in.read()

  // kill all philosophers
  log("Received Termination Signal, Terminating...")
  killed = true

  // collect forked threads to check termination
  threads.foreach { case (phil, thread) =>
    import scala.language.postfixOps
    thread.join(50)
    if (!thread.isAlive) log(phil + " terminated.")
    else log(phil + " failed to terminate!")
  }
}
