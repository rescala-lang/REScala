package animal


import rescala._
import rescala.turns.Engines.default

import scala.util.Random

object World {
  val Width = 30
  val Height = 10
}

/**
 * A World object unites a space (Board object), time (Time object), and a random object
 * It also defines all repetitive actions, such as spawning new Animals and Plants
 */
class World {

  implicit val world = this

  val board = new Board(World.Width, World.Height)
  val time = new Time
  val randomness = new Random(1)

  val statusString: Signal[String] = Signals.lift(board.animalsAlive, board.animalsBorn) { (a, b) =>
    s"Animals alive: $a Total born: $b"
  }
  def status = statusString.now

  def tick() = time.tick(Unit)

  def newAnimal(isHerbivore: Boolean, isMale: Boolean): Animal = {
    if (isHerbivore) {
      if (isMale) new MaleHerbivore else new FemaleHerbivore
    }
    else {
      if (isMale) new MaleCarnivore else new FemaleCarnivore
    }
  }

  /** returns an animal at random */
  def newAnimal: Animal = newAnimal(randomness.nextBoolean(), randomness.nextBoolean())

  /** batch spawns n Animals and m Plants */
  def batchSpawn(nAnimals: Int, mPlants: Int): Unit = {
    for (_ <- 1 to nAnimals) spawn(newAnimal)
    for (_ <- 1 to mPlants) spawn(new Plant)
  }

  /** spawns the given board element at the given position */
  def spawn(element: BoardElement, pos: Pos) = board.add(element, pos)

  /** spawns the given Board element at a free random position in the world */
  def spawn(element: BoardElement): Unit = {
    spawn(element, board.randomFreePosition(randomness))
  }

  // tick / clear board elements
  time.tick += { x => //#HDL //#IF
    board.elements.foreach {
      case (pos, be) =>
        if (be.isDead.now)
          board.remove(pos)
        else be.doStep(pos)
    }
  }

  // each day, spawn a new plant
  time.day.changed += { _ => //#HDL //#IF
    this spawn new Plant
  }

  //each week, spawn a new animal
  time.week.changed += { _ => //#HDL  //#IF
    this spawn newAnimal
  }
}
