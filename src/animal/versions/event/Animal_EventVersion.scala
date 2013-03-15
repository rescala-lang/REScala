package animal.versions.event

import animal.types.Pos
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.events.ImperativeEvent
import scala.util.Random
import scala.events.Event
import animal.types.Pos.fromTuple
import scala.Option.option2Iterable

object Board {
  def square(range: Int) = for (x <- -range to range; y <- -range to range) yield (x, y)
  def proximity(pos: Pos, range: Int) = square(range).map(pos + _).sortBy(pos.distance(_))
}

/**
 * Mutable data structure which stores board elements in 2-dimensional coordinates.
 * A Board is infinite, but width and height specify the area being displayed.
 */
class Board(val width: Int, val height: Int) {
  val elements: Map[(Int, Int), BoardElement] = new HashMap

  /** adds a board element at given position */
  def add(be: BoardElement, pos: (Int, Int)) {
    elements.put(pos, be)
  }

  /** removes the board element if present in the board */
  def remove(be: BoardElement) = getPosition(be).foreach(clear(_))

  /** @return the elements in this board nearby pos */
  def nearby(pos: (Int, Int), range: Int) = Board.proximity(pos, range).map(elements.get).flatten

  /** @return the immediate neighbors of the given position */
  def neighbors(pos: (Int, Int)) = nearby(pos, 2)

  /** @return true if pos is free */
  def isFree(pos: (Int, Int)) = !elements.contains(pos)

  /** clears the current element from pos */
  def clear(pos: (Int, Int)) = elements.remove(pos)

  /** @return the nearest free position to pos */
  def nearestFree(pos: (Int, Int)) = Board.proximity(pos, 1).find(isFree)

  /** moves pos in direction dir if possible (when target is free) */
  def moveIfPossible(pos: Pos, dir: Pos) {
    val newPos = pos + dir
    if (isFree(newPos) && !isFree(pos)) {
      val e = clear(pos)
      add(e.get, newPos)
    }
  }

  /** @return the position of the given BoardElement. slow. */
  def getPosition(be: BoardElement) = {
    elements.collectFirst {
      _ match {
        case (pos, b) if b == be => pos
      }
    }
  }

  /** @return a random free position on this board */
  def randomFreePosition(random: Random) = {
    val freeSlots = width * height - elements.size
    val rIndex = random nextInt freeSlots
    (rIndex % width, rIndex / width)
  }

  /** @return textual representation for drawing this board to console */
  def dump: String = {
    def repr(be: Option[BoardElement]) = be match {
      case None => '.'
      case Some(m: Male) if m.isAdult => 'm'
      case Some(f: Female) if f.isAdult => if (f.isPregnant) 'F' else 'f'
      case Some(x: Animal) => 'x'
      case Some(p: Plant) => '#'
      case Some(_) => '?'
    }
    val lines = for (y <- 0 to height)
      yield (0 to width).map(x => repr(elements.get(x, y))).mkString
    lines.mkString("\n")
  }
}

abstract class BoardElement(implicit val world: World) {
  def isDead: Boolean

  /** event that gets fired when this element dies (should be removed) */
  val dies: Event[Unit]

  /** handlers */
  val tickHandler: (Unit => Unit)
  val dailyHandler: (Unit => Unit)

  /** Some imperative code that is called each tick */
  def doStep(pos: Pos) {}
}

object Animal {
  val ViewRadius = 9
  val StartEnergy = 100
  val MaxAge = 100
  val EatRate = 3
  val HungryThreshold = 10
  val FertileAge = 1
  val PregnancyTime = 30
}

class Animal(override implicit val world: World) extends BoardElement {

  /** An animal is in a state */
  trait AnimalState
  case object Idling extends AnimalState
  case class Eating(plant: Plant) extends AnimalState
  case class Moving(dir: Pos) extends AnimalState
  case class Procreating(female: Animal) extends AnimalState

  // mutable vars
  var state: AnimalState = Idling
  var age = 0
  var energy = Animal.StartEnergy
  var isDead = false

  def isAdult = age > Animal.FertileAge
  def isFertile = isAdult
  def isEating = state match {
    case Eating(_) => true
    case _ => false
  }
  /// A lot of what is modeled as signals in the signal version, makes no sense to model as a
  /// mutable var / handler pair in events.
  /// These two properties actually make a lot more sense to implement as functions.
  /// (because we don't need to react to changes of these values, but simply query them)

  // dies event, gets triggered once at the moment this animal dies
  val dies = world.time.tick && (_ => !isDead && age > Animal.MaxAge || energy < 0)

  // handler that makes sure the dies event gets only fired once
  dies += { _ => isDead = true }

  // handle updates
  val tickHandler = { _: Unit =>
    val energyGain = if (isEating) Animal.EatRate else 0
    val energyDrain = 1 + age / 2
    energy += energyGain
    energy -= energyDrain
  }

  // handle aging
  val dailyHandler = { _: Unit =>
    age += 1
  }

  protected def nextAction(pos: Pos): AnimalState = {
    val findPlant: PartialFunction[BoardElement, Plant] = { case p: Plant => p }
    val neighbors = world.board.neighbors(pos)
    val food = neighbors.collectFirst(findPlant)

    food match {
      case Some(plant) => Eating(plant) // I'm near a plant, eat it!
      case None => // I have to look for plants nearby
        world.board.nearby(pos, Animal.ViewRadius).collectFirst(findPlant) match {
          case Some(target) =>
            val destination = world.board.getPosition(target)
            if (destination.isDefined) Moving(pos.directionTo(destination.get))
            else randomMove
          case None => randomMove
        }
    }
  }

  protected def randomMove: AnimalState = {
    val randx = world.randomness.nextInt(2)
    val randy = world.randomness.nextInt(2)
    Moving(Pos(randx, randy))
  }

  /** imperative 'AI' function */
  override def doStep(pos: Pos) {
    state match {
      case Moving(dir) => world.board.moveIfPossible(pos, dir)
      case Eating(plant) => plant.takeEnergy(if(isEating) Animal.EatRate else 0)
      case Procreating(female: Female) => female.procreate(this)
      case Idling =>
    }
    state = nextAction(pos)
  }
}

class Female(override implicit val world: World) extends Animal {

  var isPregnant = false
  var pregnancyTime = 0

  override def isFertile = isAdult && !isPregnant
  
  val giveBirth = world.time.tick && (_ => pregnancyTime == Animal.PregnancyTime)
  giveBirth += {_: Unit =>
    isPregnant = false
    world.board.getPosition(this).foreach{ mypos =>
      world.board.nearestFree(mypos).foreach { target =>
        world.spawn(world.newAnimal, target)
      }
    }
  }

  world.time.tick && (_ => isPregnant) += { _: Unit =>
    pregnancyTime += 1
  }
  

  def procreate(father: Animal) {
    isPregnant = true
    pregnancyTime = 0
  }
}

class Male(override implicit val world: World) extends Animal {
  def seeksMate = isFertile && energy > Animal.HungryThreshold

  override def nextAction(pos: Pos): AnimalState = {
    if (seeksMate) {
      val findFemale: PartialFunction[BoardElement, Female] = {
        case f: Female if f.isFertile => f
      }
      val neighbors = world.board.neighbors(pos)
      val females = neighbors.collectFirst(findFemale)

      val nextAction: AnimalState = females match {
        case Some(female) => Procreating(female)
        case None => // I have to look for females nearby
          world.board.nearby(pos, Animal.ViewRadius).collectFirst(findFemale) match {
            case Some(target) =>
              val destination = world.board.getPosition(target)
              if (destination.isDefined) Moving(pos.directionTo(destination.get))
              else super.nextAction(pos)
            case None => super.nextAction(pos)
          }
      }
      nextAction
    } else super.nextAction(pos)
  }
}

class Plant(override implicit val world: World) extends BoardElement {

  var energy = 100
  var isDead = false

  // dies event, gets triggered once
  val dies = world.time.tick && (_ => !isDead && energy < 0)

  // handler that makes sure the dies event gets only fired once
  dies += { _ => isDead = true }

  val tickHandler = { _: Unit => }
  val dailyHandler = { _: Unit => }

  /** takes amount away from the energy of this plant */
  def takeEnergy(amount: Int) = energy -= amount

}

class Time {
  val tick = new ImperativeEvent[Unit]

  var hours = 0
  var hour = 0
  var day = 0
  var week = 0
  var timestring = ""

  // imperative update of mutable values
  tick += { _ =>
    hours += 1
    hour = hours % 24
    day = hours / 24
    week = day / 7

    timestring = "Week " + week + ", Day " + day + " hour:" + hour
  }

  // filter event stream
  val dayChanged = tick && (_ => hour == 0)
  val weekChanged = dayChanged && (_ => day % 7 == 0)
}

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
  val randomness = new Random

  def tick = time.tick()
  def dump = board.dump
  def timestring = time.timestring

  /** batch spawns n Animals and m Plants */
  def batchSpawn(nAnimals: Int, mPlants: Int) {
    for (_ <- 1 to nAnimals) spawn(newAnimal)
    for (_ <- 1 to mPlants) spawn(new Plant)
  }

  /** returns a male or female animal at random */
  def newAnimal: Animal = if (randomness.nextBoolean) new Male else new Female
  
  
  /** spawns the given Board element at a free random position in the world */
  def spawn(element: BoardElement){ 
    spawn(element,  board.randomFreePosition(randomness))
  }

  /** spawns the given Board element at a free random position in the world */
  def spawn(element: BoardElement, pos: Pos) {
    board.add(element, pos)

    // register handlers
    time.tick += element.tickHandler
    time.dayChanged += element.dailyHandler

    // register unspawning
    element.dies += { _ => unspawn(element) }
    /// In the event version, we need this method, because we
    /// need to explicitly remove all handlers (see unspawn)
  }

  /** removed the given Board element from the world */
  def unspawn(element: BoardElement) {
    // unregister handlers
    time.tick -= element.tickHandler
    time.dayChanged -= element.dailyHandler
    board.remove(element)
    /// This is really ugly, we need to know what events the board elements have attached to the
    /// outside. In this case, we simply allow one handler per tick, and one that gets triggered on day changes
  }

  // tick board elements
  time.tick += { _ =>
    board.elements.foreach {
      _ match {
        case (pos, be) => be.doStep(pos)
      }
    }
  }

  // each day, spawn a new plant
  time.dayChanged += { _ =>
    this spawn new Plant
    this spawn new Plant
  }

  //each week, spawn a new animal
  time.weekChanged += { _ =>
    this spawn newAnimal
  }

}
