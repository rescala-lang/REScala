package animal.versions.observer

import animal.types.Pos
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.events.ImperativeEvent
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import scala.util.Random
import scala.events.Event


object Board {
  def square(range: Int) = for(x <- -range to range; y <- -range to range) yield (x,y)  
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
  def isFree(pos: (Int, Int)) = ! elements.contains(pos)
  
  /** clears the current element from pos */
  def clear(pos: (Int, Int)) = elements.remove(pos)
  
  /** @return the nearest free position to pos */
  def nearestFree(pos: (Int, Int)) = Board.proximity(pos, 1).find(isFree)
  
  /** moves pos in direction dir if possible (when target is free) */
  def moveIfPossible(pos: Pos, dir: Pos){
    val newPos = pos + dir
    if(isFree(newPos) && !isFree(pos)){
      val e = clear(pos)
      add(e.get, newPos)
    }
  }
  
  /** @return the position of the given BoardElement. slow. */
  def getPosition(be: BoardElement) = {
    elements.collectFirst { _ match {
      case (pos, b) if b == be => pos
    }}
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
      case Some(x: Animal) => 'x'
      case Some(p: Plant) => '#'
      case Some(_) => '?'
    }
    val lines = for(y <- 0 to height)
      yield (0 to width).map(x => repr(elements.get(x,y))).mkString
     lines.mkString("\n")
  }
}


abstract class BoardElement(implicit val world: World) {

  /** A board element broadcasts a died event when it dies*/
  var diesObservers: List[(Unit => Unit)] = Nil
  
  def registerDiesObserver(obs: (Unit => Unit)){
   diesObservers = obs :: diesObservers
  }
  
  def unregisterDiesObserver(obs: (Unit => Unit)){
   diesObservers = diesObservers - obs
  }
  
  /** handlers */
  val tickHandler: (Unit => Unit)
  val dailyHandler: (Unit => Unit)
  
  /** Some imperative code that is called each tick */
  def doStep(pos: Pos) {}
}

object Animal {
	val StartEnergy = 100
	val MaxAge = 100
	val EatRate = 3
	val ViewRadius = 9
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
	var energyDrain = 0
	var energyGain = 0
	var isDead = false
	
	
	def isEating = state match { 
		  case Eating(_) => true
		  case _ => false
	}
	
	// handle updates
	val tickHandler = { _: Unit =>
		 energyGain =  if(isEating) Animal.EatRate else 0
		 energy += energyGain
		 energy -= energyDrain
		 
		 if(!isDead && age > Animal.MaxAge || energy < 0) {
		 	diesObservers.foreach(f => f())
		 	isDead = true
		 }
	}
	
	// handle aging
	val dailyHandler = { _: Unit =>
	  age += 1
	  energyDrain = 1 + age / 2
	}
	
	private def setAction(pos: Pos) {
		    val findPlant: PartialFunction[BoardElement, Plant] =  { case p: Plant => p }
			val neighbors = world.board.neighbors(pos)
			val food = neighbors.collectFirst(findPlant)
			
			val nextAction: AnimalState = food match {
			  case Some(plant) => Eating(plant) // I'm near a plant, eat it!
			  case None => // I have to look for plants nearby
			    world.board.nearby(pos, Animal.ViewRadius).collectFirst(findPlant) match {
			      case Some(target) => 
			        val destination = world.board.getPosition(target)
			        if(destination.isDefined) Moving(pos.directionTo(destination.get)) 
			        else Idling
			      case None => Idling
			    }
			}
			// set the new action
			state = nextAction
	  	}
	
	/** imperative 'AI' function */
	override def doStep(pos: Pos) {	
	    state match {
	      case Moving(dir) => world.board.moveIfPossible(pos, dir)
	      case Eating(plant) => plant.takeEnergy(energyGain)
	      case Idling =>
	    }
	    
	    setAction(pos)
	}
}



class Plant(override implicit val world: World) extends BoardElement {

  var energy = 100
  var isDead = false
  
  val tickHandler = {_: Unit => }
  val dailyHandler = {_: Unit => }
  
  /** takes amount away from the energy of this plant */
  def takeEnergy(amount: Int) {
    energy -= amount
    if(energy <= 0){
      diesObservers.foreach(f => f())
	  isDead = true
    }
  }
  
}

class Time {
  // there are better solutions to handle multiple observation events,
  // but this is closest to the event/signal implementations
  
  var hours = 0
  var hour = 0
  var day = 0
  var week = 0
  var timestring = ""  
    
  def tick {
    
    hours += 1
    hour = hours % 24
    day = hours / 24
    week = day / 7
    
    timestring = "Week " + week + ", Day " + day + " hour:" + hour
    
    // trigger observers
    tickObservers.foreach(f => f())
    
    if(hour == 0)
      dailyObservers.foreach(f => f())
      
    if(hour == 0 && day % 7 == 0)
      weeklyObservers.foreach(f => f())
  }

  
  private var tickObservers: List[(Unit => Unit)] = Nil
  private var dailyObservers: List[(Unit => Unit)] = Nil
  private var weeklyObservers: List[(Unit => Unit)] = Nil
  
  def registerTickObserver(obs: (Unit => Unit)){
   tickObservers = obs :: tickObservers
  }
  
  def registerDayChangedObserver(obs: (Unit => Unit)){
    dailyObservers = obs :: dailyObservers
  }
  
  def registerWeekChangedObserver(obs: (Unit => Unit)){
    weeklyObservers = obs :: weeklyObservers
  }
  
  def unregisterTickObserver(obs: (Unit => Unit)){
   tickObservers = tickObservers - obs
  }
  
  def unregisterDayChangedObserver(obs: (Unit => Unit)){
    dailyObservers =  dailyObservers - obs
  }
  
  def unregisterWeekChangedObserver(obs: (Unit => Unit)){
    weeklyObservers =  weeklyObservers - obs
  }
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
  
  def tick = time.tick
  def dump = board.dump
  def timestring = time.timestring
  
  /** batch spawns n Animals and m Plants */
  def batchSpawn(nAnimals: Int, mPlants: Int) {
    for(_ <- 1 to nAnimals) spawn(new Animal)
    for(_ <- 1 to mPlants) spawn(new Plant)
  }
  
  /** spawns the given Board element at a free random position in the world */
  def spawn(element: BoardElement) {
    board.add(element, board.randomFreePosition(randomness))
    
    // register handlers
    time.registerTickObserver(element.tickHandler)
    time.registerDayChangedObserver(element.dailyHandler)
    
    // register unspawning
    
    def diesHandler(a: Unit) {
      unspawn(element)
      element.unregisterDiesObserver(diesHandler)
    }    
    element.registerDiesObserver(diesHandler)
  }
  
  /** removed the given Board element from the world */
  def unspawn(element: BoardElement) {
    // unregister handlers
    time.unregisterTickObserver(element.tickHandler)
    time.unregisterDayChangedObserver(element.dailyHandler)
    board.remove(element)
  }
  
  // tick board elements
  time.registerTickObserver {_ =>
    board.elements.foreach { _ match {
      	case (pos, be) => be.doStep(pos)
      }
    }
  }
  
  // each day, spawn a new plant
  time.registerDayChangedObserver {_ =>
    this spawn new Plant
  }
  
  //each week, spawn a new animal
  time.registerWeekChangedObserver {_ =>
    this spawn new Animal
  }
}
