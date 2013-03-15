package animal.versions.signal

import animal.types.Pos
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.events.ImperativeEvent
import scala.events.behaviour._
import scala.util.Random
import animal.types.Pos.fromTuple
import scala.Option.option2Iterable


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
  def neighbors(pos: (Int, Int)) = nearby(pos, 1)
  
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
      case Some(m: Male) if m.isAdult.getValue => 'm'
      case Some(f: Female) if f.isAdult.getValue => if (f.isPregnant.getValue) 'F' else 'f'
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
  /** A signal denoting if this element is dead ( = should be removed from the board) */
  val isDead: Signal[Boolean]
  /// actually, you would want to register an event handler right here, so that
  /// an element that changes its state to dead could remove itself.
  /// However, we have no control where in the 'update cycle' that happens.
  /// If some other reaction happens after removing this element, but in the same cycle
  /// the element is invalid
  
  /** Some imperative code that is called each tick */
  def doStep(pos: Pos) {}
  /// This method exists so that i have have some imperative code that can
  /// be called on each tick. This was necessary for some parts which don't work at all with events
  /// If we were to disallow events completely, the size of this method would grow
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

abstract class Animal(override implicit val world: World) extends BoardElement {
  
	/** An animal is in a state */
	trait AnimalState
	case object Idling extends AnimalState
	case class Eating(plant: Plant) extends AnimalState
	case class Moving(dir: Pos) extends AnimalState
	case class Procreating(female: Animal) extends AnimalState
 
	val state: Var[AnimalState] = Var(Idling)
	/// I have tried to keep the number of Vars as small as possible.
	/// 'state' is one of the few properties which has to be a Var
	/// because its hard to define as dependent on other signals.
	/// (although functionally dependent on pos and current world state (see nextAction))
	
	protected def nextAction (pos: Pos): AnimalState =  {
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
		        else randomMove
		      case None => randomMove
		    }
		}
		nextAction
	}
	
	protected def randomMove: AnimalState = {
	   val randx = world.randomness.nextInt(2)
	   val randy = world.randomness.nextInt(2)
	   Moving(Pos(randx, randy))
	}

	
	val age = world.time.day.changed.iterate(1)(_ + 1)
	
	val isAdult =  Signal { age() > Animal.FertileAge }
	val isFertile = Signal { isAdult() }
	val isEating = Signal { state() match { 
	  case Eating(_) => true
	  case _ => false}
	}
	
	val energyDrain = Signal { 1 + age() / 2}
	val energyGain = Signal { if(isEating()) Animal.EatRate else 0}
	val energy = world.time.tick.iterate(Animal.StartEnergy)(_ + energyGain.getValue - energyDrain.getValue)
	
	override val isDead = Signal { age() > Animal.MaxAge || energy() < 0}
	
	/** imperative 'AI' function */
	override def doStep(pos: Pos) {
	    state.getValue match {
	      case Moving(dir) => world.board.moveIfPossible(pos, dir)
	      case Eating(plant) => plant.takeEnergy(energyGain.getValue)
	      case Procreating(female: Female) => female.procreate(this)
	      case Idling =>
	    }
	    state.update(nextAction(pos))
	}
}


class Female(override implicit val world: World) extends Animal {
  /// This is the intended definition for procreation, in which we 'reset' a signal
  /// pregnancy time, and listen to the 'changedTo(0)' event.
  /// Not working yet, because nested signals seem not to be working in Scala.react
  /*
  val becomePregnant = new ImperativeEvent[Unit]
  
  val pregnancyTime: Signal[Int] = becomePregnant.reset(()){ _ =>
    world.time.day.changed.iterate(Animal.PregnancyTime)(_ - 1)
  }

  val giveBirth = pregnancyTime.changedTo(0)
  val isPregnant = Signal { pregnancyTime() < 0 }
  */
  
  val isPregnant = Var(false)
  val pregnancyTime = Var(0)
  /// Workaround for issue above: Stateful signals
  
  val incrementer =  {_: Int =>
     if(isPregnant.getValue){
    	pregnancyTime.update(pregnancyTime.getValue + 1)
    }
  }
  /// If we want to use signals here, we have to keep a reference to our handler
  /// And (re-/dis-)attach it when we want the timer to increase.
  /// This is a kinda forced application of reactivity.
  /// It would be much simpler to put this operation in the imperative 'doStep' method
  
  override val isFertile = Signal { isAdult() && !isPregnant()}
  /// Here we have an example of overriding a signal
  
  val giveBirth = pregnancyTime.changed && {_ == Animal.PregnancyTime}
  giveBirth += {_ =>
    world.board.getPosition(this).foreach{ mypos =>
      world.board.nearestFree(mypos).foreach { target =>
        world.spawn(world.newAnimal, target)
      }
    }
    isPregnant.update(false)
    world.time.hour.changed -= incrementer
  }
  
  def procreate(father: Animal) {
    if(isPregnant.getValue) return;
    
    isPregnant() = true
    pregnancyTime() = 0
    
    world.time.hour.changed += incrementer
  }
  
}


class Male(override implicit val world: World) extends Animal {
  val seeksMate = Signal { isFertile() && energy() > Animal.HungryThreshold }
  
  override def nextAction(pos: Pos): AnimalState = {
    if(seeksMate.getValue) {
        val findFemale: PartialFunction[BoardElement, Female] =  { 
          case f: Female if f.isFertile.getValue => f 
        }
		val neighbors = world.board.neighbors(pos)
		val females = neighbors.collectFirst(findFemale)
		
		val nextAction: AnimalState = females match {
		  case Some(female) => Procreating(female)
		  case None => // I have to look for females nearby
		    world.board.nearby(pos, Animal.ViewRadius).collectFirst(findFemale) match {
		      case Some(target) => 
		        val destination = world.board.getPosition(target)
		        if(destination.isDefined) Moving(pos.directionTo(destination.get)) 
		        else super.nextAction(pos)
		      case None => super.nextAction(pos)
		    }
		}
		nextAction
    }
    else super.nextAction(pos)
  }
}



class Plant(override implicit val world: World) extends BoardElement {
  val energy = Var(100)
  val isDead = Signal { energy() <= 0}
  
  /** takes amount away from the energy of this plant */
  def takeEnergy(amount: Int) = energy.update(energy.getValue - amount)
  
}

class Time {
  val tick = new ImperativeEvent[Unit]

  val hours = tick.iterate(0)(_ + 1)
  val day = Signal { hours() / 24 }
  val hour = Signal { hours() % 24}
  val week = Signal { day() / 7}
  val timestring = Signal { "Week " + week() + ", Day " + day() + " hour:" + hour()}
  val newWeek = week.changed
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
  val randomness = new Random(123)
  
  def tick = time.tick()
    
  def dump = board.dump
  def timestring = time.timestring.getValue
  
  
  /** returns a male or female animal at random */
  def newAnimal: Animal = if(randomness.nextBoolean) new Male else new Female
  
  /** batch spawns n Animals and m Plants */
  def batchSpawn(nAnimals: Int, mPlants: Int) {
    for(_ <- 1 to nAnimals) spawn(newAnimal)
    for(_ <- 1 to mPlants) spawn(new Plant)
  }
  
  /** spawns the given board element at the given position */
  def spawn(element: BoardElement, pos: Pos) {
     board.add(element, pos)
  }
  
  /** spawns the given Board element at a free random position in the world */
  def spawn(element: BoardElement){ 
    spawn(element,  board.randomFreePosition(randomness))
  }
  
  // tick / clear board elements
  time.hour.changed += {x  =>    
    board.elements.foreach { _ match {
      	case (pos, be) =>
      	  if(be.isDead.getValue)
      	    board.clear(pos)
      	  else be.doStep(pos)
      }
    }
  }
  
  
  // each day, spawn a new plant
  time.day.changed += {_ =>
    this spawn new Plant
    this spawn new Plant
  }
  
  //each week, spawn a new animal
  time.week.changed += {_ =>
    this spawn newAnimal
  }
}

