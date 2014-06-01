package animal.versions.observer

import animal.types.Pos
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import rescala.events.ImperativeEvent
import rescala.Signal
import rescala.Var
import scala.util.Random
import commons.Observable

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
  val allPositions = (for(x <- 0 to width; y <- 0 to height) yield (x, y)).toSet
  
  /** adds a board element at given position */
  def add(be: BoardElement, pos: (Int, Int)) {
    elements.put(pos, be)
  }
  
  /** removes the board element if present in the board */
  def remove(be: BoardElement): Unit = getPosition(be).foreach(remove(_))
  def remove(pos: (Int, Int)) = {
    val e = elements.remove(pos)
  }
  
  /** @return the elements in this board nearby pos */
  def nearby(pos: (Int, Int), range: Int) = Board.proximity(pos, range).map(elements.get).flatten
  
  /** @return the immediate neighbors of the given position */
  def neighbors(pos: (Int, Int)) = nearby(pos, 1)
  
  /** @return true if pos is free */ 
  def isFree(pos: (Int, Int)) = ! elements.contains(pos)
  
  /** clears the current element from pos */
  private def clear(pos: (Int, Int)) = elements.remove(pos)
  
  /** @return the nearest free position to pos */
  def nearestFree(pos: (Int, Int)) = Board.proximity(pos, 1).find(isFree)
  
  /** moves pos in direction dir if possible (when target is free) */
  def moveIfPossible(pos: Pos, dir: Pos){
    val newPos = pos + dir
    if(isFree(newPos) && !isFree(pos)){
      val e = clear(pos)
      elements.put(newPos, e.get)
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
    val possiblePositions = allPositions.diff(elements.keySet).toVector
    possiblePositions(random.nextInt(possiblePositions.length))
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
    val lines = for(y <- 0 to height)
      yield (0 to width).map(x => repr(elements.get(x,y))).mkString
     lines.mkString("\n")
  }
}


abstract class BoardElement(implicit val world: World) {
  def isDead: Boolean
  
  /** A board element broadcasts a died event when it dies*/
  var dies = Observable[Unit](())
  
  var diesObservers: List[(Unit => Unit)] = Nil
  
  def registerDiesObserver(obs: (Unit => Unit)){
   diesObservers = obs :: diesObservers
  }
  
  def unregisterDiesObserver(obs: (Unit => Unit)){
   diesObservers = diesObservers.filterNot(_ == obs)
  }
  
  /** handlers */
  var tickHandlers: List[(Unit => Unit)] = Nil
  var dailyHandlers: List[(Unit => Unit)] = Nil
  
  /** Some imperative code that is called each tick */
  def doStep(pos: Pos) {}
}

object Animal {
	val StartEnergy = 200
  	val ViewRadius = 9 // radius that animals can see the world around them
	val MoveCost = 1 // energy required to move
	val ProcreateCost = 10 // energy required to procreate
	val MaxAge = 100 // maximum age in days when an animal dies, regardless of energy
	val PlantEatRate = 3 // energy rate gained when eating plants
	val ProcreateThreshold = 60 // minimum energy required for male animals to seek a mate
	val FertileAge = 1 // minimum age in days for animals to be fertile
	val PregnancyTime = 30 // time in hours for female sheep to be pregnant
	val AttackThreshold = 100 // minimum energy for carnivores to attack
	val AttackAmount = 50 // energy stolen when carnivores attack
	val SleepThreshold = 30 // minimum energy for carnivores to start sleeping 
	val SleepRate = 2 // energy gained while sleeping
}

abstract class Animal(override implicit val world: World) extends BoardElement {
  
	/** An animal is in a state */
	trait AnimalState
	case object Idling extends AnimalState
	case class Eating(plant: Plant) extends AnimalState
	case class Attacking(other: Animal) extends AnimalState
	case class Moving(dir: Pos) extends AnimalState
	case class Procreating(female: Animal) extends AnimalState
	case object FallPrey extends AnimalState
	case object Sleeping extends AnimalState
	
	// mutable vars
	var state: AnimalState = Idling
	var age = 0
	var energy = Animal.StartEnergy
	var isDead = false
	var energyDrain = 0
	var energyGain = 0
	
	def isAdult = age > Animal.FertileAge
    def isFertile = isAdult	
	
	def isEating = state match { 
		  case Eating(_) => true
		  case _ => false
	}
	
	def findFood: PartialFunction[BoardElement, BoardElement]
    def reachedState(target: BoardElement): AnimalState
    
    def savage = state = FallPrey
	
	// handle updates
	tickHandlers ::= { _: Unit =>
	  
      energyGain = state match {
	    case Eating(_) => Animal.PlantEatRate
	    case Sleeping => Animal.SleepRate
	    case Attacking(prey) => Animal.AttackAmount
	    case _ => 0
	  }
    
     energyDrain = 1 + age / 2 + (state match {
	    case Moving(_) => Animal.MoveCost
	    case Procreating(_) => Animal.ProcreateCost
	    case FallPrey => Animal.AttackAmount
	    case _ => 0
	  }) 	  
	  
    energy += energyGain
    energy -= energyDrain
		 
	 if(!isDead && age > Animal.MaxAge || energy < 0) {
	 	diesObservers.foreach(f => f())
	 	isDead = true
	 }
	}
	
	// handle aging
	dailyHandlers ::= { _: Unit =>
	  age += 1
	  
	  if(!isDead && age > Animal.MaxAge || energy < 0) {
	 	diesObservers.foreach(f => f())
	 	isDead = true
	 }
	}
	
	protected def nextAction(pos: Pos): AnimalState =  {
		val neighbors = world.board.neighbors(pos)
		val food = neighbors.collectFirst(findFood)
		val nextAction: AnimalState = food match {
		  case Some(target) => reachedState(target) // I'm near food, eat it!
		  case None => // I have to look for food nearby
		    world.board.nearby(pos, Animal.ViewRadius).collectFirst(findFood) match {
		      case Some(target) => 
		        val destination = world.board.getPosition(target)
		        if(destination.isDefined) 
		          Moving(pos.directionTo(destination.get)) 
		        else 
		          randomMove
		      case None => randomMove
		    }
		}
		nextAction
	}

  protected def randomMove: AnimalState = {
    val randx = 1 - world.randomness.nextInt(3)
    val randy = 1 - world.randomness.nextInt(3)
    Moving(Pos(randx, randy))
  }

	/** imperative 'AI' function */
	override def doStep(pos: Pos) {
	    state match {
	      case Moving(dir) => world.board.moveIfPossible(pos, dir)
	      case Eating(plant) => plant.takeEnergy(if(isEating) Animal.PlantEatRate else 0)
	      case Attacking(prey) => prey.savage
	      case Procreating(female: Female) => female.procreate(this)
	      case _ =>
	    }
	    state = nextAction(pos)
	}
}


class Carnivore(override implicit val world: World) extends Animal {
  
  def sleepy = energy < Animal.SleepThreshold
  def canHunt = energy > Animal.AttackThreshold
	
  // only adult carnivores with min energy can hunt, others eat plants
  def findFood: PartialFunction[BoardElement, BoardElement] = 
     if(isAdult && canHunt) { case p: Herbivore => p}
     else { case p: Plant => p }
    

  override def reachedState(prey: BoardElement): AnimalState = prey match {
    case p: Herbivore => Attacking(p)
    case _ => Idling
  }
  
  
  override protected def nextAction(pos: Pos): AnimalState =  {
	  if(sleepy) Sleeping
	  else super.nextAction(pos)
  }
}

class Herbivore(override implicit val world: World) extends Animal {
  
  val findFood: PartialFunction[BoardElement, BoardElement] =
    { case p: Plant => p }
  
  override def reachedState(plant: BoardElement): AnimalState = plant match {
    case p: Plant => Eating(p)
    case _ => Idling
  }
}


trait Female extends Animal {
  
  var mate: Option[Animal] = None  
  var pregnancyTime = 0  
  def isPregnant =  mate.isDefined
  override def isFertile = isAdult && !isPregnant 
  
  
   tickHandlers ::= {_: Unit => 
    if(isPregnant)
      pregnancyTime += 1
      
    if(pregnancyTime == Animal.PregnancyTime){
      // give birth
      val father = mate.get
	  val child = createOffspring(father)
	  world.board.getPosition(this).foreach{ mypos =>
	    world.board.nearestFree(mypos).foreach { target =>
	       world.spawn(child, target)
	    }
	  }
      pregnancyTime = 0
	  mate = None
    }
  }
  
  
  
  def procreate(father: Animal) {
    mate = Some(father)
    pregnancyTime = 0
  }
  
  
   def createOffspring(father: Animal): Animal = {
      val male = world.randomness.nextBoolean
  	  val nHerbivores = List(this, father).map(_.isInstanceOf[Herbivore]).count(_ == true)
  	  val herbivore = 
  	    if (nHerbivores == 0) false // both parents are a carnivores, child is carnivore
  	    else if (nHerbivores == 2) true // both parents are herbivores, child is herbivore
  	    else world.randomness.nextBoolean // mixed parents, random
  	  
  	  world.newAnimal(herbivore, male)
  }
}

trait Male extends Animal {
  def seeksMate = isFertile && energy > Animal.ProcreateThreshold

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



class FemaleHerbivore(override implicit val world: World) extends Herbivore with Female
class MaleHerbivore(override implicit val world: World) extends Herbivore with Male
class FemaleCarnivore(override implicit val world: World) extends Carnivore with Female
class MaleCarnivore(override implicit val world: World) extends Carnivore with Male



object Plant {
  val Energy = 100
  val GrowTime = 50 // after how many hours plant grows (increments size)
  val MaxSize = 6  // max size a plant reaches. then expands
}

class Plant(override implicit val world: World) extends BoardElement {

  var energy = Plant.Energy
  var isDead = false
  var age = 0
  var size = 0
  
  val tickHandler = {_: Unit => 
    age += 1
    
    if(age % Plant.GrowTime == 0){
       val oldSize = size
	   size = math.min(Plant.MaxSize, size + 1)
	   if(size != oldSize){
	     if(size == Plant.MaxSize){
	        // germinate: spawn a new plant in proximity to this one
		    world.board.getPosition(this).foreach{ mypos =>
		      world.board.nearestFree(mypos).foreach { target =>
		        world.spawn(new Plant)
		      }
		   }
	     }
	   }
    }
    
  }
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
   tickObservers = tickObservers.filterNot(_ == obs)
  }
  
  def unregisterDayChangedObserver(obs: (Unit => Unit)){
    dailyObservers =  dailyObservers.filterNot(_ == obs)
  }
  
  def unregisterWeekChangedObserver(obs: (Unit => Unit)){
    weeklyObservers =  weeklyObservers.filterNot(_ == obs)
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
  def status = "Status string not implemented yet."
  
  
  def newAnimal(isHerbivore: Boolean, isMale: Boolean): Animal = {
	if(isHerbivore){
	  if(isMale) new MaleHerbivore else new FemaleHerbivore
	}
	else {
      if(isMale) new MaleCarnivore else new FemaleCarnivore
	}
  }
  
  /** returns an animal at random */
  def newAnimal: Animal = newAnimal(randomness.nextBoolean, randomness.nextBoolean)
  
  /** batch spawns n Animals and m Plants */
  def batchSpawn(nAnimals: Int, mPlants: Int) {
    for(_ <- 1 to nAnimals) spawn(newAnimal)
    for(_ <- 1 to mPlants) spawn(new Plant)
  }
  
  /** spawns the given Board element at a free random position in the world */
  def spawn(element: BoardElement) { 
    spawn(element,  board.randomFreePosition(randomness))
  }
  
  /** spawns the given Board element at a given position in the world */
  def spawn(element: BoardElement, pos: Pos) {
    board.add(element, pos)
    
    // register handlers
    element.tickHandlers.foreach(handler => 
      time.registerTickObserver(handler)
    )
    element.dailyHandlers.foreach(handler => 
      time.registerDayChangedObserver(handler)
    )
    
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
    element.tickHandlers.foreach(handler => 
      time.unregisterTickObserver(handler)
    )
    element.dailyHandlers.foreach(handler => 
      time.unregisterDayChangedObserver(handler)
    )    
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
    this spawn newAnimal
  }
}
