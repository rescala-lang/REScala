package animal.versions.signal

import animal.types.Pos
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import react.events.ImperativeEvent
import react.SignalSynt
import react.Var
import react.Signal
import scala.util.Random
import animal.types.Pos.fromTuple
import scala.Option.option2Iterable
import macro.SignalMacro.{SignalM => Signal}


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
      case Some(m: Male) if m.isAdult.getVal => 'm'
      case Some(f: Female) if f.isAdult.getVal => if (f.isPregnant.getVal) 'F' else 'f'
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
  lazy val dies = isDead changedTo true
  
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
 
	val state: Var[AnimalState] = Var(Idling)
	
	// partial function for collecting food, dependant on state of the object
	val findFood: Signal[PartialFunction[BoardElement, BoardElement]]
	
	// function for creating a state upon reaching target
	def reachedState(target: BoardElement): AnimalState
	
	
	def savage = state.setVal(FallPrey)
	
	protected def nextAction(pos: Pos): AnimalState =  {
		val neighbors = world.board.neighbors(pos)
		val food = neighbors.collectFirst(findFood.getVal)
		val nextAction: AnimalState = food match {
		  case Some(target) => reachedState(target) // I'm near food, eat it!
		  case None => // I have to look for food nearby
		    world.board.nearby(pos, Animal.ViewRadius).collectFirst(findFood.getVal) match {
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

	
	val age = world.time.day.changed.iterate(1)(_ + 1)
	
	val isAdult =  Signal { age() > Animal.FertileAge }
	val isFertile = Signal { isAdult() }
	val isEating = Signal { state() match { 
	  case Eating(_) => true
	  case _ => false
	}}
	
	val energyDrain = Signal { 
	  1 + age() / 2 + (state() match {
	    case Moving(_) => Animal.MoveCost
	    case Procreating(_) => Animal.ProcreateCost
	    case FallPrey => Animal.AttackAmount
	    case _ => 0
	  })
	}
	
	val energyGain = Signal{
	  state() match {
	    case Eating(_) => Animal.PlantEatRate
	    case Sleeping => Animal.SleepRate
	    case Attacking(prey) => Animal.AttackAmount
	    case _ => 0
	  }
	}
	val energy: Signal[Int] = 
	  world.time.tick.iterate(Animal.StartEnergy)(_ + energyGain.getVal - energyDrain.getVal)
	
	override val isDead = Signal { age() > Animal.MaxAge || energy() < 0}

	/** imperative 'AI' function */
	override def doStep(pos: Pos) {
	    state.getVal match {
	      case Moving(dir) => world.board.moveIfPossible(pos, dir)
	      case Eating(plant) => plant.takeEnergy(energyGain.getVal)
	      case Attacking(prey) => prey.savage
	      case Procreating(female: Female) => female.procreate(this)
	      case _ =>
	    }
	    state.setVal(nextAction(pos))
	}
}

class Carnivore(override implicit val world: World) extends Animal {
  
  val sleepy = Signal { energy() < Animal.SleepThreshold }
  val canHunt = Signal { energy() > Animal.AttackThreshold }
	
  // only adult carnivores with min energy can hunt, others eat plants
  val findFood: Signal[PartialFunction[BoardElement, BoardElement]] = SignalSynt { (s: SignalSynt[PartialFunction[BoardElement, BoardElement]]) =>  
     if(isAdult(s) && canHunt(s)) { case p: Herbivore => p} : PartialFunction[BoardElement, BoardElement]
     else { case p: Plant => p }                            : PartialFunction[BoardElement, BoardElement] 
   }
    

  override def reachedState(prey: BoardElement): AnimalState = prey match {
    case p: Herbivore => Attacking(p)
    case _ => Idling
  }
  
  
  override protected def nextAction(pos: Pos): AnimalState =  {
	  if(sleepy.getVal) Sleeping
	  else super.nextAction(pos)
  }
}

class Herbivore(override implicit val world: World) extends Animal {
  
  val findFood: Signal[PartialFunction[BoardElement, BoardElement]] =
    SignalSynt { (s: SignalSynt[PartialFunction[BoardElement, BoardElement]]) =>  { case p: Plant => p } :PartialFunction[BoardElement, BoardElement] }
  
  override def reachedState(plant: BoardElement): AnimalState = plant match {
    case p: Plant => Eating(p)
    case _ => Idling
  }
}

trait Female extends Animal {
  
  val mate: Var[Option[Animal]] = Var(None)
  
  val isPregnant = Signal { mate().isDefined }
  
  val becomePregnant = isPregnant.changedTo(true)  
  
  // counts down to 0
  lazy val pregnancyTime: Signal[Int] = Signal { 10 }
  /*
  becomePregnant.reset(()){ _ =>
    world.time.hour.changed.iterate(Animal.PregnancyTime)(_ - (if(isPregnant.getValue) 1 else 0))
  }
   */
  
  
  lazy val giveBirth = pregnancyTime.changedTo(0)
  
  override val isFertile = Signal { isAdult() && !isPregnant()}
  
  // override val energyDrain = Signal { super.energyDrain() * 2 }
  // not possible
  
  giveBirth += {_ =>
    val father = mate.getVal.get
    val child = createOffspring(father)
    world.board.getPosition(this).foreach{ mypos =>
      world.board.nearestFree(mypos).foreach { target =>
        world.spawn(child, target)
      }
    }
    mate.setVal(None)
  }
  
  def procreate(father: Animal) {
    if(isPregnant.getVal) return;
    mate.setVal(Some(father))
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
  val seeksMate = Signal { isFertile() && energy() > Animal.ProcreateThreshold }
  
  override def nextAction(pos: Pos): AnimalState = {
    if(seeksMate.getVal) {
        val findFemale: PartialFunction[BoardElement, Female] =  { 
          case f: Female if f.isFertile.getVal => f 
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
  
  val energy = Var(Plant.Energy)
  
  val isDead = Signal{ energy() <= 0}  
  
  val age = world.time.hour.changed.iterate(0)(_ + 1)
  val grows = age.changed && { _ % Plant.GrowTime == 0}
  val size = grows.iterate(0)(acc => math.min(Plant.MaxSize, acc + 1))
  val expands = size.changedTo(Plant.MaxSize)
  
  
  expands += {_ => 
    // germinate: spawn a new plant in proximity to this one
    world.board.getPosition(this).foreach{ mypos =>
      world.board.nearestFree(mypos).foreach { target =>
        world.spawn(new Plant)
      }
    }
  }

  
  /** takes amount away from the energy of this plant */
  def takeEnergy(amount: Int) = energy.setVal(energy.getVal - amount)
}

class Seed(override implicit val world: World) extends BoardElement {
  
  val growTime = world.time.hour.changed.iterate(Plant.GrowTime)(_ - 1)
  val isDead = Signal{ growTime() <= 0 }
  
  dies += {_ => 
  	world.board.getPosition(this).foreach{ mypos =>
      world.board.nearestFree(mypos).foreach { target =>
        world.spawn(new Plant)
      }
    }
  }
}

class Time {
  val tick = new ImperativeEvent[Unit]

  val hours = tick.iterate(0)(_ + 1)
  val day = Signal { hours() / 24 }
  val hour = Signal { hours() % 24}
  val week = Signal { day() / 7}
  val timestring = Signal { "Week " + week() + ", Day " + day() + " hour:" + hour() }
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
  val randomness = new Random(1)
  
  def tick = time.tick()
    
  def dump = board.dump
  def timestring = time.timestring.getVal
   
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
  
  /** spawns the given board element at the given position */
  def spawn(element: BoardElement, pos: Pos) = board.add(element, pos)
  
  /** spawns the given Board element at a free random position in the world */
  def spawn(element: BoardElement){ 
    spawn(element,  board.randomFreePosition(randomness))
  }
  
  // tick / clear board elements
  time.hour.changed += {x  =>    
    board.elements.foreach { _ match {
      	case (pos, be) =>
      	  if(be.isDead.getVal)
      	    board.clear(pos)
      	  else be.doStep(pos)
      }
    }
  }
  
  
  // each day, spawn a new plant
  time.day.changed += {_ =>
    this spawn new Plant
  }
  
  //each week, spawn a new animal
  time.week.changed += {_ =>
    this spawn newAnimal
  }
}

