package universe

import rescala.Signals

import AEngine.engine
import AEngine.engine._
import rescala.turns.Ticket

object Animal {
  val StartEnergy = 200
  // radius that animals can see the world around them
  val ViewRadius = 9
  // energy required to move
  val MoveCost = 1
  // energy required to procreate
  val ProcreateCost = 10
  // maximum age in days when an animal dies, regardless of energy
  val MaxAge = 25
  // energy rate gained when eating plants
  val PlantEatRate = 3
  // minimum energy required for male animals to seek a mate
  val ProcreateThreshold = 60
  // minimum age in days for animals to be fertile
  val FertileAge = 1
  // time in hours for female sheep to be pregnant
  val PregnancyTime = 30
  // minimum energy for carnivores to attack
  val AttackThreshold = 100
  // energy stolen when carnivores attack
  val AttackAmount = 50
  // minimum energy for carnivores to start sleeping
  val SleepThreshold = 30
  // energy gained while sleeping
  val SleepRate = 2
}


abstract class Animal(implicit world: World) extends BoardElement {


  override def isAnimal: Boolean = true

  /** An animal is in a state */
  sealed trait AnimalState
  case object Idling extends AnimalState
  case class Eating(plant: Plant) extends AnimalState
  case class Attacking(other: Animal) extends AnimalState
  case class Moving(dir: Pos) extends AnimalState
  case class Procreating(female: Animal) extends AnimalState
  case object FallPrey extends AnimalState
  case object Sleeping extends AnimalState

  val state: Var[AnimalState] = Var(Idling) //#VAR

  // partial function for collecting food, dependant on state of the object
  val findFood: Signal[PartialFunction[BoardElement, BoardElement]] // Abstract (//#SIG)

  // function for creating a state upon reaching target
  def reachedState(target: BoardElement): AnimalState


  def savage() = state.set(FallPrey)

  protected def nextAction(pos: Pos): AnimalState = {
    val neighbors = world.board.neighbors(pos)
    val food = neighbors.collectFirst(findFood.now)
    val nextAction: AnimalState = food match {
      case Some(target) => reachedState(target) // I'm near food, eat it!
      case None => // I have to look for food nearby
        world.board.nearby(pos, Animal.ViewRadius).collectFirst(findFood.now) match {
          case Some(target) =>
            val destination = world.board.getPosition(target)
            if (destination.isDefined)
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


  val age: Signal[Int] = world.time.day.changed.iterate(1)(_ + 1) //#SIG //#IF //#IF

  val isAdult = age.map(_ > Animal.FertileAge)
  val isFertile = isAdult
  val isEating = state map {
    case Eating(_) => true
    case _ => false
  }

  val energyDrain = Signals.lift(age, state, world.board.animalsAlive) { (a, s, alive) =>
    (alive / (world.board.width + world.board.height)) +
      (a / 2) +
      (s match {
        case Moving(_) => Animal.MoveCost
        case Procreating(_) => Animal.ProcreateCost
        case FallPrey => Animal.AttackAmount
        case _ => 0
      })
  }

  val energyGain =
    state map {
      case Eating(_) => Animal.PlantEatRate
      case Sleeping => Animal.SleepRate
      case Attacking(prey) => Animal.AttackAmount
      case _ => 0
    }

  // we do not have a built in method for this kind of “fold some snapshot” but its not that hard to write one
  val energy: Signal[Int] =
    implicitly[Ticket[Spores]].apply(Signals.Impl.makeStatic[Int, Spores](Set(world.time.tick, energyDrain, energyGain), Animal.StartEnergy) {
      (turn, current) => world.time.tick.pulse(turn).fold(current, _ => current + energyGain.get(turn) - energyDrain.get(turn))
    })

  override val isDead = Signals.lift(age, energy) { (a, e) => a > Animal.MaxAge || e < 0 }

  /** imperative 'AI' function */
  override def doStep(pos: Pos): Unit = {
    state.now match {
      case Moving(dir) => world.board.moveIfPossible(pos, dir)
      case Eating(plant) => plant.takeEnergy(energyGain.now)
      case Attacking(prey) => prey.savage()
      case Procreating(female: Female) => female.procreate(this)
      case _ =>
    }
    state.set(nextAction(pos))
  }
}
