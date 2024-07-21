package universe

import reactives.default.*
import universe.Animal.*

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

  /** An animal is in a state */
  sealed trait AnimalState
  case object Idling                     extends AnimalState
  case class Eating(plant: Plant)        extends AnimalState
  case class Attacking(other: Animal)    extends AnimalState
  case class Moving(dir: Pos)            extends AnimalState
  case class Procreating(female: Animal) extends AnimalState
  case object FallPrey                   extends AnimalState
  case object Sleeping                   extends AnimalState
}

abstract class Animal(implicit _world: World) extends BoardElement {

  final override def isAnimal: Boolean = true

  final val step: Evt[(Pos, Boolean)] = Evt[(Pos, Boolean)]()

  private val statePos: Signal[(AnimalState, Pos)] = step.fold((Idling: AnimalState, Pos(0, 0))) { (p1, p2) =>
    (p1, p2) match {
      case ((oldState, oldPos), (newPos, prey)) =>
        if prey then (FallPrey, oldPos)
        else (nextAction(newPos), newPos)
    }
  }

  private val state: Signal[AnimalState] = statePos.map(_._1)

  statePos.observe {
    case (cstate, pos) =>
      world.plan {
        cstate match {
          case Moving(dir)                 => world.board.moveIfPossible(pos, dir)
          case Eating(plant)               => plant.takeEnergy(energyGain.readValueOnce)
          case Attacking(prey)             => prey.savage()
          case Procreating(female: Female) => female.procreate(this)
          case _                           =>
        }
      }
  }

  /** Some imperative code that is called each tick */
  final override def doStep(pos: Pos): Unit = step.fire((pos, false))

  private def savage() = step.fire((Pos(0, 0), true))

  // partial function for collecting food, dependant on state of the object
  val findFood: Signal[PartialFunction[BoardElement, BoardElement]] // Abstract (//#SIG)

  // function for creating a state upon reaching target
  def reachedState(target: BoardElement): AnimalState

  protected def nextAction(pos: Pos): AnimalState = {
    val neighbors = world.board.neighbors(pos)
    val food      = neighbors.collectFirst(findFood.readValueOnce)
    val nextAction: AnimalState = food match {
      case Some(target) => reachedState(target) // I'm near food, eat it!
      case None => // I have to look for food nearby
        world.board.nearby(pos, Animal.ViewRadius).collectFirst(findFood.readValueOnce) match {
          case Some(target) =>
            val destination = world.board.getPosition(target)
            if destination.isDefined then
              Moving(pos.directionTo(destination.get))
            else
              randomMove
          case None => randomMove
        }
    }
    nextAction
  }

  private def randomMove: AnimalState = {
    val randx = 1 - world.randomness.nextInt(3)
    val randy = 1 - world.randomness.nextInt(3)
    Moving(Pos(randx, randy))
  }

  final val age: Signal[Int] = world.time.day.changed.count() // #SIG //#IF //#IF

  final val isAdult: Signal[Boolean] = age.map(_ > Animal.FertileAge)

  val isFertile: Signal[Boolean] = isAdult

  private val energyDrain: Signal[Int] =
    Signal {
      (world.board.animalsAlive.value / (world.board.width + world.board.height)) +
      (age.value / 2) +
      (state.value match {
        case Moving(_)      => Animal.MoveCost
        case Procreating(_) => Animal.ProcreateCost
        case FallPrey       => Animal.AttackAmount
        case _              => 0
      })
    }

  private val energyGain: Signal[Int] =
    state map {
      case Eating(_)       => Animal.PlantEatRate
      case Sleeping        => Animal.SleepRate
      case Attacking(prey) => Animal.AttackAmount
      case _               => 0
    }

  // we do not have a built in method for this kind of “fold some snapshot” but its not that hard to write one
  final protected val energy: Signal[Int] =
    Event { world.time.tick.value.map(_ => energyGain.value - energyDrain.value) }.fold(Animal.StartEnergy)(
      (current, change) =>
        current + change
    )

  final override val isDead = Signal.lift(age, energy) { (a, e) => a > Animal.MaxAge || e < 0 }

}
