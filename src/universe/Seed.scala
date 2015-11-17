package universe


import rescala.Signals
import AEngine.engine
import AEngine.engine._

class Seed(implicit world: World) extends BoardElement {

  override def isAnimal: Boolean = false

  val growTime = world.time.hour.changed.iterate(Plant.GrowTime)(_ - 1)
  val isDead = growTime map { _ <= 0 } //#SIG

  dies += { _ => //#HDL
    world.board.getPosition(this).foreach { mypos =>
      world.board.nearestFree(mypos).foreach { target =>
        world.spawn(new Plant)
      }
    }
  }
}
