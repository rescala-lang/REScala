package universe

import universe.Globals.engine._

class Seed(implicit world: World) extends BoardElement {

  val growTime: Signal[Int]   = world.time.hour.changed.iterate(Plant.GrowTime)(_ - 1)
  val isDead: Signal[Boolean] = growTime map { _ <= 0 }
  // #SIG
  override def isAnimal: Boolean = false

  dies += { _ => // #HDL
    world.plan {
      world.board.getPosition(this).foreach { mypos =>
        world.board.nearestFree(mypos).foreach { _ =>
          world.spawn(new Plant)
        }
      }
    }
  }
}
