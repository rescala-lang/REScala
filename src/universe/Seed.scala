package universe


import universe.AEngine.engine

class Seed(implicit world: World) extends BoardElement {

  val growTime = world.time.hour.changed.iterate(Plant.GrowTime)(_ - 1)
  val isDead = growTime map {_ <= 0}
  //#SIG
  override def isAnimal: Boolean = false

  dies += { _ => //#HDL
    world.plan {
      world.board.getPosition(this).foreach { mypos =>
        world.board.nearestFree(mypos).foreach { target =>
          world.spawn(new Plant)
        }
      }
    }
  }
}
