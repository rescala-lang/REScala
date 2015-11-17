package universe

import universe.AEngine.engine

// test
object RunConsole extends App {
  val nAnimals = 10
  val nPlants = 3

  val world = new World

  world batchSpawn(nAnimals, nPlants)

  val start = System.nanoTime()
  while (world.time.week.now < 2) {
    println(world.time)
    println(world.status)
    //println(world.board.dump) // dumping the board is a bottleneck!

    world.tick()
    world.runPlan()
  }
  println(world.board.dump)
  println(f"duration = ${(System.nanoTime() - start) / 1000000000.0D}%2.3fs")
}
