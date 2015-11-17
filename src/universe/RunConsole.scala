package universe

import universe.AEngine.engine

// test
object RunConsole extends App {
  val nAnimals = 100
  val nPlants = 300

  val world = new World

  world batchSpawn(nAnimals, nPlants)

  val start = System.nanoTime()
  while (world.time.week.now < 2) {
//    println(world.time)
//    println(world.status)
//    println(world.board.dump) // dumping the board is a bottleneck!

    world.tick()
    world.runPlan()
  }
  println(world.time)
  println(world.status)
  println(world.board.dump)
  println(s"duration = ${(System.nanoTime() - start) / 1000000000.0D}s")
}
