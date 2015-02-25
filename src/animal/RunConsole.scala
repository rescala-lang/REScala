package animal

// test
object RunConsole extends App {
  val nAnimals = 10
  val nPlants = 3

  val world = new World

  world batchSpawn(nAnimals, nPlants)

  while (true) {
    println("\n\n\n")
    println(world.time)
    println(world.status)
    println(world.board.dump) // dumping the board is a bottleneck!
    
    world.tick()
    world.runPlan()
  }
}
