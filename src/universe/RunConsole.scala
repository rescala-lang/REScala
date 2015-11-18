package universe

import java.nio.file.{StandardOpenOption, Paths, Files}

import universe.Globals.engine

// test
object RunConsole {
  def main(args: Array[String]): Unit = {
    val nAnimals = 100
    val nPlants = 300

    for (repetition <- 0 to 5; threads <- 1 to 16) {
      println(s"rep: $repetition, threads: $threads")

      Globals.setParallelism(threads)

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
      //    println(world.time)
      //    println(world.status)
      //    println(world.board.dump)
      val duration = (System.nanoTime() - start) / 1000000000.0D
      //    println(s"duration = ${duration}s")
      if (repetition > 0) Files.write(Paths.get(args(0) + s"-threads$threads.txt"), s"$duration\n".getBytes(), StandardOpenOption.APPEND, StandardOpenOption.CREATE)
    }
  }
}
