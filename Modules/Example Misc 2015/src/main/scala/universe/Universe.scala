package universe

import java.nio.file.{Files, Paths, StandardOpenOption}

object Universe {

  def main(args: Array[String]): Unit = {
    val nAnimals    = 100
    val nPlants     = 300
    val width       = 100
    val height      = 100
    val repetitions = 5

    val genCsv = !args.headOption.contains("nocsv")

    println(s"profiling ${Globals.engineName}")

    val outfile = s"universe-${Globals.engineName}.csv"

    if (genCsv) Files.write(
      Paths.get(outfile),
      s""""Repetition","Threads","Score","Param: engineName","Benchmark","Param: height","Param: width","Param: animals","Param: plants"${"\n"}""".getBytes(),
      StandardOpenOption.WRITE,
      StandardOpenOption.CREATE
    )

    for (repetition <- 0 to repetitions) {
      println(s"rep: $repetition")

      System.gc()

      val world = new World(width, height)

      world.batchSpawn(nAnimals, nPlants)

      val start = System.nanoTime()
      while (world.time.week.readValueOnce(Globals.engine.scheduler) < 2) {
        world.tick()
        world.runPlan()
      }
      val duration = (System.nanoTime() - start) / 1e9d
      if (!genCsv) println(s"duration: $duration")
      if (repetition > 0 && genCsv) {
        Files.write(
          Paths.get(outfile),
          s"""$repetition,$duration,"${Globals.engineName}","UniverseCaseStudy",$height,$width,$nAnimals,$nPlants${"\n"}""".getBytes(),
          StandardOpenOption.APPEND
        )
      }
    }
  }

}
