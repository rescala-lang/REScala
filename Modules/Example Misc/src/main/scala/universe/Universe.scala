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

    println(s"profiling ${reactives.SelectedScheduler.candidate.scheduler.schedulerName}")

    val outfile = s"universe-${reactives.SelectedScheduler.candidate.scheduler.schedulerName}.csv"

    if genCsv then
      Files.write(
        Paths.get(outfile),
        s""""Repetition","Threads","Score","Param: engineName","Benchmark","Param: height","Param: width","Param: animals","Param: plants"${"\n"}""".getBytes(),
        StandardOpenOption.WRITE,
        StandardOpenOption.CREATE
      )
      ()

    for repetition <- 0 to repetitions do {
      println(s"rep: $repetition")

      System.gc()

      val world = new World(width, height)

      world.batchSpawn(nAnimals, nPlants)

      val start = System.nanoTime()
      while world.time.week.readValueOnce < 2 do {
        world.tick()
        world.runPlan()
      }
      val duration = (System.nanoTime() - start) / 1e9d
      if !genCsv then println(s"duration: $duration")
      if repetition > 0 && genCsv then {
        Files.write(
          Paths.get(outfile),
          s"""$repetition,$duration,"${reactives.SelectedScheduler.candidate.scheduler.schedulerName}","UniverseCaseStudy",$height,$width,$nAnimals,$nPlants${"\n"}""".getBytes(),
          StandardOpenOption.APPEND
        )
        ()
      }
    }
  }

}
