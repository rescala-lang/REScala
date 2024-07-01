package dtn

import rdts.base.Uid
import rdts.time.{Dot, Dots}

import scala.util.Random

object DotsCreation {
  val uid_pool: List[Uid] = List("A1", "B2", "C3", "D4", "E5", "F6", "G7", "H8", "I9", "J10").map(Uid.predefined)

  def generate_pseudo_random_dots(): Dots = {
    val rand = Random()

    val current_time: Long = System.currentTimeMillis()
    // 4 years earlier
    val earliest_time: Long = current_time - (1000 * 60 * 60 * 24 * 365 * 4)

    val uids = rand.shuffle(uid_pool).take(8)

    val dots = uids.flatMap { uid =>
      List.fill(21) {
        val time = rand.between(earliest_time, current_time)
        Dot(uid, time)
      }
    }

    Dots.from(dots)

  }
}
