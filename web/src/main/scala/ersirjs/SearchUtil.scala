package ersirjs


import scala.annotation.tailrec


object SearchUtil {
  def search[T](query: String, items: List[(String, T)]): List[T] = {
    val lcql = query.toLowerCase.replaceAll("""\s+""", "").toList
    if (lcql.isEmpty) items.sortBy(_._1).map(_._2)
    else items
      .map { item => item -> fuzzyMatch(lcql, item._1.toLowerCase.toList) }
      .filter {_._2 > 0}
      .sortBy {-_._2}
      .map {_._1._2}
  }

  @tailrec
  def fuzzyMatch(query: List[Char], text: List[Char], score: Long = 0, bestScore: Long = 0): Long = query match {
    case Nil => bestScore + score * score
    case q :: qs => text match {
      case Nil => 0
      case t :: ts =>
        if (t == q) fuzzyMatch(qs, ts, score + 1, bestScore)
        else fuzzyMatch(query, ts, 0, score * score + bestScore)
    }
  }
}
