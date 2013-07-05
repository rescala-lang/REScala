package millgame.types

class Slot {
  def other = this match {
    case Black => White
    case White => Black
  }
}

case object Empty extends Slot
case object Black extends Slot
case object White extends Slot