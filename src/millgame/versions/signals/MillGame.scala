package millgame.versions.signals

import millgame.types._
import millgame.versions.signals._
import react.events._
import react.SignalSynt
import react.Var
import react.Signal
import macro.SignalMacro.{SignalM => Signal}

abstract class Gamestate {
  def getPlayer: Slot
  def text: String
}

case class PlaceStone(val player: Slot) extends Gamestate {
  def getPlayer = player
  def text = player + " to PLACE a stone"
}

case class RemoveStone(val player: Slot) extends Gamestate {
  def getPlayer = player
  def color = player.other
  def text = player + " to REMOVE a " + color + " stone"
}

case class MoveStoneSelect(val player: Slot) extends Gamestate {
  def getPlayer = player
  def text = player + " to MOVE a stone"
}

case class MoveStoneDrop(val player: Slot, index: Int) extends Gamestate {
  def getPlayer = player
  def text = player + " to select destination for stone " + index
}

case class JumpStoneSelect(val player: Slot) extends Gamestate {
  def getPlayer = player
  def text = player + " to JUMP with a stone"
}
case class JumpStoneDrop(val player: Slot, index: Int) extends Gamestate {
  def getPlayer = player
  def text = player + " to select destination for stone " + index
}

case class GameOver(winner: Slot) extends Gamestate {
  def getPlayer = Empty
  def text = "Game over. " + winner + " wins."
}

class MillGame {

  val board = new MillBoard
  
  var state: Gamestate = PlaceStone(White)
  val state2: Var[Gamestate] = Var(PlaceStone(White))
  var remainCount: Map[Slot, Int] = Map(Black -> 4, White -> 4)
  val remainCount2: Var[Map[Slot, Int]] = Var(Map(Black -> 4, White -> 4))

  board.millClosed += { color =>
    state2.setVal(RemoveStone(color))
    // -- changeState(RemoveStone(color))
  }

  val winState: Signal[Option[Slot]] = Signal {
    val remain: Map[Slot, Int] = remainCount2()
    val stones = board.numStones()
    
    val won = for (player: Slot <- List(Black, White))
      yield (player, remain(player) == 0 && stones(player) < 3)
      
    // TODO: Why do we get a type error here?
    //val foo = won.collectFirst { case (winner, true) => winner }
    None
   }
  
  val gameWon: Event[Slot] = winState.changed && (_.isDefined) map {(_: Option[Slot]) match { case Some(winner) => winner}}

  board.numStonesChanged += {
    case (color, n) =>
      if (remainCount(color) == 0 && n < 3) {
        // -- changeState(GameOver(color.other))
        // -- gameWon(color.other)
      }
  }
  
  val remainCountChanged = new ImperativeEvent[Map[Slot, Int]]


  val stateChanged = new ImperativeEvent[Gamestate]
  
  
  private def changeState(to: Gamestate) {
    state = to
    stateChanged(state)
  }

  private def nextState(player: Slot): Gamestate =
    if (remainCount(player) > 0) PlaceStone(player)
    else if (board.numStones.getVal(player) == 3) JumpStoneSelect(player)
    else MoveStoneSelect(player)

  private def decrementCount(player: Slot) {
    remainCount = remainCount.updated(player, remainCount(player) - 1)
    remainCountChanged(remainCount)
  }

  def playerInput(i: Int): Boolean = state match {

    case PlaceStone(player) =>
      if (board.canPlace(i)) {
        changeState(nextState(player.other))
        decrementCount(player)
        board.place(i, player)
        true
      } else false

    case remove @ RemoveStone(player) =>
      if (board(i) == remove.color) {
        board.remove(i)
        changeState(nextState(player.other))
        true
      } else false

    case MoveStoneSelect(player) =>
      if (board(i) == player) {
        changeState(MoveStoneDrop(player, i))
        true
      } else false

    case MoveStoneDrop(player, stone) =>
      if (board.canMove(stone, i)) {
        changeState(nextState(player.other))
        board.move(stone, i)
        true
      } else {
        changeState(MoveStoneSelect(player))
        false
      }

    case JumpStoneSelect(player) =>
      if (board(i) == player) {
        changeState(JumpStoneDrop(player, i))
        true
      } else false

    case JumpStoneDrop(player, stone) =>
      if (board.canJump(stone, i)) {
        changeState(nextState(player.other))
        board.move(stone, i)
        true
      } else {
        changeState(MoveStoneSelect(player))
        false
      }

    case _ => false
  }
}