package millgame.versions.signals

import millgame.types._
import millgame.versions.signals._
import react.events._
import react.SignalSynt
import react.Var
import react.Signal
import macro.SignalMacro.{SignalM => Signal}
import millgame.types.Empty

abstract class Gamestate {
  def getPlayer: Slot
  def text: String
}

case class PlaceStone(val player: Slot) extends Gamestate {
  def getPlayer = player
  def text = player + " to PLACE a stone"
}

case class RemoveStone(val player: Slot) extends Gamestate {
  assert(player != Empty)
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

  val state: Var[Gamestate] = Var(PlaceStone(White))  //#VAR
  val remainCount: Var[Map[Slot, Int]] = Var(Map(Black -> 9, White -> 9)) //#VAR
  
  val remainCountChanged = Signal{remainCount()}.changed //#EVT //#IF
  val stateChanged = Signal{ state()}.changed //#EVT //#IF
  def stateText = state().text
  
  
  /* Event based game logic: */
  board.millClosed += { color => //#HDL
    state() = RemoveStone(color)
  }

  board.numStonesChanged += { //#HDL
    case (color, n) =>
      if (remainCount.getValue(color) == 0 && n < 3) {
        state() = GameOver(color.other)  
      }
  }
  
  val gameEnd = stateChanged && ((_: Gamestate) match {case GameOver(_) => true; case _ => false}) //#EVT
  val gameWon: Event[Slot] = gameEnd.map {(_: Gamestate) match {case GameOver(w) => w; case _ => null}} //#EVT

  def possibleMoves = board.possibleMoves.getVal
  
  private def nextState(player: Slot): Gamestate =
    if (remainCount()(player) > 0) PlaceStone(player)
    else if (board.numStones.getVal(player) == 3) JumpStoneSelect(player)
    else MoveStoneSelect(player)

  private def decrementCount(player: Slot) {
    val currentCount = remainCount.getValue
    remainCount() = currentCount.updated(player, currentCount(player) - 1)
  }

  def playerInput(i: Int): Boolean = state.getValue match {

    case PlaceStone(player) =>
      if (board.canPlace(i)) {
        state() = (nextState(player.other))
        decrementCount(player)
        board.place(i, player)
        true
      } else false

    case remove @ RemoveStone(player) =>
      if (board(i) == remove.color) {
        state() = (nextState(player.other))
        /// NOTE: Removing the stone can trigger events which change the state
        /// therefore, remove has to be called after the change state
        board.remove(i)
        true
      } else false

    case MoveStoneSelect(player) =>
      if (board(i) == player) {
        state() = (MoveStoneDrop(player, i))
        true
      } else false

    case MoveStoneDrop(player, stone) =>
      if (board.canMove(stone, i)) {
        state() = (nextState(player.other))
        board.move(stone, i)
        true
      } else {
        state() = (MoveStoneSelect(player))
        false
      }

    case JumpStoneSelect(player) =>
      if (board(i) == player) {
        state() = (JumpStoneDrop(player, i))
        true
      } else false

    case JumpStoneDrop(player, stone) =>
      if (board.canJump(stone, i)) {
        state() = (nextState(player.other))
        board.move(stone, i)
        true
      } else {
        state() = (MoveStoneSelect(player))
        false
      }

    case _ => false
  }
}