package millgame.versions.events

import millgame.types._
import millgame.versions.events._
import react.events.ImperativeEvent

abstract class Gamestate {
  def getPlayer: Slot
  def text: String
}

case class PlaceStone(player: Slot) extends Gamestate {
  def getPlayer = player
  def text = player + " to PLACE a stone"
}

case class RemoveStone(player: Slot) extends Gamestate {
  def getPlayer = player
  def color = player.other
  def text = player + " to REMOVE a " + color + " stone"
}

case class MoveStoneSelect(player: Slot) extends Gamestate {
  def getPlayer = player
  def text = player + " to MOVE a stone"
}

case class MoveStoneDrop(player: Slot, index: SlotIndex) extends Gamestate {
  def getPlayer = player
  def text = player + " to select destination for stone at position " + index
}

case class JumpStoneSelect(player: Slot) extends Gamestate {
  def getPlayer = player
  def text = player + " to JUMP with a stone"
}
case class JumpStoneDrop(player: Slot, index: SlotIndex) extends Gamestate {
  def getPlayer = player
  def text = player + " to select destination for stone at position " + index
}

case class GameOver(winner: Slot) extends Gamestate {
  def getPlayer = Empty
  def text = "Game over. " + winner + " wins."
}

class MillGame {

  val board = new MillBoard
  var state: Gamestate = PlaceStone(White)
  var remainCount: Map[Slot, Int] = Map(Black -> 9, White -> 9)
  
  def possibleMoves: Seq[(SlotIndex, SlotIndex)] = {
    state match {
      case PlaceStone(_) | RemoveStone(_) | GameOver(_) => Seq.empty
      case state @ (MoveStoneSelect(_) |MoveStoneDrop(_, _)) =>
        board.possibleMoves filter { case (from, to) => board(from) == state.getPlayer }
      case state @ (JumpStoneSelect(_) | JumpStoneDrop(_, _)) =>
        board.possibleJumps filter { case (from, to) => board(from) == state.getPlayer }
    }
  }

  val remainCountChanged = new ImperativeEvent[Map[Slot, Int]] //#EVT
  val gameWon = new ImperativeEvent[Slot] //#EVT
  val stateChanged = new ImperativeEvent[Gamestate] //#EVT
  
  private def changeState(to: Gamestate) {
    state = to
    stateChanged(state)
  }
   
  /* Event based game logic: */
   board.millClosed += { color => //#HDL
    changeState(RemoveStone(color))
  }

  board.numStonesChanged += { //#HDL
    case (color, n) =>
      if (remainCount(color) == 0 && n < 3) {
        gameWon(color.other)
        changeState(GameOver(color.other))
      }
  }
  

  private def nextState(player: Slot): Gamestate =
    if (remainCount(player) > 0) PlaceStone(player)
    else if (board.numStones(player) == 3) JumpStoneSelect(player)
    else MoveStoneSelect(player)

  private def decrementCount(player: Slot) {
    remainCount = remainCount.updated(player, remainCount(player) - 1)
    remainCountChanged(remainCount)
  }

  def playerInput(i: SlotIndex): Boolean = state match {

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
        changeState(JumpStoneSelect(player))
        false
      }

    case _ => false
  }
}