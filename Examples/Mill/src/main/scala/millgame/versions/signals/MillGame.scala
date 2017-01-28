package millgame.versions.signals

import millgame.types._

import rescala._

import rescala._
import rescala._
import rescala._

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

  val stateVar: Var[Gamestate] = Var(PlaceStone(White))  //#VAR
  val remainCount: Var[Map[Slot, Int]] = Var(Map(Black -> 9, White -> 9)) //#VAR

  def state = stateVar.get

  val remainCountChanged = remainCount.changed //#EVT //#IF
  val stateChanged = stateVar.changed //#EVT //#IF

  val possibleNextMoves: Signal[Seq[(SlotIndex, SlotIndex)]] = Signal { //#SIG
    stateVar() match {
      case PlaceStone(_) | RemoveStone(_) | GameOver(_) => Seq.empty
      case state @ (MoveStoneSelect(_) |MoveStoneDrop(_, _)) =>
        board.possibleMoves() filter { case (from, to) => board(from) == state.getPlayer }
      case state @ (JumpStoneSelect(_) | JumpStoneDrop(_, _)) =>
        board.possibleJumps() filter { case (from, to) => board(from) == state.getPlayer }
    }
  }

  def possibleMoves = possibleNextMoves.get

  /* Event based game logic: */
  board.millClosed += { color => //#HDL
    stateVar() = RemoveStone(color)
  }

  board.numStonesChanged += { //#HDL
    case (color, n) =>
      if (remainCount.get(color) == 0 && n < 3) {
        stateVar() = GameOver(color.other)
      }
  }

  val gameEnd = stateChanged && ((_: Gamestate) match {case GameOver(_) => true; case _ => false}) //#EVT //#EF
  val gameWon: Event[Slot] = gameEnd map {(_: Gamestate) match {case GameOver(w) => w; case _ => null}} //#EVT //#EF

  private def nextState(player: Slot): Gamestate =
    if (remainCount()(player) > 0) PlaceStone(player)
    else if (board.numStones.get(player) == 3) JumpStoneSelect(player)
    else MoveStoneSelect(player)

  private def decrementCount(player: Slot): Unit = {
    val currentCount = remainCount.get
    remainCount() = currentCount.updated(player, currentCount(player) - 1)
  }

  def playerInput(i: SlotIndex): Boolean = state match {

    case PlaceStone(player) =>
      if (board.canPlace.get(i)) {
        stateVar() = (nextState(player.other))
        decrementCount(player)
        board.place(i, player)
        true
      } else false

    case remove @ RemoveStone(player) =>
      if (board(i) == remove.color) {
        board.remove(i)
        stateVar() = (nextState(player.other))
        true
      } else false

    case MoveStoneSelect(player) =>
      if (board(i) == player) {
        stateVar() = (MoveStoneDrop(player, i))
        true
      } else false

    case MoveStoneDrop(player, stone) =>
      if (board.canMove.get(stone, i)) {
        stateVar() = (nextState(player.other))
        board.move(stone, i)
        true
      } else {
        stateVar() = (MoveStoneSelect(player))
        false
      }

    case JumpStoneSelect(player) =>
      if (board(i) == player) {
        stateVar() = (JumpStoneDrop(player, i))
        true
      } else false

    case JumpStoneDrop(player, stone) =>
      if (board.canJump.get(stone, i)) {
        stateVar() = (nextState(player.other))
        board.move(stone, i)
        true
      } else {
        stateVar() = (JumpStoneSelect(player))
        false
      }

    case _ => false
  }
}
