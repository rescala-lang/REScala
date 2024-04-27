package reswing.millgame.versions.signals

import reactives.default.*
import reswing.millgame.types.*

sealed abstract class Gamestate {
  def getPlayer: Slot
  def text: String
}

case class PlaceStone(player: Slot) extends Gamestate {
  def getPlayer = player
  def text      = player.toString + " to PLACE a stone"
}

case class RemoveStone(player: Slot) extends Gamestate {
  def getPlayer = player
  def color     = player.other
  def text      = player.toString + " to REMOVE a " + color + " stone"
}

case class MoveStoneSelect(player: Slot) extends Gamestate {
  def getPlayer = player
  def text      = player.toString + " to MOVE a stone"
}

case class MoveStoneDrop(player: Slot, index: SlotIndex) extends Gamestate {
  def getPlayer = player
  def text      = player.toString + " to select destination for stone at position " + index
}

case class JumpStoneSelect(player: Slot) extends Gamestate {
  def getPlayer = player
  def text      = player.toString + " to JUMP with a stone"
}
case class JumpStoneDrop(player: Slot, index: SlotIndex) extends Gamestate {
  def getPlayer = player
  def text      = player.toString + " to select destination for stone at position " + index
}

case class GameOver(winner: Slot) extends Gamestate {
  def getPlayer = Empty
  def text      = "Game over. " + winner + " wins."
}

class MillGame {

  val board = new MillBoard

  val stateVar: Var[Gamestate]         = Var(PlaceStone(White))           // #VAR
  val remainCount: Var[Map[Slot, Int]] = Var(Map(Black -> 9, White -> 9)) // #VAR

  def state = stateVar.now

  val remainCountChanged = remainCount.changed // #EVT //#IF
  val stateChanged       = stateVar.changed    // #EVT //#IF

  val possibleNextMoves: Signal[Seq[(SlotIndex, SlotIndex)]] = Signal { // #SIG
    stateVar.value match {
      case PlaceStone(_) | RemoveStone(_) | GameOver(_) => Seq.empty
      case state @ (MoveStoneSelect(_) | MoveStoneDrop(_, _)) =>
        board.possibleMoves.value filter { case (from, to) => board(from) == state.getPlayer }
      case state @ (JumpStoneSelect(_) | JumpStoneDrop(_, _)) =>
        board.possibleJumps.value filter { case (from, to) => board(from) == state.getPlayer }
    }
  }

  def possibleMoves = possibleNextMoves.now

  /* Event based game logic: */
  board.millClosed observe { color => // #HDL
    stateVar set RemoveStone(color)
  }

  board.numStonesChanged observe { // #HDL
    case (color, n) =>
      if (remainCount.now.apply(color) == 0 && n < 3) {
        stateVar set GameOver(color.other)
      }
  }

  val gameEnd: Event[Gamestate] =
    stateChanged && ((_: Gamestate) match { case GameOver(_) => true; case _ => false }) // #EVT //#EF
  val gameWon: Event[Slot] = gameEnd map {
    (_: Gamestate) match { case GameOver(w) => w; case _ => null }
  } // #EVT //#EF

  private def nextState(player: Slot): Gamestate =
    if (remainCount.now.apply(player) > 0) PlaceStone(player)
    else if (board.numStones.now.apply(player) == 3) JumpStoneSelect(player)
    else MoveStoneSelect(player)

  private def decrementCount(player: Slot): Unit = {
    remainCount.transform(currentCount => currentCount.updated(player, currentCount(player) - 1))
  }

  def playerInput(i: SlotIndex): Boolean =
    state match {

      case PlaceStone(player) =>
        if (board.canPlace.now.apply(i)) {
          stateVar set nextState(player.other)
          decrementCount(player)
          board.place(i, player)
          true
        } else false

      case remove @ RemoveStone(player) =>
        if (board(i) == remove.color) {
          board.remove(i)
          stateVar set nextState(player.other)
          true
        } else false

      case MoveStoneSelect(player) =>
        if (board(i) == player) {
          stateVar set MoveStoneDrop(player, i)
          true
        } else false

      case MoveStoneDrop(player, stone) =>
        if (board.canMove.now.apply(stone, i)) {
          stateVar set nextState(player.other)
          board.move(stone, i)
          true
        } else {
          stateVar set MoveStoneSelect(player)
          false
        }

      case JumpStoneSelect(player) =>
        if (board(i) == player) {
          stateVar set JumpStoneDrop(player, i)
          true
        } else false

      case JumpStoneDrop(player, stone) =>
        if (board.canJump.now.apply(stone, i)) {
          stateVar set nextState(player.other)
          board.move(stone, i)
          true
        } else {
          stateVar set JumpStoneSelect(player)
          false
        }

      case _ => false
    }
}
