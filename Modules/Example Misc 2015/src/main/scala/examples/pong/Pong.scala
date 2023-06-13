package examples.pong

import java.awt.Point

import examples.Mouse
import rescala.default._

object Pong {
  val Max_X = 800
  val Max_Y = 400
}

object Ball {
  val Size = 20
}

class Pong(val tick: Evt[Unit], val mouse: Mouse) {

  val LeftRacketPos  = 30
  val RightRacketPos = 770

  val initPosition = new Point(400, 200)
  val speed        = new Point(10, 8)

  val x: Signal[Int] = tick.fold(initPosition.x) { (pos, _) => pos + speedX.now }
  val y: Signal[Int] = tick.fold(initPosition.y) { (pos, _) => pos + speedY.now }

  val mouseY = Signal { mouse.position.value.getY().toInt }

  val leftRacket  = new Racket(LeftRacketPos, mouseY)
  val rightRacket = new Racket(RightRacketPos, y)

  val rackets         = Signal { List(leftRacket, rightRacket) }
  val areas           = Signal.dynamic { rackets.value.map(_.area.value) }
  val ballInRacket    = Signal { areas.value.exists(_.contains(x.value, y.value)) }
  val collisionRacket = ballInRacket.changed.filter(_ == true)

  val leftWall  = x.changed && (x => x < 0)
  val rightWall = x.changed && (x => x + Ball.Size > Pong.Max_X)

  val xBounce = leftWall || rightWall || collisionRacket
  val yBounce = y.changed && (y => y < 0 || y + Ball.Size > Pong.Max_Y)

  val speedX = xBounce.toggle(Var(speed.x), Var(-speed.x))
  val speedY = yBounce.toggle(Var(speed.y), Var(-speed.y))

  val pointsPlayer   = rightWall.count()
  val pointsComputer = leftWall.count()

  val score = Signal { pointsPlayer.value.toString + " : " + pointsComputer.value }
}
