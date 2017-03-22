package examples.demo.complete

import java.awt.{BorderLayout, Color, Dimension}

import examples.demo.ui._
import rescala._

import scala.swing.{MainFrame, SimpleSwingApplication, Swing, UIElement}

class BouncingCircle(val diameter: Signal[Int], val reset: Event[Point]) {
  val physicsTicks = Clock.time.change.map{ diff => diff.to.get - diff.from.get }

  val angle = Signal{ Clock.time().toDouble / Clock.NanoSecond }

  val horizontalBounceSources: Var[List[Event[Any]]] = Var(List())
  val verticalBounceSources: Var[List[Event[Any]]] = Var(List())

  val velocityX = horizontalBounceSources.flatten.fold(150d / Clock.NanoSecond) { (old, _) => -old }
  val velocityY = verticalBounceSources.flatten.fold(100d / Clock.NanoSecond) { (old, _ ) => -old }

  val posX = reset.zipOuter(physicsTicks).fold(0d){
    case (_, (Some(Point(x, _)), _)) => x.toDouble
    case (pX, (None, Some(tick))) => pX + tick * velocityX.before
  }
  val posY = reset.zipOuter(physicsTicks).fold(0d){
    case (_, (Some(Point(_, y)), _)) => y.toDouble
    case (pY, (None, Some(tick))) => pY + tick * velocityY.before
  }

  val shape = new Circle(posX.map(_.toInt), posY.map(_.toInt), diameter)
}

class BoundingBox(val width: Signal[Int], val height: Signal[Int], val boundShape: Shape) {
  val centerBBHorizontalDistance = Signal{ (width() - boundShape.hitboxWidth()) / 2 }
  val centerBBVerticalDistance = Signal{ (height() - boundShape.hitboxHeight()) / 2 }

  val outOfBoundsLeft = Signal{ boundShape.centerX() < -centerBBHorizontalDistance() }
  val outOfBoundsRight = Signal{ boundShape.centerX() > centerBBHorizontalDistance() }
  val outOfBoundsTop = Signal{ boundShape.centerY() < -centerBBVerticalDistance() }
  val outOfBoundsBottom = Signal{ boundShape.centerY() > centerBBVerticalDistance() }

  val movedOutOfBoundsLeft = outOfBoundsLeft.changedTo(true)
  val movedOutOfBoundsRight = outOfBoundsRight.changedTo(true)
  val movedOutOfBoundsHorizontal = movedOutOfBoundsLeft || movedOutOfBoundsRight
  val movedOutOfBoundsVertical = Signal{ outOfBoundsTop() || outOfBoundsBottom() }.changedTo(true)

  val shape = new Rectangle(Var(0), Var(0), width, height, Signal{
    if(outOfBoundsLeft() || outOfBoundsRight() || outOfBoundsTop() || outOfBoundsBottom())
      Some(Color.RED) else Some(Color.GREEN)
  })
}

class Racket(val fieldWidth: Signal[Int], val fieldHeight: Signal[Int], val height: Signal[Int], val isRight: Boolean, val inputY: Signal[Int]) {
  val width = Var(10)

  val posX = fieldWidth.map(w => (if(isRight) 1 else -1) * (w / 2 - 25))
  val posY = Signal{ math.max(math.min(inputY(), (fieldHeight() - height()) / 2), - (fieldHeight() - height()) / 2) }

  val shape = new Rectangle(posX, posY, width, height)

  def collisionWith(collider: Shape): Event[Unit] = {
    val collisionBoxHeight = Signal{ height() + collider.hitboxHeight() }
    val collisionBoxWidth = Signal{ width() + collider.hitboxWidth() }
    val shapeInsideRacket = Signal{ (posX() - collisionBoxWidth() / 2 < collider.centerX()) &&
      (posX() + collisionBoxWidth() / 2 > collider.centerX()) &&
      (posY() - collisionBoxHeight() / 2 < collider.centerY()) &&
      (posY() + collisionBoxHeight() / 2 > collider.centerY())}
    shapeInsideRacket.changedTo(true)
  }
}

trait PongHost {
  val shapes: Signal[Traversable[Shape]]
  val fieldWidth: Signal[Int]
  val fieldHeight: Signal[Int]
  def setRacketInput(inputY: Signal[Int]) : Unit
}

object PongHostImpl extends SimpleSwingApplication with PongHost {
  override val shapes = Var(List[Shape]())
  val panel = new ShapesPanel(shapes)
  panel.preferredSize = new Dimension(400, 300)

  override val fieldWidth = panel.width.map(_ - 25)
  override val fieldHeight = panel.height.map(_ - 25)

  val bouncingCircle = new BouncingCircle(Var(50), panel.Mouse.middleButton.pressed)
  val boundingBox = new BoundingBox(fieldWidth, fieldHeight, bouncingCircle.shape)
  val racket = new Racket(fieldWidth, fieldHeight, Var(100), true, panel.Mouse.y)

  bouncingCircle.horizontalBounceSources.transform(boundingBox.movedOutOfBoundsHorizontal :: racket.collisionWith(bouncingCircle.shape) :: _)
  bouncingCircle.verticalBounceSources.transform(boundingBox.movedOutOfBoundsVertical :: _)

  shapes.transform(_ ++ List(bouncingCircle.shape, boundingBox.shape, racket.shape))

  override lazy val top = {
    new MainFrame {
      title = "Pong"
      contents = panel
      setLocationRelativeTo(new UIElement { override def peer = null })
    }
  }

  override def setRacketInput(inputY: Signal[Int]) : Unit = {
    val opponentRacket = new Racket(fieldWidth, fieldHeight, racket.height, false, inputY)
    shapes.transform(opponentRacket.shape :: _)
    bouncingCircle.horizontalBounceSources.transform(opponentRacket.collisionWith(bouncingCircle.shape) :: _)
  }

  override def main(args: Array[String]): Unit = {
    super.main(args)

    while(!top.visible) Thread.sleep(5)

    new Opponent(this).main(Array())

    while(top.visible) {
      Clock.tick()
      Thread.sleep(1)
    }
  }
}

// TODO make this an object with host as a remote.
class Opponent(host: PongHost) extends SimpleSwingApplication {
  val panel = new ShapesPanel(host.shapes)
  host.setRacketInput(panel.Mouse.y)

  override def top = {
    val mf = new MainFrame {
      title = "Player 2"
      contents = panel
    }
    Signal{(host.fieldWidth(), host.fieldHeight())}.observe{ case (w, h) =>
      Swing.onEDT(mf.size = new Dimension(w + 50, h + 60))
    }
    mf
  }
}
