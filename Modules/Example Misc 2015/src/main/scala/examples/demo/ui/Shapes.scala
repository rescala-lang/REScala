package examples.demo.ui

import examples.demo.Pos
import reactives.core.AdmissionTicket
import reactives.default.*
import reactives.default.global.State as BundleState

import java.awt.{Color, Graphics2D}

trait Shape extends Serializable {
  val changed: Event[Any]
  val centerX: Signal[Int]
  val centerY: Signal[Int]
  val hitboxWidth: Signal[Int]
  val hitboxHeight: Signal[Int]
  def drawSnapshot(g: Graphics2D)(implicit turn: AdmissionTicket[BundleState]): Unit
}

class Circle(
    center: Signal[examples.demo.Pos],
    val diameter: Signal[Int],
    val border: Signal[Option[Color]] = Var(Some(Color.BLACK)),
    val fill: Signal[Option[Color]] = Var(None)
) extends Shape {
  def this(cx: Signal[Int], cy: Signal[Int], dia: Signal[Int]) =
    this(Signal { Pos(cx.value.toDouble, cy.value.toDouble) }, dia)
  override val centerX: Signal[Int] = center.map(_.x.toInt)
  override val centerY: Signal[Int] = center.map(_.y.toInt)
  override val changed      = centerX.changed || centerY.changed || diameter.changed || border.changed || fill.changed
  override val hitboxWidth  = diameter
  override val hitboxHeight = diameter
  override def drawSnapshot(g: Graphics2D)(implicit turn: AdmissionTicket[BundleState]): Unit = {
    val d = turn.now(diameter)
    val x = turn.now(centerX) - d / 2
    val y = turn.now(centerY) - d / 2
    val f = turn.now(fill)
    if f.isDefined then {
      g.setColor(f.get)
      g.fillOval(x, y, d, d)
    }
    val b = turn.now(border)
    if b.isDefined then {
      g.setColor(b.get)
      g.drawOval(x, y, d, d)
    }
  }
}

class Rectangle(
    override val centerX: Signal[Int],
    override val centerY: Signal[Int],
    override val hitboxWidth: Signal[Int],
    override val hitboxHeight: Signal[Int],
    val border: Signal[Option[Color]] = Var(Some(Color.BLACK)),
    val fill: Signal[Option[Color]] = Var(None)
) extends Shape {
  override val changed =
    centerX.changed || centerY.changed || hitboxWidth.changed || hitboxHeight.changed || border.changed || fill.changed
  override def drawSnapshot(g: Graphics2D)(implicit turn: AdmissionTicket[BundleState]): Unit = {
    val w = turn.now(hitboxWidth)
    val h = turn.now(hitboxHeight)
    val x = turn.now(centerX) - w / 2
    val y = turn.now(centerY) - h / 2
    val f = turn.now(fill)
    if f.isDefined then {
      g.setColor(f.get)
      g.fillRect(x, y, w, h)
    }
    val b = turn.now(border)
    if b.isDefined then {
      g.setColor(b.get)
      g.drawRect(x, y, w, h)
    }
  }
}
