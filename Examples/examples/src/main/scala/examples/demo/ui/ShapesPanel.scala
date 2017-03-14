package examples.demo.ui;

import java.awt.{Color, Graphics2D}

import rescala._

import scala.swing.Panel

class ShapesPanel extends Panel {
  val shapes = Var(List[Shape]())
  //val allChanges: Event[_] = Event { shapes().find{ shape: Shape => shape.changed().isDefined } }
  val allChanges: Event[_] = shapes.map(_.map(_.changed)).flatten
  allChanges observe {_ => repaint() }

  override def paintComponent(g: Graphics2D): Unit = {
    Engine.plan() {implicit turn =>
      for (shape <- shapes.now) {
        shape.drawSnapshot(turn, g)
      }
    }
  }
}

trait Shape {
  val changed: Event[_]
  def drawSnapshot(turn: Turn, g: Graphics2D): Unit
}

class Circle (val centerX: Signal[Int],
              val centerY: Signal[Int],
              val radius: Signal[Int],
              val border: Signal[Option[Color]],
              val fill: Signal[Option[Color]]) extends Shape {
  override val changed = centerX.changed || centerY.changed || radius.changed || border.changed || fill.changed
  override def drawSnapshot(turn: Turn, g: Graphics2D): Unit = {
    val r = radius.now
    val x = centerX.now - r
    val y = centerY.now - r
    val dia = r * 2
    val f = fill.now
    if(f.isDefined) {
      g.setColor(f.get)
      g.fillOval(x, y, dia, dia)
    }
    val b = border.now
    if(b.isDefined) {
      g.setColor(b.get)
      g.drawOval(x, y, dia, dia)
    }
  }
}

class Rectangle (val centerX: Signal[Int],
                 val centerY: Signal[Int],
                 val width: Signal[Int],
                 val height: Signal[Int],
                 val border: Signal[Option[Color]],
                 val fill: Signal[Option[Color]]) extends Shape {
  override val changed = centerX.changed || centerY.changed || width.changed || height.changed || border.changed || fill.changed
  override def drawSnapshot(turn: Turn, g: Graphics2D): Unit = {
    val w = width.now
    val h = height.now
    val x = centerX.now - w/2
    val y = centerY.now - h/2
    val f = fill.now
    if(f.isDefined) {
      g.setColor(f.get)
      g.fillRect(x, y, w, h)
    }
    val b = border.now
    if(b.isDefined) {
      g.setColor(b.get)
      g.drawRect(x, y, w, h)
    }
  }
}
