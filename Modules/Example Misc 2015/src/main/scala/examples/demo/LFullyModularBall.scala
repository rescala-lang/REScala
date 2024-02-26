package examples.demo

import examples.demo.GModularClockCircle.Clock
import examples.demo.ui.{Circle, Point, Shape, ShapesPanel}
import reactives.default._

/** To resolve this circular initialization order, we introduce
  * higher-order reactives. Higher-order reactives are Signals
  * or Events, whose values contain references to other Signals
  * or Events. We have already been using a higher-order
  * reactive for a while, namely since we made the circle's
  * trajectory scale with the ShapesPanel's size. We started
  * using shapes in its initialization, defined as a
  * Var[List[Shape]], and each Shape contains several Signals
  * defining its position, size, etc. Thus, shapes already is a
  * higher-order reactive.
  *
  * To implement our horizontal and vertical bounces, we now use
  * a similar pattern, although their higher-order-ness is much
  * clearer. For both, we define a Var[List[Event[Any]]], which
  * we initialize with an empty List. This signature clearly
  * shows, that the outer Var contains pointers to several inner
  * Events. To be able to use these higher-order reactives in
  * place of the leftButton.pressed and rightButton.pressed, we
  * need to flatten these higher-order reactives into regular
  * Events. The according flatten method is available on any
  * reactive, whose values reference other reactives either
  * directly or indirected through any collection of the scala
  * standard library. The type of reactive returned is a normal
  * reactive (or, one layer less higher-order), with the concrete
  * type depending on the type signature of the higher-order
  * reactive that is being flattened. E.g., a Signal[Signal[T]]
  * will simply become a Signal[T]. A Signal[Event[T]] will
  * become an Event[T]. A Signal[Set[Signal[T]] will become a
  * Signal[Set[T]]. A Signal[List[Event[T]]], as we use it here,
  * is the most complex case, becoming an Event[List[Option[T]]].
  * The Option wrapper models, that not just one, but each Event
  * in the Signal may or may not have emitted a value
  * simultaneously. Higher-order reactives containing only a
  * single Event rather than a multiples do not have this option
  * wrapper because it is redundant -- if the one inner Event
  * did not emit a value, then the flattened result will also
  * not emit a value, i.e., the None case never occurs.
  *
  * Having implemented both higher-order bounce Vars and having
  * changed both velocities to depend on their flattened
  * counterparts, the BouncingBall class is now independent of
  * the ShapesPanel. To complete the refactoring, all that is
  * left to do is add the left and right mouse button to the
  * respective list of bounce sources, equally to how the ball
  * shape is added to the list of displayed shapes for the
  * ShapesPanel.
  */
object LFullyModularBall extends Main {
  class BouncingBall(val initVx: Double, val initVy: Double, val diameter: Signal[Int], val reset: Event[Point]) {
    val horizontalBounceSources: Var[List[Event[Any]]] = Var(List())
    val verticalBounceSources: Var[List[Event[Any]]]   = Var(List())
    val filteredHorizontalBounceSources = horizontalBounceSources.map(_.map(_.recover {
      case _: IllegalArgumentException => None
    }))

    val velocity = Signal {
      Pos(
        x = horizontalBounceSources.flatten[Event[Any]](firstFiringEvent)
          .fold(initVx / Clock.NanoSecond) { (old, _) => -old }.value,
        y = verticalBounceSources.flatten[Event[Any]](firstFiringEvent)
          .fold(initVy / Clock.NanoSecond) { (old, _) => -old }.value
      )
    }

    // TODO: using now to remove cycle …
    val inc = Clock.ticks.map(tick => velocity.readValueOnce * tick.toDouble)

    val pos = Fold(Pos(0, 0))(
      reset act { case Point(x, y) => Pos(x.toDouble, y.toDouble) },
      inc act { inc => current + inc }
    )

    val shape = new Circle(pos, diameter)
  }

  val shapes = Var[List[Shape]](List.empty)
  val panel  = new ShapesPanel(shapes)

  val bouncingBall = new BouncingBall(200d, 150d, Var(50), panel.Mouse.middleButton.pressed)
  shapes.transform(bouncingBall.shape :: _)

  bouncingBall.horizontalBounceSources.transform(panel.Mouse.leftButton.pressed :: _)
  bouncingBall.verticalBounceSources.transform(panel.Mouse.rightButton.pressed :: _)
}
