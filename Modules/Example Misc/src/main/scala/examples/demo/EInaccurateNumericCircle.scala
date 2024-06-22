package examples.demo

import examples.demo.ui.{Circle, Shape, ShapesPanel}
import reactives.default.*

import java.awt.Dimension
import scala.swing.{MainFrame, SimpleSwingApplication, UIElement}

/** So far, we demonstrated the Signal abstraction, which allows modular,
  * yet declarative implementation of interactive applications through
  * deriving state with constraints. The persistent values of Signals
  * are complemented by briefly occurring, transient values of Events,
  * the second fundamental abstraction of RP for event-based programming.
  *
  * To introduce Events, we refactor our animation into a numeric
  * simulation. So far, we described the moving circle's trajectory
  * through arithmetic formulae. In the final application, though, the
  * ball should bounce off walls and rackets, making it much more
  * cumbersome to describe its trajectory arithmetically. Therefore, we
  * migrate its trajectory to be simulated through numeric integration,
  * using the euler method as the simplest approach. This means, the
  * application has to be driven in discrete time increments, which we
  * can easily model as Evt ticks. Equally to Vars being a manually
  * assignable type of Signals, Evts are Events which can be manually
  * triggered. Thus, we can add an according trigger in our main method.
  *
  * Events and Signals in RP are tightly integrated. We already showed,
  * how new Signals can be derived from existing ones, and new Events
  * can be derived equally. We will show Event expressions and several
  * Event combinators later. In addition, though, Signals and Events
  * can also be derived from each other. To implement our numeric
  * integration, we do in fact need such a transformation. First,
  * we use the previous definition of posX and posY now to define
  * the moving circle's velocityX and velocityY, adding a "per
  * nanosecond" factor to each. We then redefine Signals posX and
  * posY as the result of folding over the Event ticks. Just like
  * fold can compute an aggregate over all elements of a List, it
  * here computes an aggregate Signal over all Event values that
  * occurred so far, updated whenever the event emits a value.
  *
  * The euler method defines x for the next step as x of the previous
  * step plus the product of the velocity of the previous step times
  * the length of the step. X of the previous step and the length of
  * the step are given as parameters to the fold closure. To access
  * the velocity of the previous step, Signals offer the before method.
  * Usually, this method is necessary to break data dependency cycles.
  * For instance, for the Pong game, the ball's velocity is dependent
  * on the ball colliding with the field boundary and player rackets.
  * The balls collisions are dependent on the balls position. If the
  * balls position now would depend on the velocity, these dependencies
  * would form an endless loop. But, by using velocity.before, this
  * loop is broken, and the program is well-defined.
  */
object EInaccurateNumericCircle extends SimpleSwingApplication {
  val NanoSecond = 1000000000L

  val nsTime = Var(System.nanoTime())
  def tick() = nsTime.set(System.nanoTime())

  val ticks = Evt[Long]()

  val shapes = Var[List[Shape]](List.empty)
  val panel  = new ShapesPanel(shapes)

  val angle = nsTime.map(_.toDouble / NanoSecond * math.Pi)

  val velocity = Signal {
    Pos(
      x = (panel.width.value / 2 - 50).toDouble * math.sin(angle.value) / NanoSecond,
      y = (panel.height.value / 2 - 50).toDouble * math.cos(angle.value) / NanoSecond
    )
  }

  val inc = ticks.map(tick => velocity.value * tick.toDouble)

  val pos = inc.fold(Pos(0, 0)) { (cur, inc) => cur + inc }

  shapes.transform(new Circle(pos, Var(50)) :: _)

  override lazy val top = {
    panel.preferredSize = new Dimension(400, 300)
    new MainFrame {
      title = "REScala Demo"
      contents = panel
      setLocationRelativeTo(new UIElement { override def peer = null })
    }
  }

  override def main(args: Array[String]): Unit = {
    super.main(args)

    while !top.visible do Thread.sleep(5)
    while top.visible do {
      Thread.sleep(1)
      tick()
      ticks.fire(1 * NanoSecond / 1000L)
    }
  }
}
