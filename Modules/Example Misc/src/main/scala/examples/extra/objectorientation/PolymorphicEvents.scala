package examples.extra.objectorientation

import reactives.default.*

class Keyboard {
  protected lazy val press: Event[String] = Evt[String]()
  def keyPressed                          = press
  protected lazy val something            = Signal { 0 }
}

class Numpad extends Keyboard {
  // lazy val keyPressed = super.keyPressed && (_ => true)
  override def keyPressed     = super.keyPressed && (_ => true)
  override lazy val something = Signal { 1 }
}
