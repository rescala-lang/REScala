package texteditor.imperative

import scala.swing.Publisher
import scala.swing.event.Event

import javax.swing

case class TimerEvent(val source: Timer) extends Event

class Timer(delay0: Int) extends Publisher {
  val peer: swing.Timer = new swing.Timer(delay0, null) {
    override def fireActionPerformed(e: java.awt.event.ActionEvent) =
      publish(new TimerEvent(Timer.this))
  }
  
  def this(delay: Int, repeating: Boolean) {
    this(delay)
    this.repeating = repeating
  }
  
  def running = peer.isRunning
  
  def delay = peer.getDelay
  def delay_=(delay: Int) = peer.setDelay(delay)
  def repeating = peer.isRepeats
  def repeating_=(repeating: Boolean) = peer.setRepeats(repeating)
  
  def restart = { peer.restart; this }
  def start = { peer.start; this }
  def stop = { peer.stop; this }
}