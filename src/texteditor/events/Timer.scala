package texteditor.events

import scala.swing.event.Event

import javax.swing
import rescala.events.ImperativeEvent

case class TimerEvent(val source: Timer) extends Event

class Timer(delay0: Int) {
  val peer: swing.Timer = new swing.Timer(delay0, null) {
    override def fireActionPerformed(e: java.awt.event.ActionEvent) = fired()
  }
  
  def this(delay: Int, repeating: Boolean) {
    this(delay)
    this.repeating = repeating
  }
  
  def running = peer.isRunning
  val fired = new ImperativeEvent[Unit] //#EVT
  
  def delay = peer.getDelay
  def delay_=(delay: Int) = peer.setDelay(delay)
  def repeating = peer.isRepeats
  def repeating_=(repeating: Boolean) = peer.setRepeats(repeating)
  
  def restart = { peer.restart; this }
  def start = { peer.start; this }
  def stop = { peer.stop; this }
}