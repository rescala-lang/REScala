package ex201x.reswingexamples.texteditor.signalsAndEventsFromImperative

import reactives.default.*

import javax.swing

class Timer(delay0: Int) {
  val peer: swing.Timer = new swing.Timer(delay0, null) {
    override def fireActionPerformed(e: java.awt.event.ActionEvent): Unit = {
      Timer.this.isRunning `set` this.isRunning(); fired.fire()
    }
  }

  def this(delay: Int, repeating: Boolean) = {
    this(delay)
    this.repeating = repeating
  }

  private val isRunning = Var(true)

  val running                               = Signal { isRunning.value }
  val fired                                 = Evt[Unit]()
  def delay                                 = peer.getDelay
  def delay_=(delay: Int)                   = peer.setDelay(delay)
  def repeating                             = peer.isRepeats
  def repeating_=(repeating: Boolean): Unit = { peer.setRepeats(repeating); isRunning `set` peer.isRunning() }

  def restart = { peer.restart(); isRunning `set` peer.isRunning(); this }
  def start   = { peer.start(); isRunning `set` peer.isRunning(); this }
  def stop    = { peer.stop(); isRunning `set` peer.isRunning(); this }
}
