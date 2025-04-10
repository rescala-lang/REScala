package lofi_acl.sync

import crypto.PublicIdentity

import java.util.concurrent.LinkedBlockingQueue
import scala.collection.mutable

private[sync] trait CausalityCheckingMessageHandler[MSG] extends Runnable with MessageReceiver[MSG] {
  val msgQueue: LinkedBlockingQueue[(MSG, PublicIdentity)] = LinkedBlockingQueue()
  @volatile private var stopped                            = false

  override def run(): Unit =
    val backlog = new mutable.Queue[(MSG, PublicIdentity)]()
    while !stopped do {
      try {
        val msgTuple @ (msg, sender) = msgQueue.take()
        if canHandleMessage(msg) then {
          if handleMessage(msg, sender) then {
            val backlogMessages = backlog.removeAll()
            val unhandledMessages = backlogMessages.filter { (msg, sender) =>
              if !canHandleMessage(msg) then false
              else
                handleMessage(msg, sender)
                true
            }
            backlog.addAll(unhandledMessages)
          }
        } else {
          newMessageWithMissingPredecessors(msg: MSG, sender)
          backlog.enqueue(msgTuple)
        }
      } catch
        case e: InterruptedException =>
    }

  def setStopped(): Unit = stopped = true

  /** Checks whether the message can be handled */
  def canHandleMessage(msg: MSG): Boolean

  /** Handles the message and returns whether this message has changed the causal context. */
  def handleMessage(msg: MSG, sender: PublicIdentity): Boolean

  def newMessageWithMissingPredecessors(msg: MSG, sender: PublicIdentity): Unit
}
