package loci
package communicator
package webrtc

import org.scalajs.dom

import scala.annotation.compileTimeOnly

trait WebRTC
    extends Protocol
    with SetupInfo
    with SecurityInfo with Secure
    with SymmetryInfo with Bidirectional {
  override def toString = "WebRTC()"
}

object WebRTC extends WebRTCUpdate {
  def unapply(webRTC: WebRTC) = true

  private def unavailable = sys.error("WebRTC communicator only available in JS")

  trait Connector extends communicator.Connector[WebRTC] {
    def use(update: IncrementalUpdate): Unit
    def set(update: CompleteUpdate): Unit
  }

  trait ConnectorFactory {
    def incremental(update: IncrementalUpdate => Unit): Connector
    def complete(update: CompleteSession => Unit): Connector
  }

  @compileTimeOnly("WebRTC communicator only available in JS")
  def apply(channel: dom.RTCDataChannel): communicator.Connector[WebRTC] = unavailable

  @compileTimeOnly("WebRTC communicator only available in JS")
  def offer(
    configuration: dom.RTCConfiguration = new dom.RTCConfiguration { },
    options: dom.RTCOfferOptions = new dom.RTCOfferOptions { }): ConnectorFactory = unavailable

  @compileTimeOnly("WebRTC communicator only available in JS")
  def answer(
    configuration: dom.RTCConfiguration = new dom.RTCConfiguration { }): ConnectorFactory = unavailable
}
