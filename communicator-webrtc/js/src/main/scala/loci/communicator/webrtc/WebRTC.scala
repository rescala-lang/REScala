package loci
package communicator
package webrtc

import org.scalajs.dom

import scala.scalajs.js.Array

trait WebRTC
    extends Protocol
    with SetupInfo
    with SecurityInfo with Secure
    with SymmetryInfo with Bidirectional {
  override def toString = "WebRTC()"
}

object WebRTC extends WebRTCUpdate {
  def unapply(webRTC: WebRTC) = true

  trait Connector extends communicator.Connector[WebRTC] {
    def use(update: IncrementalUpdate): Unit
    def set(update: CompleteUpdate): Unit
    def connection : dom.RTCPeerConnection
  }

  trait ConnectorFactory {
    def incremental(update: IncrementalUpdate => Unit): Connector
    def complete(update: CompleteSession => Unit): Connector
  }

  def apply(channel: dom.RTCDataChannel): communicator.Connector[WebRTC] =
    new WebRTCChannelConnector(channel, None)

  def offer(
      configuration: dom.RTCConfiguration =
        new dom.RTCConfiguration { iceServers = Array[dom.RTCIceServer]() },
      options: dom.RTCOfferOptions =
        new dom.RTCOfferOptions { }): ConnectorFactory =
    new ConnectorFactory {
      def incremental(update: IncrementalUpdate => Unit) =
        new WebRTCOffer(configuration, options, Left(update))
      def complete(update: CompleteSession => Unit) =
        new WebRTCOffer(configuration, options, Right(update))
    }

  def answer(
      configuration: dom.RTCConfiguration =
        new dom.RTCConfiguration { iceServers = Array[dom.RTCIceServer]() }): ConnectorFactory =
    new ConnectorFactory {
      def incremental(update: IncrementalUpdate => Unit) =
        new WebRTCAnswer(configuration, Left(update))
      def complete(update: CompleteSession => Unit) =
        new WebRTCAnswer(configuration, Right(update))
    }
}
