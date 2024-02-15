package loci
package communicator
package webrtc

import org.scalajs.dom

import scala.scalajs.js.Array

object WebRTC extends WebRTCUpdate {

  trait Connector {
    def use(update: IncrementalUpdate): Unit
    def set(update: CompleteUpdate): Unit
    def connection: dom.RTCPeerConnection
  }

  trait ConnectorFactory {
    def incremental(update: IncrementalUpdate => Unit): Connector
    def complete(update: CompleteSession => Unit): Connector
  }

  def apply(channel: dom.RTCDataChannel): WebRTCChannelConnector =
    new WebRTCChannelConnector(channel)

  def defaultConfig  = new dom.RTCConfiguration { iceServers = Array[dom.RTCIceServer]() }
  def defaultOptions = new dom.RTCOfferOptions {}

  def offer(): ConnectorFactory = new ConnectorFactory {
    def incremental(update: IncrementalUpdate => Unit) =
      new WebRTCOffer(defaultConfig, defaultOptions, Left(update))
    def complete(update: CompleteSession => Unit) =
      val res = new WebRTCOffer(defaultConfig, defaultOptions, Right(update))
      res.connect()
      res
  }

  def answer(): ConnectorFactory = new ConnectorFactory {
    def incremental(update: IncrementalUpdate => Unit) =
      new WebRTCAnswer(defaultConfig, Left(update))
    def complete(update: CompleteSession => Unit) =
      new WebRTCAnswer(defaultConfig, Right(update))
  }
}
