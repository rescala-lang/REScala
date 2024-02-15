package loci
package communicator
package webrtc

import org.scalajs.dom

import scala.scalajs.js.Array

object WebRTC extends WebRTCUpdate {

  trait Connector {
    def set(update: CompleteUpdate): Unit
  }

  trait ConnectorFactory {
    def complete(update: CompleteSession => Unit): Connector
  }

  def defaultConfig  = new dom.RTCConfiguration { iceServers = Array[dom.RTCIceServer]() }
  def defaultOptions = new dom.RTCOfferOptions {}

  def offer(): ConnectorFactory = new ConnectorFactory {
    def complete(update: CompleteSession => Unit) =
      val res = new WebRTCOffer(defaultConfig, defaultOptions, update)
      res.connect()
      res
  }

  def answer(): ConnectorFactory = new ConnectorFactory {
    def complete(update: CompleteSession => Unit) =
      new WebRTCAnswer(defaultConfig, update)
  }
}
