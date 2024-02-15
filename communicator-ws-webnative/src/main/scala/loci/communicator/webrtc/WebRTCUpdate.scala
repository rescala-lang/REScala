package loci
package communicator
package webrtc

import org.scalajs.dom

case class CompleteSession(descType: String, sdp: String) {
  def sessionDescription = new dom.RTCSessionDescription(
    new dom.RTCSessionDescriptionInit {
      this.`type` = CompleteSession.this.descType.asInstanceOf[dom.RTCSdpType]
      this.sdp = CompleteSession.this.sdp
    }
  )
}

object CompleteSession {
  def apply(value: dom.RTCSessionDescription): CompleteSession =
    CompleteSession(value.`type`.asInstanceOf[String], value.sdp)
}
