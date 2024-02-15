package loci
package communicator
package webrtc

import org.scalajs.dom

case class SessionDescription(descType: String, sdp: String) {
  def sessionDescription = new dom.RTCSessionDescription(
    new dom.RTCSessionDescriptionInit {
      this.`type` = SessionDescription.this.descType.asInstanceOf[dom.RTCSdpType]
      this.sdp = SessionDescription.this.sdp
    }
  )
}

object SessionDescription {
  def apply(value: dom.RTCSessionDescription): SessionDescription =
    SessionDescription(value.`type`.asInstanceOf[String], value.sdp)
}
