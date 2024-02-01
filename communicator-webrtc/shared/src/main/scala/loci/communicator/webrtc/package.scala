package loci
package communicator

import webrtc.WebRTC._

import transmitter._

package webrtc {
  protected[webrtc] trait WebRTCUpdateTransmittable {
    implicit val transmittableUpdate: TransformingTransmittable[
        Update,
        (Option[(String, String)], Option[(String, String)], Option[(String, String, Double)]),
        Update] =
      TransformingTransmittable(
        provide = (value, context) => value match {
          case CompleteSession(descType, sdp) =>
            (Some((descType, sdp)), None, None)
          case InitialSession(descType, sdp) =>
            (None, Some((descType, sdp)), None)
          case SessionUpdate(candidate, sdpMid, sdpMLineIndex) =>
            (None, None, Some((candidate, sdpMid, sdpMLineIndex)))
        },
        receive = (value, context) => value match {
          case (Some((descType, sdp)), _, _) =>
            CompleteSession(descType, sdp)
          case (_, Some((descType, sdp)), _) =>
            InitialSession(descType, sdp)
          case (_, _, Some((candidate, sdpMid, sdpMLineIndex))) =>
            SessionUpdate(candidate, sdpMid, sdpMLineIndex)
          case _ =>
            throw new RemoteAccessException("invalid WebRTC update")
        })
  }

  protected[webrtc] trait WebRTCUpdateTransmittables extends WebRTCUpdateTransmittable {
    implicit val transmittableIncrementalUpdate: TransformingTransmittable[
        IncrementalUpdate,
        (Option[(String, String)], Option[(String, String, Double)]),
        IncrementalUpdate] =
      TransformingTransmittable(
        provide = (value, context) => value match {
          case InitialSession(descType, sdp) =>
            (Some((descType, sdp)), None)
          case SessionUpdate(candidate, sdpMid, sdpMLineIndex) =>
            (None, Some((candidate, sdpMid, sdpMLineIndex)))
        },
        receive = (value, context) => value match {
          case (Some((descType, sdp)), _) =>
            InitialSession(descType, sdp)
          case (_, Some((candidate, sdpMid, sdpMLineIndex))) =>
            SessionUpdate(candidate, sdpMid, sdpMLineIndex)
          case _ =>
            throw new RemoteAccessException("invalid WebRTC update")
        })

    implicit val transmittableCompleteUpdate: TransformingTransmittable[
        CompleteUpdate,
        (String, String),
        CompleteUpdate] =
      TransformingTransmittable(
        provide = (value, context) => value match {
          case CompleteSession(descType, sdp) =>
            (descType, sdp)
        },
        receive = (value, context) => value match {
          case (descType, sdp) =>
            CompleteSession(descType, sdp)
        })
  }
}

package object webrtc extends webrtc.WebRTCUpdateTransmittables {
  implicit val transmittableInitialSession: TransformingTransmittable[
    InitialSession, (String, String), InitialSession] =
    TransformingTransmittable(
      provide = (value, context) => (value.descType, value.sdp),
      receive = (value, context) => InitialSession(value._1, value._2))

  implicit val transmittableSessionUpdate: TransformingTransmittable[
      SessionUpdate, (String, String, Double), SessionUpdate] =
    TransformingTransmittable(
      provide = (value, context) => (value.candidate, value.sdpMid, value.sdpMLineIndex),
      receive = (value, context) => SessionUpdate(value._1, value._2, value._3))

  implicit val transmittableCompleteSession: TransformingTransmittable[
      CompleteSession, (String, String), CompleteSession] =
    TransformingTransmittable(
      provide = (value, context) => (value.descType, value.sdp),
      receive = (value, context) => CompleteSession(value._1, value._2))
}
