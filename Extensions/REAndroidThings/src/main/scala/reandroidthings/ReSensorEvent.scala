package reandroidthings

import android.hardware.Sensor
import android.hardware.SensorEvent
import rescala.graph.Pulse.Value

abstract class ReSensorEvent {

  /**
    * The underlying Android peer
    */
  def peer: SensorEvent

  def self = peer

  /** See {@Link android.hardware.SensorEvent SensorEvent} for documentation */
  var values: Array[Float]
  var sensor: ReSensor
  var accuracy: Int
  var timestamp: Long

  // does not work, because constructor of SensorEvent is not accessible (package local)
  //  def ReSensorEvent(valueSize: Int): ReSensorEvent = {
  //    val sensorEvent = new SensorEvent(valueSize)
  //    new ReSensorEvent {
  //      def peer = sensorEvent
  //    }
  //  }
}


object ReSensorEvent {

  def wrap(sensorEvent: SensorEvent): ReSensorEvent = {
    new ReSensorEvent {
      def peer = sensorEvent

      var values = peer.values
      var sensor = ReSensor.wrap(peer.sensor)
      var accuracy = peer.accuracy
      var timestamp = peer.timestamp
    }
  }
}


