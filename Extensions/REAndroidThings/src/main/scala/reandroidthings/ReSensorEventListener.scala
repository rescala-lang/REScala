package reandroidthings

import android.hardware.{Sensor, SensorEvent, SensorEventListener}


trait ReSensorEventListener {

  def peer: android.hardware.SensorEventListener

  def self = peer

  def onSensorChanged(event: ReSensorEvent): Unit

  def onAccuracyChanged(sensor: ReSensor, accuracy: Int): Unit

}

object ReSensorEventListener {
  def wrap(sensorEventListener: SensorEventListener): ReSensorEventListener = {

    new ReSensorEventListener {
      def peer = sensorEventListener
      override def onSensorChanged(event: ReSensorEvent): Unit = sensorEventListener.onSensorChanged(event.peer)
      override def onAccuracyChanged(sensor: ReSensor, accuracy: Int): Unit =
        sensorEventListener.onAccuracyChanged(sensor.peer, accuracy)
    }
  }


}
