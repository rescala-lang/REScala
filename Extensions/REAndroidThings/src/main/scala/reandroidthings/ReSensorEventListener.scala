package reandroidthings

import android.hardware.SensorEvent
import android.hardware.SensorEventListener

// the pure interface does not require an Android peer
trait ReSensorEventListener {

  //  def this(sensorEventListener: SensorEventListener) {
  //    onSensorChanged = sensorEventListener.onSensorChanged
  //    onAccuracyChanged = sensorEventListener.onAccuracyChanged
  //  }

  def onSensorChanged(event: ReSensorEvent): Unit

  def onAccuracyChanged(sensor: ReSensor, accuracy: Int): Unit

}

object ReSensorEventListener {
  def wrap(sensorEventListener: SensorEventListener): ReSensorEventListener = {
    new ReSensorEventListener {
      override def onSensorChanged(event: ReSensorEvent): Unit = sensorEventListener.onSensorChanged(event.peer)

      override def onAccuracyChanged(sensor: ReSensor, accuracy: Int): Unit =
        sensorEventListener.onAccuracyChanged(sensor.peer, accuracy)
    }
  }


}
