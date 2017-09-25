package de.tuda.stg

import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.util.Log
import rescala._
import reandroidthings._

class MainActivity extends AppCompatActivity {
  private val TAG = "Barometer4Android"
  implicit val context = this


  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)
    Log.d(TAG, "Started ReSensor MainActivity")

    ReSensorManager.init(this.getApplicationContext)
    // get temperature sensor (requires cast)
    val temperatureSensor: ReSensor[Float] = ReSensorManager.getSensor(ReSensor.TypeDynamicSensorMetaTemperatureDescriptor)

    ReAlphaNumericDisplay.init(temperatureSensor.valueChanged)
  }


  override def onDestroy(): Unit = {
    super.onDestroy()
    Log.d(TAG, "Destroying ReSensor MainActivity")

    destroy()
  }


  def destroy(): Unit = {
    // remove sensors
    ReSensorManager.removeSensors()

    // turn off display
    ReAlphaNumericDisplay.destroy()
  }
}
