package de.tuda.stg

import android.content.Context
import android.graphics.drawable.Animatable
import android.hardware._
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.util.Log
import java.io.IOException
import com.google.android.things.contrib.driver.bmx280.Bmx280SensorDriver
// TODO: SensorEventListener, SensorEvent

class MainActivity extends AppCompatActivity with SensorEventListener {
  // allows accessing `.value` on TR.resource.constants
  implicit val context = this

  var sensorManager: SensorManager = null
  private var mEnvironmentalSensorDriver: Bmx280SensorDriver = null
  private var mLastPressure = .0


  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)

    // get SensorService and cast it to SensorManager
    sensorManager = getSystemService(Context.SENSOR_SERVICE) match {
      case sm: SensorManager => sm
      case _ => throw new ClassCastException
    }
    //    val pressureSensor: Sensor = sensorManager.getDefaultSensor(Sensor.TYPE_PRESSURE)
    val deviceSensors = sensorManager.getSensorList(Sensor.TYPE_ALL)
    Log.d("Barometer4Android", deviceSensors.toString)
    //    print(pressureSensor == null)
    //    sensorManager.registerListener(this, pressureSensor, Sensor.)


    try {
      mEnvironmentalSensorDriver = new Bmx280SensorDriver("I2C1")
      sensorManager.registerDynamicSensorCallback(mDynamicSensorCallback)
      mEnvironmentalSensorDriver.registerTemperatureSensor
      mEnvironmentalSensorDriver.registerPressureSensor
      print("Initialized I2C BMP280")
    } catch {
      case e: IOException =>
        throw new RuntimeException("Error initializing BMP280", e)
    }
    sensorManager.registerDynamicSensorCallback(mDynamicSensorCallback)

    // type ascription is required due to SCL-10491
    val vh: TypedViewHolder.main = TypedViewHolder.setContentView(this, TR.layout.main)
    vh.text.setText(s"Hello world, from ${TR.string.app_name.value}")
    vh.image.getDrawable match {
      case a: Animatable => a.start()
      case _ => // not animatable
    }
  }


  def onSensorChanged(event: SensorEvent): Unit = {
    print("onSensorChanged")
  }

  def onAccuracyChanged(sensor: Sensor, accuracy: Int): Unit = {
    print("onAccuracyChanged")
  }


  private val mDynamicSensorCallback = new SensorManager.DynamicSensorCallback() {
    override def onDynamicSensorConnected(sensor: Sensor): Unit = {
      print("Hi")
      if (sensor.getType == Sensor.TYPE_AMBIENT_TEMPERATURE) { // Our sensor is connected. Start receiving temperature data.
        sensorManager.registerListener(mTemperatureListener, sensor, SensorManager.SENSOR_DELAY_NORMAL)
      }
      else if (sensor.getType == Sensor.TYPE_PRESSURE) { // Our sensor is connected. Start receiving pressure data.
        sensorManager.registerListener(mPressureListener, sensor, SensorManager.SENSOR_DELAY_NORMAL)
      }
    }

    override

    def onDynamicSensorDisconnected(sensor: Sensor): Unit = {
      super.onDynamicSensorDisconnected(sensor)
    }
  }

  // Callback when SensorManager delivers temperature data.
  private val mTemperatureListener = new SensorEventListener() {
    override def onSensorChanged(event: SensorEvent): Unit = {
      print("onSensorChanged - temperature " + event.values(0))
      //      mLastTemperature = event.values(0)
      //      Log.d(TAG, "sensor changed: " + mLastTemperature)
    }

    override

    def onAccuracyChanged(sensor: Sensor, accuracy: Int): Unit = {
      print("onAccuracyChanged - temperature " + accuracy)
    }
  }

  // Callback when SensorManager delivers pressure data.
  private val mPressureListener = new SensorEventListener() {
    override def onSensorChanged(event: SensorEvent): Unit = {
      print("onSensorChanged - pressure " + event.values(0))
      //      mLastPressure = event.values(0)
      //      Log.d(TAG, "sensor changed: " + mLastPressure)
      //      if (mDisplayMode eq DisplayMode.PRESSURE) updateDisplay(mLastPressure)
      //      updateBarometer(mLastPressure)
    }

    override

    def onAccuracyChanged(sensor: Sensor, accuracy: Int): Unit = {
      print("onAccuracyChanged - pressure " + accuracy)
    }
  }
}
