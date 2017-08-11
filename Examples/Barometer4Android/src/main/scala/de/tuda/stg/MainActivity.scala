package de.tuda.stg

import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.util.Log
import java.io.IOException

import android.graphics.drawable.Animatable
import rescala._
import reandroidthings._
import com.google.android.things.contrib.driver.ht16k33.AlphanumericDisplay


class MainActivity extends AppCompatActivity {
  private val TAG = "Barometer4Android"
  // allows accessing `.value` on TR.resource.constants
  implicit val context = this
  private var mDisplay: AlphanumericDisplay = null


  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)
    Log.d(TAG, "Started ReSensor MainActivity")

    // nice Activity View, if connected to Monitor
        makeNiceMonitorDisplay()

    ReSensorManager.init(this.getApplicationContext)
    // TODO: check for interesting event-values (define classes + type parameter)
    val temperatureSensor : ReTemperatureSensor = ReSensorManager.getSensor(ReSensor.TypeDynamicSensorMetaTemperature)

    //    temperatureSensor.value.map(_.toInt) observe { v => Log.d("Barometer4Android: ", String.valueOf(v)) }
    //    temperatureSensor.valueChanged += { v: Float => Log.d("Barometer4Android: ", String.valueOf(v)) }
    // Display on Alphanumeric Display of Rainbowhead
    temperatureSensor.valueChanged += { v: Float => updateDisplay(v.asInstanceOf[Double]) }

    val temperatureReading: Signal[Float] = temperatureSensor.value

    try {
      mDisplay = new AlphanumericDisplay("I2C1")
      mDisplay.setEnabled(true)
      mDisplay.clear
    } catch {
      case e: Exception => {
        import android.util.Log

        Log.e(TAG, "Error initializing display", e)
        mDisplay = null
      }
    }
  }


  private def updateDisplay(value: Double) = {
    if (mDisplay != null) try
      mDisplay.display(value)
    catch {
      case e: IOException =>
        Log.e(TAG, "" +
          "Error setting display", e)
    }
  }

  override def onDestroy(): Unit = {
    super.onDestroy()

    // remove sensors
    ReSensorManager.removeSensors()

    //
    if (mDisplay != null) try {
      mDisplay.clear
      mDisplay.setEnabled(false)
      mDisplay.close
    } catch {
      case e: IOException =>
        Log.e(TAG, "Error disabling display", e)
    } finally mDisplay = null
  }

  private def makeNiceMonitorDisplay(): Unit = {
    val vh: TypedViewHolder.main = TypedViewHolder.setContentView(this, TR.layout.main).asInstanceOf[TypedViewHolder.main]
    vh.text.setText(s"Hello world, from ${TR.string.app_name.value}")
    vh.image.getDrawable match {
      case a: Animatable => a.start()
      case _ => // not animatable
    }
  }
}
