package de.tuda.stg

import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.util.Log

import rescala._
import reandroidthings._


class MainActivity extends AppCompatActivity {
  private val TAG = "Barometer4Android"
  implicit val context = this
  var alphNumDisplay: ReAlphaNumericDisplay = null


  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)
    Log.d(TAG, "Started ReSensor MainActivity")

    // nice Activity View, if connected to Monitor
    //    makeNiceMonitorDisplay()

    ReSensorManager.init(this.getApplicationContext)
    // get temperature sensor (requires cast)
    val temperatureSensor: ReTemperatureSensor =
      ReSensorManager.getSensor(ReSensor.TypeDynamicSensorMetaTemperature).asInstanceOf[ReTemperatureSensor]

    alphNumDisplay = new ReAlphaNumericDisplay(temperatureSensor.valueChanged)
    alphNumDisplay.init

    //    // turn display off and on again and simulate stop (all for testing reasons)
    //    import scala.concurrent.Future
    //    import scala.concurrent.ExecutionContext.Implicits.global
    //    val future = Future {
    //      Thread.sleep(3000)
    //      alphNumDisplay.turnOff
    //
    //      Thread.sleep(3000)
    //      alphNumDisplay.turnOn
    //
    //      //      Thread.sleep(3000)
    //      //      simulateStop()
    //    }
  }

  // actual method called on stopped activity
  override def onDestroy(): Unit = {
    super.onDestroy()
    Log.d(TAG, "Destroying ReSensor MainActivity")

    destroy()
  }

  def destroy(): Unit = {
    // remove sensors
    ReSensorManager.removeSensors()

    // turn off display
    alphNumDisplay.destroy()
  }

  // simulate stopping the activity (only for testing)
  //  def simulateStop(): Unit = {
  //    destroy()
  //  }

  //  private def makeNiceMonitorDisplay(): Unit = {
  //  import android.graphics.drawable.Animatable
  //    val vh: TypedViewHolder.main =
  //      TypedViewHolder.setContentView(this, TR.layout.main).asInstanceOf[TypedViewHolder.main]
  //    vh.text.setText(s"Hello world, from ${TR.string.app_name.value}")
  //    vh.image.getDrawable match {
  //      case a: Animatable => a.start()
  //      case _ => // not animatable
  //    }
  //  }
}
