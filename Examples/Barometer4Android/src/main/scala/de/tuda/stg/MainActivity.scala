package de.tuda.stg

import android.content.Context
//import android.util.Log
import android.hardware.SensorManager
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.graphics.drawable.Animatable
import reandroidthings.ReSensor
import rescala._

class MainActivity extends AppCompatActivity {
  // allows accessing `.value` on TR.resource.constants
  implicit val context = this


  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)

    //    // type ascription is required due to SCL-10491
//    val vh: TypedViewHolder.main = TypedViewHolder.setContentView(this, TR.layout.main)
    //    vh.text.setText(s"Hello world, from ${TR.string.app_name.value}")
    //    vh.image.getDrawable match {
    //      case a: Animatable => a.start()
    //      case _ => // not animatable
    //    }

    // get SensorManager (unfortunately for the moment this must be done here, since 'getSystemService' can only
    // be accessed by an activity)
    val sensorManager: SensorManager = getSystemService(Context.SENSOR_SERVICE).asInstanceOf[SensorManager]

    // API - goal
    // Remark - wishful thinking: have "val pressureSensor = new ReSensor(ReSensor.TYPE_PRESSURE)", but only
    // activities, services, etc.... can access "getSystemService", maybe later with a "ScalaActivity", that can be called
    //    ReAndroidFactory.init()
    val temperatureSensor = new ReSensor(ReSensor.TypeDynamicSensorMetaTemperature, sensorManager)
    val temperatureReading = Signal {
      temperatureSensor.value()
    }
    val temperatureAccuracy = Signal {
      temperatureSensor.accuracy()
    }


//    var pressureSensor = ReSensorManager.getSensor(ReSensor.TypeDynamicSensorMetaPressure)
//    pressureSensor.value()
  }
}
