package de.tuda.stg

import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.util.Log
import rescala._
import reandroidthings._

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

    ReSensorManager.init(this.getApplicationContext)
    // TODO: check for interesting event-values (define classes + type parameter)
    val temperatureSensor = ReSensorManager.getSensor(ReSensor.TypeDynamicSensorMetaTemperature)

    //    temperatureSensor.value.map(_.toInt) observe { v => Log.d("Barometer4Android: ", String.valueOf(v)) }
    //    temperatureSensor.valueChanged += { v: Float => Log.d("Barometer4Android: ", String.valueOf(v)) }

    val temperatureReading: Signal[Float] = temperatureSensor.value

  }
}
