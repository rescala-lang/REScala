import android.app.{IntentService, Service}
import android.content.{Context, Intent}
import android.hardware.SensorManager
import android.os.{IBinder, Binder}
import android.util.Log

/**
  * class to setup the background for accessing Android-Sensors
  *
  * and allow communication between the Activity and the classes
  */
//class ReAndroidFactory extends IntentService {
//  var sensorManager: SensorManager = null
//
//  def init(): Unit = {
//    sensorManager = getSystemService(Context.SENSOR_SERVICE).asInstanceOf[SensorManager]
//    Log.d("Barometer4Android", "initFactory")
//  }
//
//
//  // ------------------------ Android stuff ------------------------
//  class LocalBinder extends Binder {
//    def getService: ReAndroidFactory = ReAndroidFactory.this
//  }
//  private val mBinder = new LocalBinder()
//
//  //  // onBind always returns null:
//  //  // see https://developer.android.com/reference/android/app/IntentService.html
//
//  //  override def onBind(intent: Intent): IBinder = null
//
//  override def onHandleIntent(intent: Intent): IBinder = {
//
//    sensorManager = getSystemService(Context.SENSOR_SERVICE).asInstanceOf[SensorManager]
//    Log.d("Barometer4Android", "initFactory")
//    mBinder
//  }
//
//
//}


