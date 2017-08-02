package reandroidthings

import java.io.IOException

import android.hardware.{Sensor, SensorEvent, SensorEventListener, SensorManager}
import android.util.Log
import driver.bmx280.Bmx280SensorDriver
import iot_devices.BoardDefaults
import rescala._

//abstract class ReSensor {
class ReSensor(sensor: Sensor) {

  /**
    * The SensorManager associated with this Sensor
    */
  var reSensorManager: ReSensorManager = null
  // the buildModel, e.g. "iot_rpi3"
  private var buildModel: String = android.os.Build.MODEL
  private var mEnvironmentalSensorDriver: Bmx280SensorDriver = null

  /**
    * The current value and accuracy (as ReScala-Vars)
    */
  var value: Var[Float] = Var(Float.MinValue)
  var accuracy: Var[Int] = Var(Int.MinValue)

  /**
    * The underlying Android peer
    */
  var peer: Sensor = sensor

  def self = peer

  def this(sensorType: Int, sensorManager: SensorManager) {
    this(null);

    // init SensorManager and Sensor
    reSensorManager = ReSensorManager.wrap(sensorManager)
    peer = sensorManager.getDefaultSensor(sensorType)

    // dynamic sensor stuff
    val dynamicPressure: Int = ReSensor.TypeDynamicSensorMetaPressure
    val dynamicTemperature: Int = ReSensor.TypeDynamicSensorMetaTemperature
    if (sensorType == dynamicPressure || sensorType == dynamicTemperature) {
      try {
        mEnvironmentalSensorDriver = new Bmx280SensorDriver(BoardDefaults.getI2cBus)
        //        mEnvironmentalSensorDriver = new Bmx280SensorDriver("I2C1")
        reSensorManager.registerDynamicSensorCallback(mDynamicSensorCallback)
        if (sensorType == dynamicTemperature) {
          mEnvironmentalSensorDriver.registerTemperatureSensor
        } else if (sensorType == dynamicPressure) {
          mEnvironmentalSensorDriver.registerPressureSensor
        }
        Log.i("Barometer4Android", "Initialized I2C BMP280")
      } catch {
        case e: IOException => throw new RuntimeException("Error initializing I2C BMP280", e)
      }
    }
  }

  /**
    * its methods
    */
  def name: String = peer.getName

  def vendor: String = peer.getVendor

  def `type`: Int = peer.getType

  def version: Int = peer.getVersion

  def id: Int = peer.getId

  def stringType: String = peer.getStringType

  def maximumRange: Float = peer.getMaximumRange

  def resolution: Float = peer.getResolution

  def power: Float = peer.getPower

  def fifoReservedEventCount: Int = peer.getFifoReservedEventCount

  def fifoMaxEventCount: Int = peer.getFifoMaxEventCount

  def minDelay: Int = peer.getMinDelay

  def maxDelay: Int = peer.getMaxDelay

  def isWakeUpSensor: Boolean = peer.isWakeUpSensor

  def isDynamicSensor: Boolean = peer.isDynamicSensor

  def isAdditionalInfoSupported: Boolean = peer.isAdditionalInfoSupported

  def getReportingMode: Int = peer.getReportingMode


  private val abstractSensorListener = ReSensorEventListener.wrap(new SensorEventListener {

    override def onSensorChanged(event: SensorEvent): Unit = {
      //      try {
      value() = event.values(0)
      //      } catch {
      //        case e: Exception => throw new RuntimeException("Error assigning Value Vars", e)
      //      }
    }

    override def onAccuracyChanged(sensor: Sensor, newAccuracy: Int): Unit =
    //      try {
      accuracy() = newAccuracy

    //      } catch {
    //        case e: Exception => throw new RuntimeException("Error assigning Accuracy Vars", e)
    //      }
  }


  )

  /*
    special handling of IOT devices/ dynamic sensors
  */
  private val mDynamicSensorCallback = new ReSensorManager.DynamicSensorCallback() {
    override def onDynamicSensorConnected(sensor: ReSensor): Unit = {
      if (sensor.`type` == ReSensor.TypeAmbientTemperature) {
        reSensorManager.registerListener(abstractSensorListener, sensor, SensorManager.SENSOR_DELAY_NORMAL)
      }
      else if (sensor.`type` == ReSensor.TypePressure) {
        reSensorManager.registerListener(abstractSensorListener, sensor, SensorManager.SENSOR_DELAY_NORMAL)
      }
    }

    override def onDynamicSensorDisconnected(sensor: ReSensor): Unit = {
      super.onDynamicSensorDisconnected(sensor)
    }
  }

}


object ReSensor {

  def wrap(sensor: Sensor): ReSensor = {
    new ReSensor(sensor)
  }

  val TypeAccelerometer = Sensor.TYPE_ACCELEROMETER
  val StringTypeAccelerometer = Sensor.STRING_TYPE_ACCELEROMETER

  val TypeMagneticField = Sensor.TYPE_MAGNETIC_FIELD
  val StringTypeMagneticField = Sensor.STRING_TYPE_MAGNETIC_FIELD

  val TypeMagneticFieldUncalibrated = Sensor.TYPE_MAGNETIC_FIELD_UNCALIBRATED
  val StringTypeMagneticFieldUncalibrated = Sensor.STRING_TYPE_MAGNETIC_FIELD_UNCALIBRATED

  val TypeGyroscope = Sensor.TYPE_GYROSCOPE
  val StringTypeGyroscope = Sensor.STRING_TYPE_GYROSCOPE

  val TypeLight = Sensor.TYPE_LIGHT
  val StringTypeLight = Sensor.STRING_TYPE_LIGHT

  val TypePressure = Sensor.TYPE_PRESSURE
  val StringTypePressure = Sensor.STRING_TYPE_PRESSURE

  val TypeProximity = Sensor.TYPE_PROXIMITY
  val StringTypeProximity = Sensor.STRING_TYPE_PROXIMITY

  val TypeGravity = Sensor.TYPE_GRAVITY
  val StringTypeGravity = Sensor.STRING_TYPE_GRAVITY

  val TypeLinearAcceleration = Sensor.TYPE_LINEAR_ACCELERATION
  val StringTypeLinearAcceleration = Sensor.STRING_TYPE_LINEAR_ACCELERATION

  val TypeRotationVector = Sensor.TYPE_ROTATION_VECTOR
  val StringTypeRotationVector = Sensor.STRING_TYPE_ROTATION_VECTOR

  val TypeGameRotationVector = Sensor.TYPE_GAME_ROTATION_VECTOR
  val StringTypeGameRotationVector = Sensor.STRING_TYPE_GAME_ROTATION_VECTOR

  val TypeRelativeHumidity = Sensor.TYPE_RELATIVE_HUMIDITY
  val StringTypeRelativeHumidity = Sensor.STRING_TYPE_RELATIVE_HUMIDITY

  val TypeAmbientTemperature = Sensor.TYPE_AMBIENT_TEMPERATURE
  val StringTypeAmbientTemperature = Sensor.STRING_TYPE_AMBIENT_TEMPERATURE

  val TypeGyroscopeUncalibrated = Sensor.TYPE_GYROSCOPE_UNCALIBRATED
  val StringTypeGyroscopeUncalibrated = Sensor.STRING_TYPE_GYROSCOPE_UNCALIBRATED

  val TypeSignificantMotion = Sensor.TYPE_SIGNIFICANT_MOTION
  val StringTypeGyroscopeUncalibrated = Sensor.STRING_TYPE_SIGNIFICANT_MOTION

  val TypeStepDetector = Sensor.TYPE_STEP_DETECTOR
  val StringTypeStepDetector = Sensor.STRING_TYPE_STEP_DETECTOR

  val TypeStepCounter = Sensor.TYPE_STEP_COUNTER
  val StringTypeStepCounter = Sensor.STRING_TYPE_STEP_COUNTER

  val TypeGeomagneticRotationVector = Sensor.TYPE_GEOMAGNETIC_ROTATION_VECTOR
  val StringTypeGeomagneticRotationVector = Sensor.STRING_TYPE_GEOMAGNETIC_ROTATION_VECTOR

  val TypeHeartRate = Sensor.TYPE_HEART_RATE
  val StringTypeHeartRate = Sensor.STRING_TYPE_HEART_RATE

  val TypePose6DOF = Sensor.TYPE_POSE_6DOF
  val StringTypePose6DOF = Sensor.STRING_TYPE_POSE_6DOF

  val TypeStationaryDetect = Sensor.TYPE_STATIONARY_DETECT
  val StringTypeStationaryDetect = Sensor.STRING_TYPE_STATIONARY_DETECT

  val TypeMotionDetect = Sensor.TYPE_MOTION_DETECT
  val StringTypeMotionDetect = Sensor.STRING_TYPE_MOTION_DETECT

  val TypeHeartBeat = Sensor.TYPE_HEART_BEAT
  val StringTypeHeartBeat = Sensor.STRING_TYPE_HEART_BEAT

  // need to be accessed via reflection, because they are usually hidden
  val TypeDynamicSensorMeta = classOf[Sensor].getDeclaredField("TYPE_DYNAMIC_SENSOR_META").getInt(null)
  val StringTypeHeartBeat = classOf[Sensor].getDeclaredField("STRING_TYPE_DYNAMIC_SENSOR_META").get(null).toString

  val TypeDynamicSensorMetaPressure = 40
  val TypeDynamicSensorMetaTemperature = 41

  val TypeAll = Sensor.TYPE_ALL
  val TypeDevicePrivateBase = Sensor.TYPE_DEVICE_PRIVATE_BASE
  val ReportingModeContinuous = Sensor.REPORTING_MODE_CONTINUOUS
  val ReportingModeOnChange = Sensor.REPORTING_MODE_ON_CHANGE
  val ReportingModeOneShot = Sensor.REPORTING_MODE_ONE_SHOT
  val ReportingModeSpecialTrigger = Sensor.REPORTING_MODE_SPECIAL_TRIGGER
}


