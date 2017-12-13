package reandroidthings

import android.hardware.{Sensor, SensorEvent, SensorEventListener, SensorManager}
import com.google.android.things.contrib.driver.bmx280.Bmx280SensorDriver
import reandroidthings.iot_devices.BoardDefaults
import rescala._

import scala.reflect.ClassTag

/**
  * The SensorManager associated with this Sensor
  */

abstract class ReSensor[E](initialValue: E) {
  initialize()

  /**
    * The underlying Android peer
    */
  protected var peer: Sensor = null

  /**
    * The current value and accuracy (as ReScala Vars, Signals and Events)
    */
  protected val valueVar: Var[E] = Var(initialValue)
  protected val accuracyVar: Var[Int] = Var(Int.MinValue)

  def valueChanged(): Event[E] = {
    valueChangedEvt
  }

  def accuracyChanged(): Event[Int] = {
    accuracyChangedEvt
  }

  private val valueChangedEvt: Evt[E] = Evt[E]()
  private val accuracyChangedEvt: Evt[Int] = Evt[Int]()

  /**
    * initializes the Sensor Manager, conducts the pre and post initialize routine and assigns
    * the peer
    */
  private def initialize(): Unit = {
    val sensorManager: SensorManager = ReSensorManager.getSensorManager()
    preInitialize(sensorManager)
    initializePeer(sensorManager)
    postInitialize(sensorManager)
  }

  protected def preInitialize(sensorManager: SensorManager): Unit = {}

  protected def initializePeer(sensorManager: SensorManager): Unit = {
    peer = sensorManager.getDefaultSensor(sensorType)
  }

  protected def postInitialize(sensorManager: SensorManager): Unit = {
    sensorManager.registerListener(sensorListener, peer, SensorManager.SENSOR_DELAY_NORMAL)
  }

  /**
    * the sensorListener simply assigns any new value given by the SensorEvent
    * to the according rescala.Var
    */
  protected val sensorListener: SensorEventListener = new SensorEventListener {
    override def onSensorChanged(event: SensorEvent): Unit = {
      val e: E = parseSensorValues(event.values)
//      valueVar() = e
      valueChangedEvt(e)
    }

    override def onAccuracyChanged(sensor: Sensor, newAccuracy: Int): Unit = {
//      accuracyVar() = newAccuracy
      accuracyChangedEvt(newAccuracy)
    }
  }

  protected def parseSensorValues(values: Array[Float]): E

  protected def sensorType: Int

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

}

/**
  * Bmx280 sensor deals with the Bmx280 driver, that requires special treatment
  * (dynamic sensor callback)
  */
abstract class ReBmx280Sensor extends ReSensor[Float](Float.MinValue) {
  val bmx280Driver: Bmx280SensorDriver = new Bmx280SensorDriver(BoardDefaults.getI2cBus)

  override def initializePeer(sensorManager: SensorManager): Unit = {
    peer = sensorManager.getDefaultSensor(ReSensor.TypeDynamicSensorMeta)
  }

  override def postInitialize(sensorManager: SensorManager): Unit = {
    sensorManager.registerDynamicSensorCallback(new SensorManager.DynamicSensorCallback() {

      override def onDynamicSensorConnected(sensor: Sensor): Unit = {
        if (sensor.getType == sensorType) {
          sensorManager.registerListener(sensorListener, sensor, SensorManager.SENSOR_DELAY_NORMAL)
        }
      }

      override def onDynamicSensorDisconnected(sensor: Sensor): Unit = {
        if (sensor.getType == sensorType) {
          sensorManager.unregisterListener(sensorListener)
        }
      }
    })
  }

  override def parseSensorValues(values: Array[Float]): Float = values(0)
}

class RePressureSensor extends ReBmx280Sensor {
  override def preInitialize(sensorManager: SensorManager) = {
    bmx280Driver.registerPressureSensor
  }

  override def sensorType: Int = ReSensor.TypePressure
}

class ReTemperatureSensor extends ReBmx280Sensor {
  override def preInitialize(sensorManager: SensorManager) = {
    bmx280Driver.registerTemperatureSensor
  }

  override def sensorType: Int = ReSensor.TypeAmbientTemperature
}

class ReGyroscopeSensor extends ReSensor[(Float, Float, Float)]((0, 0, 0)) {

  override def sensorType: Int = ReSensor.TypeGyroscope

  override def parseSensorValues(values: Array[Float]): (Float, Float, Float) = (values(0), values(1), values(2))
}

case class ReSensorDescriptor[T](sensorType: Int) {
}

object ReSensor {
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
  val StringTypeSignificantMotion = Sensor.STRING_TYPE_SIGNIFICANT_MOTION

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
  val StringTypeDynamicSensorMeta = classOf[Sensor].getDeclaredField("STRING_TYPE_DYNAMIC_SENSOR_META").get(null).toString

  // special Sensor-types present in the RPi3 Rainbow-Head (a dynamic temperature and pressure sensor)
  val TypeDynamicSensorMetaPressure = /*ReSensorDescriptor[RePressureSensor](*/40/*)*/
  val TypeDynamicSensorMetaTemperature = /*ReSensorDescriptor[ReTemperatureSensor](*/41/*)*/

  val TypeDynamicSensorMetaPressureDescriptor = ReSensorDescriptor[Float](40)
  val TypeDynamicSensorMetaTemperatureDescriptor = ReSensorDescriptor[Float](41)

  val TypeAll = Sensor.TYPE_ALL
  val TypeDevicePrivateBase = Sensor.TYPE_DEVICE_PRIVATE_BASE
  val ReportingModeContinuous = Sensor.REPORTING_MODE_CONTINUOUS
  val ReportingModeOnChange = Sensor.REPORTING_MODE_ON_CHANGE
  val ReportingModeOneShot = Sensor.REPORTING_MODE_ONE_SHOT
  val ReportingModeSpecialTrigger = Sensor.REPORTING_MODE_SPECIAL_TRIGGER
}


