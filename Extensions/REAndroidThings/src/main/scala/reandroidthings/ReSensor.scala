package reandroidthings

import android.hardware.Sensor

abstract class ReSensor {

  /**
    * The underlying Android peer
    */
  def peer: Sensor

  def self = peer

  /**
    * its methods
    */
  // getter
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

  //  def toString: String = peer.toString

  /* marked as hidden:
  def handle: int = peer.getHandle
  def requiredPermission: String = peer.getRequiredPermission
  */
}


object ReSensor {

  val TYPE_ACCELEROMETER = Sensor.TYPE_ACCELEROMETER
  val STRING_TYPE_ACCELEROMETER = Sensor.STRING_TYPE_ACCELEROMETER

  val TYPE_MAGNETIC_FIELD = Sensor.TYPE_MAGNETIC_FIELD
  val STRING_TYPE_MAGNETIC_FIELD = Sensor.STRING_TYPE_MAGNETIC_FIELD

  val TYPE_MAGNETIC_FIELD_UNCALIBRATED = Sensor.TYPE_MAGNETIC_FIELD_UNCALIBRATED
  val STRING_TYPE_MAGNETIC_FIELD_UNCALIBRATED = Sensor.STRING_TYPE_MAGNETIC_FIELD_UNCALIBRATED

  val TYPE_GYROSCOPE = Sensor.TYPE_GYROSCOPE
  val STRING_TYPE_GYROSCOPE = Sensor.STRING_TYPE_GYROSCOPE

  val TYPE_LIGHT = Sensor.TYPE_LIGHT
  val STRING_TYPE_LIGHT = Sensor.STRING_TYPE_LIGHT

  val TYPE_PRESSURE = Sensor.TYPE_PRESSURE
  val STRING_TYPE_PRESSURE = Sensor.STRING_TYPE_PRESSURE

  val TYPE_PROXIMITY = Sensor.TYPE_PROXIMITY
  val STRING_TYPE_PROXIMITY = Sensor.STRING_TYPE_PROXIMITY

  val TYPE_GRAVITY = Sensor.TYPE_GRAVITY
  val STRING_TYPE_GRAVITY = Sensor.STRING_TYPE_GRAVITY

  val TYPE_LINEAR_ACCELERATION = Sensor.TYPE_LINEAR_ACCELERATION
  val STRING_TYPE_LINEAR_ACCELERATION = Sensor.STRING_TYPE_LINEAR_ACCELERATION

  val TYPE_ROTATION_VECTOR = Sensor.TYPE_ROTATION_VECTOR
  val STRING_TYPE_ROTATION_VECTOR = Sensor.STRING_TYPE_ROTATION_VECTOR

  val TYPE_GAME_ROTATION_VECTOR = Sensor.TYPE_GAME_ROTATION_VECTOR
  val STRING_TYPE_GAME_ROTATION_VECTOR = Sensor.STRING_TYPE_GAME_ROTATION_VECTOR

  val TYPE_RELATIVE_HUMIDITY = Sensor.TYPE_RELATIVE_HUMIDITY
  val STRING_TYPE_RELATIVE_HUMIDITY = Sensor.STRING_TYPE_RELATIVE_HUMIDITY

  val TYPE_AMBIENT_TEMPERATURE = Sensor.TYPE_AMBIENT_TEMPERATURE
  val STRING_TYPE_AMBIENT_TEMPERATURE = Sensor.STRING_TYPE_AMBIENT_TEMPERATURE

  val TYPE_GYROSCOPE_UNCALIBRATED = Sensor.TYPE_GYROSCOPE_UNCALIBRATED
  val STRING_TYPE_GYROSCOPE_UNCALIBRATED = Sensor.STRING_TYPE_GYROSCOPE_UNCALIBRATED

  val TYPE_SIGNIFICANT_MOTION = Sensor.TYPE_SIGNIFICANT_MOTION
  val STRING_TYPE_SIGNIFICANT_MOTION = Sensor.STRING_TYPE_SIGNIFICANT_MOTION

  val TYPE_STEP_DETECTOR = Sensor.TYPE_STEP_DETECTOR
  val STRING_TYPE_STEP_DETECTOR = Sensor.STRING_TYPE_STEP_DETECTOR

  val TYPE_STEP_COUNTER = Sensor.TYPE_STEP_COUNTER
  val STRING_TYPE_STEP_COUNTER = Sensor.STRING_TYPE_STEP_COUNTER

  val TYPE_GEOMAGNETIC_ROTATION_VECTOR = Sensor.TYPE_GEOMAGNETIC_ROTATION_VECTOR
  val STRING_TYPE_GEOMAGNETIC_ROTATION_VECTOR = Sensor.STRING_TYPE_GEOMAGNETIC_ROTATION_VECTOR

  val TYPE_HEART_RATE = Sensor.TYPE_HEART_RATE
  val STRING_TYPE_HEART_RATE = Sensor.STRING_TYPE_HEART_RATE

  val TYPE_POSE_6DOF = Sensor.TYPE_POSE_6DOF
  val STRING_TYPE_POSE_6DOF = Sensor.STRING_TYPE_POSE_6DOF

  val TYPE_STATIONARY_DETECT = Sensor.TYPE_STATIONARY_DETECT
  val STRING_TYPE_STATIONARY_DETECT = Sensor.STRING_TYPE_STATIONARY_DETECT

  val TYPE_MOTION_DETECT = Sensor.TYPE_MOTION_DETECT
  val STRING_TYPE_MOTION_DETECT = Sensor.STRING_TYPE_MOTION_DETECT

  val TYPE_HEART_BEAT = Sensor.TYPE_HEART_BEAT
  val STRING_TYPE_HEART_BEAT = Sensor.STRING_TYPE_HEART_BEAT

  val TYPE_ALL = Sensor.TYPE_ALL
  val TYPE_DEVICE_PRIVATE_BASE = Sensor.TYPE_DEVICE_PRIVATE_BASE
  val REPORTING_MODE_CONTINUOUS = Sensor.REPORTING_MODE_CONTINUOUS
  val REPORTING_MODE_ON_CHANGE = Sensor.REPORTING_MODE_ON_CHANGE
  val REPORTING_MODE_ONE_SHOT = Sensor.REPORTING_MODE_ONE_SHOT
  val REPORTING_MODE_SPECIAL_TRIGGER = Sensor.REPORTING_MODE_SPECIAL_TRIGGER


  def wrap(sensor: Sensor): ReSensor = {
    new ReSensor {
      def peer = sensor
    }
  }
}


