package reandroidthings

import scala.language.implicitConversions
import android.hardware.{Sensor, SensorEventListener, SensorManager}
import android.os.Handler
import android.util.Log
import rescala.graph.Pulse.Value
// needed for implicit conversation of Java->Scala List
import scala.collection.JavaConverters._


trait ReSensorManager {
  /**
    * The underlying Android peer
    */
  protected def peer: SensorManager

  def sensorList(`type`: Int): List[ReSensor] = {
    //    val l: List[Int] = List(1, 2, 3, 4)
    //    print(l.map(_ * 2))
    val l: List[Sensor] = peer.getSensorList(`type`).asScala.toList
    l.map(ReSensor.wrap(_))
  }

  def dynamicSensorList(`type`: Int): List[ReSensor] = {
    //    val l: List[Int] = List(1, 2, 3, 4)
    //    print(l.map(_ * 2))
    val l: List[Sensor] = peer.getDynamicSensorList(`type`).asScala.toList
    l.map(ReSensor.wrap(_))
  }

  def defaultSensor(`type`: Int): ReSensor = {
    val sensor = peer.getDefaultSensor(`type`)

    new ReSensor {
      def peer = sensor
    }
  }

  def defaultSensor(`type`: Int, wakeUp: Boolean): ReSensor = {
    val sensor = peer.getDefaultSensor(`type`, wakeUp)

    new ReSensor {
      def peer = sensor
    }
  }

  //  def defaultSensor(tyipe: Int, wakeUp: boolean): List[ReSensor] = peer.getDefaultSensor(tyipe, wakeUp)


  def registerDynamicSensorCallback(callback: ReSensorManager.DynamicSensorCallback): Unit = {
    // cast to SensorManager.DynamicSensorCallback
    val callbackSensorManager: SensorManager.DynamicSensorCallback = callback match {
      case cb: SensorManager.DynamicSensorCallback => cb
      case _ => null
    }

    peer.registerDynamicSensorCallback(callbackSensorManager, null)
  }

  def registerDynamicSensorCallback(callback: ReSensorManager.DynamicSensorCallback, handler: Handler): Unit = {
    peer.registerDynamicSensorCallback(callback, handler)
  }

  def registerListener(listener: ReSensorEventListener, sensor: ReSensor, samplingPeriodUs: Int): Unit = {
    registerListener(listener, sensor, samplingPeriodUs, null)
  }

  def registerListener(listener: ReSensorEventListener, sensor: ReSensor, samplingPeriodUs: Int, maxReportLatencyUs: Int): Boolean = {
    peer.registerListener(castToSensorEventListener(listener), sensor.peer, samplingPeriodUs, maxReportLatencyUs)
  }

  // TODO: Wrapper für Handler?
  def registerListener(listener: ReSensorEventListener, sensor: ReSensor, samplingPeriodUs: Int, handler: Handler): Unit = {
    peer.registerListener(castToSensorEventListener(listener), sensor.peer, samplingPeriodUs, handler)
  }

  def unregisterListener(listener: ReSensorEventListener): Unit = {
    unregisterListener(listener, null)
  }

  def unregisterListener(listener: ReSensorEventListener, sensor: ReSensor): Unit = {
    peer.unregisterListener(castToSensorEventListener(listener), sensor.peer)
  }

  def castToSensorEventListener(listener: ReSensorEventListener): SensorEventListener = listener match {
    case ls: SensorEventListener => ls
    case _ => null
  }
}


object ReSensorManager {

  /** Standard gravity (g) on Earth. This value is equivalent to 1G */
  final val StandardGravity = SensorManager.STANDARD_GRAVITY

  /** Sun's gravity in SI units (m/s^2) */
  final val GravitySun = SensorManager.GRAVITY_SUN

  /** Mercury's gravity in SI units (m/s^2) */
  final val GravityMercury = SensorManager.GRAVITY_MERCURY

  /** Venus' gravity in SI units (m/s^2) */
  final val GravityVenus = SensorManager.GRAVITY_VENUS

  /** Earth's gravity in SI units (m/s^2) */
  final val GravityEarth = SensorManager.GRAVITY_EARTH

  /** The Moon's gravity in SI units (m/s^2) */
  final val GravityMoon = SensorManager.GRAVITY_MOON

  /** Mars' gravity in SI units (m/s^2) */
  final val GravityMars = SensorManager.GRAVITY_MARS

  /** Jupiter's gravity in SI units (m/s^2) */
  final val GravityJupiter = SensorManager.GRAVITY_JUPITER

  /** Saturn's gravity in SI units (m/s^2) */
  final val GravitySaturn = SensorManager.GRAVITY_SATURN

  /** Uranus' gravity in SI units (m/s^2) */
  final val GravityUranus = SensorManager.GRAVITY_URANUS

  /** Neptune's gravity in SI units (m/s^2) */
  final val GravityNeptune = SensorManager.GRAVITY_NEPTUNE

  /** Pluto's gravity in SI units (m/s^2) */
  final val GravityPluto = SensorManager.GRAVITY_PLUTO

  /** Gravity (estimate) on the first Death Star in Empire units (m/s^2) */
  final val GravityDeathStarI = SensorManager.GRAVITY_DEATH_STAR_I

  /** Gravity on the island */
  final val GravityTheIsland = SensorManager.GRAVITY_THE_ISLAND

  /** Maximum magnetic field on Earth's surface */
  final val MagneticFieldEarthMax = SensorManager.MAGNETIC_FIELD_EARTH_MAX

  /** Minimum magnetic field on Earth's surface */
  final val MagneticFieldEarthMin = SensorManager.MAGNETIC_FIELD_EARTH_MIN

  /** Standard atmosphere, or average sea-level pressure in hPa (millibar) */
  final val PressureStandardAtmosphere = SensorManager.PRESSURE_STANDARD_ATMOSPHERE

  /** Maximum luminance of sunlight in lux */
  final val LightSunlightMax = SensorManager.LIGHT_SUNLIGHT_MAX

  /** luminance of sunlight in lux */
  final val LightSunlight = SensorManager.LIGHT_SUNLIGHT

  /** luminance in shade in lux */
  final val LightShade = SensorManager.LIGHT_SHADE

  /** luminance under an overcast sky in lux */
  final val LightOvercast = SensorManager.LIGHT_OVERCAST

  /** luminance at sunrise in lux */
  final val LightSunrise = SensorManager.LIGHT_SUNRISE

  /** luminance under a cloudy sky in lux */
  final val LightCloudy = SensorManager.LIGHT_CLOUDY

  /** luminance at night with full moon in lux */
  final val LightFullmoon = SensorManager.LIGHT_FULLMOON

  /** luminance at night with no moon in lux */
  final val LightNoMoon = SensorManager.LIGHT_NO_MOON

  /** get sensor data as fast as possible */
  final val SensorDelayFasttest = SensorManager.SENSOR_DELAY_FASTEST

  /** rate suitable for games */
  final val SensorDelayGame = SensorManager.SENSOR_DELAY_GAME

  /** rate suitable for the user interface  */
  final val SensorDelayUi = SensorManager.SENSOR_DELAY_UI

  /** rate (default) suitable for screen orientation changes */
  final val SensorDelayNormal = SensorManager.SENSOR_DELAY_NORMAL

  /**
    * The values returned by this sensor cannot be trusted because the sensor
    * had no contact with what it was measuring (for example, the heart rate
    * monitor is not in contact with the user).
    */
  final val SensorStatusNoContact = SensorManager.SENSOR_STATUS_NO_CONTACT

  /**
    * The values returned by this sensor cannot be trusted, calibration is
    * needed or the environment doesn't allow readings
    */
  final val SensorStatusUnreliable = SensorManager.SENSOR_STATUS_UNRELIABLE

  /**
    * This sensor is reporting data with low accuracy, calibration with the
    * environment is needed
    */
  final val SensorStatusAccuracyLow = SensorManager.SENSOR_STATUS_ACCURACY_LOW

  /**
    * This sensor is reporting data with an average level of accuracy,
    * calibration with the environment may improve the readings
    */
  final val SensorStatusAccuracyMedium = SensorManager.SENSOR_STATUS_ACCURACY_MEDIUM

  /** This sensor is reporting data with maximum accuracy */
  final val SensorStatusAccuracyHigh = SensorManager.SENSOR_STATUS_ACCURACY_HIGH

  /** see {@link #remapCoordinateSystem} */
  final val AxisX = SensorManager.AXIS_X

  /** see {@link #remapCoordinateSystem} */
  final val AxisY = SensorManager.AXIS_Y

  /** see {@link #remapCoordinateSystem} */
  final val AxisZ = SensorManager.AXIS_Z

  /** see {@link #remapCoordinateSystem} */
  final val AxisMinusX = SensorManager.AXIS_MINUS_X

  /** see {@link #remapCoordinateSystem} */
  final val AxisMinusY = SensorManager.AXIS_MINUS_Y

  /** see {@link #remapCoordinateSystem} */
  final val AxisMinusZ = SensorManager.AXIS_MINUS_Z

  implicit def toSensorManager(reSensorManager: ReSensorManager): SensorManager = reSensorManager.peer

  implicit def toSensorManagerCallback(callback: ReSensorManager.DynamicSensorCallback): SensorManager.DynamicSensorCallback = {
    callback match {
      case cb: SensorManager.DynamicSensorCallback => cb
      case _ => null
    }
  }


  def wrap(sensorManager: SensorManager): ReSensorManager = {
    new ReSensorManager {
      def peer = sensorManager
    }
  }

  abstract class DynamicSensorCallback extends android.hardware.SensorManager.DynamicSensorCallback {
    /**
      * Called when there is a dynamic sensor being connected to the system.
      *
      * @param sensor the newly connected sensor. See { @link android.hardware.Sensor Sensor}.
      */
    def onDynamicSensorConnected(sensor: ReSensor): Unit = {}

    /**
      * Called when there is a dynamic sensor being disconnected from the system.
      *
      * @param sensor the disconnected sensor. See { @link android.hardware.Sensor Sensor}.
      */
    def onDynamicSensorDisconnected(sensor: ReSensor): Unit = {}
  }
}
