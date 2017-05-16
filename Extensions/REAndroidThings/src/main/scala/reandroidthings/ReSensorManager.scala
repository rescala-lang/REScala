package reandroidthings

import scala.language.implicitConversions
import android.hardware.SensorManager
import android.hardware.Sensor
import rescala.graph.Pulse.Value
// needed for implicit conversation of Java->Scala List
import scala.collection.JavaConverters._

abstract class ReSensorManager {
  /**
    * The underlying Android peer
    */
  protected def peer: SensorManager

  def self = peer

  def sensorList(`type`: Int): List[ReSensor] = {
    //    val l: List[Int] = List(1, 2, 3, 4)
    //    print(l.map(_ * 2))
    var l: List[Sensor] = peer.getSensorList(`type`).asScala.toList
    l.map(ReSensor.wrap(_))
  }

  //  def dynamicSensorList(tyipe: Int): List[ReSensor] = peer.getDynamicSensorList(tyipe)
  //
  //  def defaultSensor(tyipe: Int): List[ReSensor] = peer.getDefaultSensor(tyipe)
  //
  //  def defaultSensor(tyipe: Int, wakeUp: boolean): List[ReSensor] = peer.getDefaultSensor(tyipe, wakeUp)


}


object ReSensorManager {

  /** Standard gravity (g) on Earth. This value is equivalent to 1G */
  final val StandardGravity = Value(SensorManager.STANDARD_GRAVITY)

  /** Sun's gravity in SI units (m/s^2) */
  final val GravitySun = Value(SensorManager.GRAVITY_SUN)

  /** Mercury's gravity in SI units (m/s^2) */
  final val GravityMercury = Value(SensorManager.GRAVITY_MERCURY)

  /** Venus' gravity in SI units (m/s^2) */
  final val GravityVenus = Value(SensorManager.GRAVITY_VENUS)

  /** Earth's gravity in SI units (m/s^2) */
  final val GravityEarth = Value(SensorManager.GRAVITY_EARTH)

  /** The Moon's gravity in SI units (m/s^2) */
  final val GravityMoon = Value(SensorManager.GRAVITY_MOON)

  /** Mars' gravity in SI units (m/s^2) */
  final val GravityMars = Value(SensorManager.GRAVITY_MARS)

  /** Jupiter's gravity in SI units (m/s^2) */
  final val GravityJupiter = Value(SensorManager.GRAVITY_JUPITER)

  /** Saturn's gravity in SI units (m/s^2) */
  final val GravitySaturn = Value(SensorManager.GRAVITY_SATURN)

  /** Uranus' gravity in SI units (m/s^2) */
  final val GravityUranus = Value(SensorManager.GRAVITY_URANUS)

  /** Neptune's gravity in SI units (m/s^2) */
  final val GravityNeptune = Value(SensorManager.GRAVITY_NEPTUNE)

  /** Pluto's gravity in SI units (m/s^2) */
  final val GravityPluto = Value(SensorManager.GRAVITY_PLUTO)

  /** Gravity (estimate) on the first Death Star in Empire units (m/s^2) */
  final val GravityDeathStarI = Value(SensorManager.GRAVITY_DEATH_STAR_I)

  /** Gravity on the island */
  final val GravityTheIsland = Value(SensorManager.GRAVITY_THE_ISLAND)

  /** Maximum magnetic field on Earth's surface */
  final val MagneticFieldEarthMax = Value(SensorManager.MAGNETIC_FIELD_EARTH_MAX)

  /** Minimum magnetic field on Earth's surface */
  final val MagneticFieldEarthMin = Value(SensorManager.MAGNETIC_FIELD_EARTH_MIN)

  /** Standard atmosphere, or average sea-level pressure in hPa (millibar) */
  final val PressureStandardAtmosphere = Value(SensorManager.PRESSURE_STANDARD_ATMOSPHERE)

  /** Maximum luminance of sunlight in lux */
  final val LightSunlightMax = Value(SensorManager.LIGHT_SUNLIGHT_MAX)

  /** luminance of sunlight in lux */
  final val LightSunlight = Value(SensorManager.LIGHT_SUNLIGHT)

  /** luminance in shade in lux */
  final val LightShade = Value(SensorManager.LIGHT_SHADE)

  /** luminance under an overcast sky in lux */
  final val LightOvercast = Value(SensorManager.LIGHT_OVERCAST)

  /** luminance at sunrise in lux */
  final val LightSunrise = Value(SensorManager.LIGHT_SUNRISE)

  /** luminance under a cloudy sky in lux */
  final val LightCloudy = Value(SensorManager.LIGHT_CLOUDY)

  /** luminance at night with full moon in lux */
  final val LightFullmoon = Value(SensorManager.LIGHT_FULLMOON)

  /** luminance at night with no moon in lux */
  final val LightNoMoon = Value(SensorManager.LIGHT_NO_MOON)

  /** get sensor data as fast as possible */
  final val SensorDelayFasttest = Value(SensorManager.SENSOR_DELAY_FASTEST)

  /** rate suitable for games */
  final val SensorDelayGame = Value(SensorManager.SENSOR_DELAY_GAME)

  /** rate suitable for the user interface  */
  final val SensorDelayUi = Value(SensorManager.SENSOR_DELAY_UI)

  /** rate (default) suitable for screen orientation changes */
  final val SensorDelayNormal = Value(SensorManager.SENSOR_DELAY_NORMAL)

  /**
    * The values returned by this sensor cannot be trusted because the sensor
    * had no contact with what it was measuring (for example, the heart rate
    * monitor is not in contact with the user).
    */
  final val SensorStatusNoContact = Value(SensorManager.SENSOR_STATUS_NO_CONTACT)

  /**
    * The values returned by this sensor cannot be trusted, calibration is
    * needed or the environment doesn't allow readings
    */
  final val SensorStatusUnreliable = Value(SensorManager.SENSOR_STATUS_UNRELIABLE)

  /**
    * This sensor is reporting data with low accuracy, calibration with the
    * environment is needed
    */
  final val SensorStatusAccuracyLow = Value(SensorManager.SENSOR_STATUS_ACCURACY_LOW)

  /**
    * This sensor is reporting data with an average level of accuracy,
    * calibration with the environment may improve the readings
    */
  final val SensorStatusAccuracyMedium = Value(SensorManager.SENSOR_STATUS_ACCURACY_MEDIUM)

  /** This sensor is reporting data with maximum accuracy */
  final val SensorStatusAccuracyHigh = Value(SensorManager.SENSOR_STATUS_ACCURACY_HIGH)

  /** see {@link #remapCoordinateSystem} */
  final val AxisX = Value(SensorManager.AXIS_X)

  /** see {@link #remapCoordinateSystem} */
  final val AxisY = Value(SensorManager.AXIS_Y)

  /** see {@link #remapCoordinateSystem} */
  final val AxisZ = Value(SensorManager.AXIS_Z)

  /** see {@link #remapCoordinateSystem} */
  final val AxisMinusX = Value(SensorManager.AXIS_MINUS_X)

  /** see {@link #remapCoordinateSystem} */
  final val AxisMinusY = Value(SensorManager.AXIS_MINUS_Y)

  /** see {@link #remapCoordinateSystem} */
  final val AxisMinusZ = Value(SensorManager.AXIS_MINUS_Z)

  implicit def toSensorManager(reSensorManager: ReSensorManager): SensorManager = reSensorManager.peer
}
