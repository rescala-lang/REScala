package reandroidthings

import android.hardware.{Sensor, SensorEvent, SensorEventListener, SensorManager}
import android.content.Context

trait ReSensorManager {
  protected def peer: SensorManager
}

object ReSensorManager {

  private var context: Context = null

  def init(contextGiven: Context): Unit = {
    context = contextGiven
  }

  def getSensor[E](sensorDescriptor: ReSensorDescriptor[E]): ReSensor[E] = {
    val reSensor: ReSensor[_] = sensorDescriptor.sensorType match {
      case ReSensor.TypeDynamicSensorMetaPressure => new RePressureSensor()
      case ReSensor.TypeDynamicSensorMetaTemperature => new ReTemperatureSensor()
      case ReSensor.TypeGyroscope => new ReGyroscopeSensor()
      case _ => throw new RuntimeException("not implemented")
    }

    return reSensor.asInstanceOf[ReSensor[E]]
  }

  def getSensorManager(): SensorManager = {
    if (context == null) {
      throw new IllegalStateException("ReSensorManager has not yet been initialized, call 'ReSensorManager.init' first.")
    }

    // get sensorManager, when needed
    return context.getSystemService(Context.SENSOR_SERVICE).asInstanceOf[SensorManager]
  }

  def sensorList(descriptor: ReSensorDescriptor[_]): List[Int] = {
    val list: java.util.List[Sensor] = getSensorManager.getSensorList(descriptor.sensorType)

    convertToScala(list)
  }

  def dynamicSensorList(descriptor: ReSensorDescriptor[_]): List[Int] = {
    val dynSensors: java.util.List[Sensor] = getSensorManager.getDynamicSensorList(descriptor.sensorType)

    convertToScala(dynSensors)
  }

  private def convertToScala(javaList: java.util.List[Sensor]): scala.collection.immutable.List[Int] = {
    var l: scala.collection.immutable.List[Int] = List()
    for (i <- 0 to javaList.size()) {
      l = l :+ javaList.get(i).getType
    }
    return l
  }

  def removeSensors(): Unit = {
    // TODO: remove all SensorListeners
  }


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
}
