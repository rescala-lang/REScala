package reandroidthings.driver.bmx280

import java.io.IOException

/**
  * Created by volkc on 16.05.2017.
  */
@throws(classOf[IOException])
class Bmx280SensorDriver private(val peer: com.google.android.things.contrib.driver.bmx280.Bmx280SensorDriver) {

  /**
    * Create a new framework sensor driver connected on the given bus.
    * The driver emits {@link android.hardware.Sensor} with pressure and temperature data when
    * registered.
    *
    * @param bus I2C bus the sensor is connected to.
    * @throws java.io.IOException
    * @see registerPressureSensor
    * @see registerTemperatureSensor
    */
  @throws(classOf[IOException])
  def this(bus: String) {
    this(new com.google.android.things.contrib.driver.bmx280.Bmx280SensorDriver(bus))
  }

  /**
    * Close the driver and the underlying device.
    *
    * @throws java.io.IOException
    */
  @throws(classOf[IOException])
  def close(): Unit = peer.close()

  /**
    * Register a UserSensor that pipes temperature readings into the Android SensorManager.
    *
    * @see unregisterTemperatureSensor
    */
  def registerTemperatureSensor(): Unit = peer.registerTemperatureSensor()

  /**
    * Register a UserSensor that pipes pressure readings into the Android SensorManager.
    *
    * @see unregisterPressureSensor
    */
  def registerPressureSensor(): Unit = peer.registerPressureSensor()

  /**
    * Unregister the temperature UserSensor.
    */
  def unregisterTemperatureSensor(): Unit = peer.unregisterTemperatureSensor()

  /**
    * Unregister the pressure UserSensor.
    */
  def unregisterPressureSensor(): Unit = peer.unregisterPressureSensor()
}

object Bmx280SensorDriver {
}
