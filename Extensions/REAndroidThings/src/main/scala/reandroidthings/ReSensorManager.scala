package reswing

import scala.language.implicitConversions
import android.hardware.SensorManager
import android.hardware.Sensor
// needed for implicit conversation of Java->Scala List
import scala.collection.JavaConverters._

abstract class ReSensorManager {

  /**
    * The underlying Android peer
    */
  protected def peer: SensorManager

  def self = peer

  // TODO: find better name, since 'type' is a keyword in Scala, sth. like clazz in Java
  def sensorList(tyipe: Int): List[ReSensor] = {
    //    val l: List[Int] = List(1, 2, 3, 4)
    //    print(l.map(_ * 2))
    var l: List[Sensor] = peer.getSensorList(tyipe).asScala.toList
    l.map(ReSensor.wrap(_))
  }

  //  def dynamicSensorList(tyipe: Int): List[ReSensor] = peer.getDynamicSensorList(tyipe)
  //
  //  def defaultSensor(tyipe: Int): List[ReSensor] = peer.getDefaultSensor(tyipe)
  //
  //  def defaultSensor(tyipe: Int, wakeUp: boolean): List[ReSensor] = peer.getDefaultSensor(tyipe, wakeUp)


}


object ReSensorManager {
  implicit def toSensorManager(reSensorManager: ReSensorManager): SensorManager = reSensorManager.peer
}
