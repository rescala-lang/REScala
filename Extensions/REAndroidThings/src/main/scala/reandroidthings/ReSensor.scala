package reswing

import scala.language.implicitConversions
import android.hardware.Sensor

abstract class ReSensor {

  /**
    * The underlying Android peer
    */
  protected def peer: Sensor
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

  def wakeUpSensor: Boolean = peer.isWakeUpSensor
  def dynamicSensor: Boolean = peer.isDynamicSensor
  def additionalInfoSupported: Boolean = peer.isAdditionalInfoSupported

//  def toString: String = peer.toString

  /* marked as hidden:
  def handle: int = peer.getHandle
  def requiredPermission: String = peer.getRequiredPermission
  */
}


object ReSensor {

  def wrap(sensor:Sensor): ReSensor = {
    new ReSensor {
      def peer = sensor
    }
  }
}


