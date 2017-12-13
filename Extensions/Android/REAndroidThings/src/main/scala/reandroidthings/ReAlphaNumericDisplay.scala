package reandroidthings

import java.io.IOException
import android.util.Log

import com.google.android.things.contrib.driver.ht16k33.AlphanumericDisplay
import reandroidthings.iot_devices.BoardDefaults
import rescala._

object ReAlphaNumericDisplay {
  protected var peer: AlphanumericDisplay = null
  private val TAG: String = "ReAlphaNumericDisplay"
  private var eventObserver: Observe = null


  def init(inputEvent: Event[Any]): Unit = {
    try {
      peer = new AlphanumericDisplay(BoardDefaults.getI2cBus)
      peer.clear

      eventObserver = inputEvent += { newV => {
        newV match {
          case v: Double => peer.display(v)
          case v: Float => peer.display(v.toDouble)
          case v: String => peer.display(v)
          case v: Int => peer.display(v)
          case v => peer.display(v.toString)
        }
      }
      }
    } catch {
      case e: Exception => {
        Log.e(TAG, "Error initializing display", e)
        peer = null
      }
    }

    turnOn()
  }


  /**
    * turns the LEDs on and leaves the connection to the inputEvent untouched
    */
  def turnOn(): Unit = {
    if (peer != null) {
      peer.setEnabled(true)
    }
  }

  /**
    * turns off the LEDs, but leaves the connection to the inputEvent untouched
    */
  def turnOff(): Unit = {
    if (peer != null) {
      peer.setEnabled(false)
    }
  }


  /**
    * removes the inputEvent and turns off the display
    */
  def destroy(): Unit = {
    if (peer != null) try {
      peer.clear
      peer.setEnabled(false)

      eventObserver.remove
      peer.close
    } catch {
      case e: IOException =>
        Log.e(TAG, "Error disabling display", e)
    }
  }
}
