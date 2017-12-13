package reandroidthings.iot_devices

import android.os.Build
import com.google.android.things.pio.PeripheralManagerService
import java.util

// TODO: Do Copyright-stuff
/*
 * Copyright 2016 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


@SuppressWarnings(Array("WeakerAccess")) object BoardDefaults {
  private val DEVICE_EDISON_ARDUINO = "edison_arduino"
  private val DEVICE_EDISON = "edison"
  private val DEVICE_JOULE = "joule"
  private val DEVICE_RPI3 = "rpi3"
  private val DEVICE_IMX6UL_PICO = "imx6ul_pico"
  private val DEVICE_IMX6UL_VVDN = "imx6ul_iopb"
  private val DEVICE_IMX7D_PICO = "imx7d_pico"
  private var sBoardVariant = ""

  def getButtonGpioPin: String = getBoardVariant match {
    case DEVICE_EDISON_ARDUINO =>
      "IO12"
    case DEVICE_EDISON =>
      "GP44"
    case DEVICE_JOULE =>
      "J7_71"
    case DEVICE_RPI3 =>
      "BCM21"
    case DEVICE_IMX6UL_PICO =>
      "GPIO4_IO20"
    case DEVICE_IMX6UL_VVDN =>
      "GPIO3_IO01"
    case DEVICE_IMX7D_PICO =>
      "GPIO_174"
    case _ =>
      throw new IllegalArgumentException("Unknown device: " + Build.DEVICE)
  }

  def getLedGpioPin: String = getBoardVariant match {
    case DEVICE_EDISON_ARDUINO =>
      "IO13"
    case DEVICE_EDISON =>
      "GP45"
    case DEVICE_JOULE =>
      "J6_25"
    case DEVICE_RPI3 =>
      "BCM6"
    case DEVICE_IMX6UL_PICO =>
      "GPIO4_IO21"
    case DEVICE_IMX6UL_VVDN =>
      "GPIO3_IO06"
    case DEVICE_IMX7D_PICO =>
      "GPIO_34"
    case _ =>
      throw new IllegalArgumentException("Unknown device: " + Build.DEVICE)
  }

  def getI2cBus: String = getBoardVariant match {
    case DEVICE_EDISON_ARDUINO =>
      "I2C6"
    case DEVICE_EDISON =>
      "I2C1"
    case DEVICE_JOULE =>
      "I2C0"
    case DEVICE_RPI3 =>
      "I2C1"
    case DEVICE_IMX6UL_PICO =>
      "I2C2"
    case DEVICE_IMX6UL_VVDN =>
      "I2C4"
    case DEVICE_IMX7D_PICO =>
      "I2C1"
    case _ =>
      throw new IllegalArgumentException("Unknown device: " + Build.DEVICE)
  }

  def getSpiBus: String = getBoardVariant match {
    case DEVICE_EDISON_ARDUINO =>
      "SPI1"
    case DEVICE_EDISON =>
      "SPI2"
    case DEVICE_JOULE =>
      "SPI0.0"
    case DEVICE_RPI3 =>
      "SPI0.0"
    case DEVICE_IMX6UL_PICO =>
      "SPI3.0"
    case DEVICE_IMX6UL_VVDN =>
      "SPI1.0"
    case DEVICE_IMX7D_PICO =>
      "SPI3.1"
    case _ =>
      throw new IllegalArgumentException("Unknown device: " + Build.DEVICE)
  }

  def getSpeakerPwmPin: String = getBoardVariant match {
    case DEVICE_EDISON_ARDUINO =>
      "IO3"
    case DEVICE_EDISON =>
      "GP13"
    case DEVICE_JOULE =>
      "PWM_0"
    case DEVICE_RPI3 =>
      "PWM1"
    case DEVICE_IMX6UL_PICO =>
      "PWM7"
    case DEVICE_IMX6UL_VVDN =>
      "PWM3"
    case DEVICE_IMX7D_PICO =>
      "PWM2"
    case _ =>
      throw new IllegalArgumentException("Unknown device: " + Build.DEVICE)
  }

  private def getBoardVariant: String = {
    if (!sBoardVariant.isEmpty) return sBoardVariant
    sBoardVariant = Build.DEVICE
    // For the edison check the pin prefix
    // to always return Edison Breakout pin name when applicable.
    // TODO: sometimes breaks unpreparedly, have a look at why
//    if (sBoardVariant == DEVICE_EDISON) {
//      val pioService: PeripheralManagerService = new PeripheralManagerService
//      val gpioList = pioService.getGpioList
//      if (gpioList.size != 0) {
//        val pin = gpioList.get(0)
//        if (pin.startsWith("IO")) sBoardVariant = DEVICE_EDISON_ARDUINO
//      }
//    }
    sBoardVariant
  }
}
