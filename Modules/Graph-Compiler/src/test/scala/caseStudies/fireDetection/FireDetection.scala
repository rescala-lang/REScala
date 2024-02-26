package caseStudies.fireDetection

import api2.*
import api2.given
import reactives.default.*

import scala.io.StdIn.readLine

object FireDetection extends App {
  val rotaryEncRemote = CompileGraph.withOutput("rotaryEnc") {
    val rawRotaryEncoder = CEvent.source[Int]
    val selectionInt     = rawRotaryEncoder.map(_ % 3)
    Tuple1(selectionInt)
  }

  val rotaryEncTCPClient = new TCPClientConnector("localhost", 8000).connect(rotaryEncRemote)

  val Tuple1(selectionInt) = rotaryEncRemote.eventsFromListen()
  val selectionSig         = selectionInt.hold(0)

  val buttonRemote = CompileGraph.withOutput("button") {
    val rawButton = CEvent.source[Boolean]
    val buttonFilter = rawButton.fold((false, false)) {
      case ((oldState, lastInput), input) =>
        if input == lastInput then (input, input) else (oldState, input)
    }
    val buttonFiltered   = buttonFilter.map(_._1)
    val buttonSingleEmit = buttonFiltered.changedTo(true)
    Tuple1(buttonSingleEmit)
  }

  val buttonTCPClient = new TCPClientConnector("localhost", 8001).connect(buttonRemote)

  val Tuple1(buttonSingleEmit) = buttonRemote.eventsFromListen()
  val toggleSelection          = buttonSingleEmit.map(_ => selectionSig.value)

  val co2Remote = CompileGraph.withIO("co2")(Tuple1(toggleSelection)) { case Tuple1(toggleSelection) =>
    val enabled = toggleSelection.fold(true) {
      case (state, 0) => !state
      case (state, _) => state
    }
    val rawCo2      = CEvent.source[Int]
    val co2Filtered = rawCo2.map(_ > 3000)
    val co2Bool = CEvent {
      if enabled.value then co2Filtered.value else Some(false)
    }
    Tuple1(co2Bool)
  }

  val co2TCPClient = new TCPClientConnector("localhost", 8002).connect(co2Remote)
  co2Remote.startObserving()

  val Tuple1(co2Bool) = co2Remote.eventsFromListen()
  val co2Status       = co2Bool.hold(false)

  val temperatureRemote = CompileGraph.withIO("temperature")(Tuple1(toggleSelection)) { case Tuple1(toggleSelection) =>
    val enabled = toggleSelection.fold(true) {
      case (state, 1) => !state
      case (state, _) => state
    }
    val rawTemperature      = CEvent.source[Int]
    val temperatureFiltered = rawTemperature.map(_ > 45)
    val temperatureBool = CEvent {
      if enabled.value then temperatureFiltered.value else Some(false)
    }
    Tuple1(temperatureBool)
  }

  val temperatureTCPClient = new TCPClientConnector("localhost", 8003).connect(temperatureRemote)
  temperatureRemote.startObserving()

  val Tuple1(temperatureBool) = temperatureRemote.eventsFromListen()
  val temperatureStatus       = temperatureBool.hold(false)

  val photoelecRemote = CompileGraph.withIO("photoelectric")(Tuple1(toggleSelection)) { case Tuple1(toggleSelection) =>
    val enabled = toggleSelection.fold(true) {
      case (state, 2) => !state
      case (state, _) => state
    }
    val rawPhotoelec = CEvent.source[Boolean]
    val photoelecBool = CEvent {
      if enabled.value then rawPhotoelec.value else Some(false)
    }
    Tuple1(photoelecBool)
  }

  val photoelecTCPClient = new TCPClientConnector("localhost", 8004).connect(photoelecRemote)
  photoelecRemote.startObserving()

  val Tuple1(photoelecBool) = photoelecRemote.eventsFromListen()
  val photoelecStatus       = photoelecBool.hold(false)

  val fireDetected = Signal {
    co2Status.value || temperatureStatus.value || photoelecStatus.value
  }

  fireDetected.observe(println)

  readLine()

  rotaryEncTCPClient.closeConnection()
  buttonTCPClient.closeConnection()
  co2TCPClient.closeConnection()
  temperatureTCPClient.closeConnection()
  photoelecTCPClient.closeConnection()
}
