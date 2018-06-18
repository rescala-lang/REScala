package tests.rescala.fullmv.transmitter

import java.util.concurrent.atomic.AtomicInteger

object TransmitterTestsPortManagement extends AtomicInteger(1099) {
  def getFreePort(): Int = getAndIncrement()
}
